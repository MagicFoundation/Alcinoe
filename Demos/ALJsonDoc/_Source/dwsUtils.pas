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
unit dwsUtils;

{$I dws.inc}
{$R-}
{$Q-}
{$WARN DUPLICATE_CTOR_DTOR OFF}

{.$define DOUBLE_FREE_PROTECTOR}

interface

uses
   System.Classes, System.SysUtils, System.Types, System.Variants,
   dwsXPlatform;

type

   TInt64DynArray = array of Int64;
   TUInt64DynArray = array of UInt64;
   TSingleDynArray = array of Single;
   TDoubleDynArray = array of Double;
   TInterfaceDynArray = array of IUnknown;
   TVariantDynArray = array of Variant;

   TInt64Array = array [0..High(MaxInt) shr 4] of Int64;
   PInt64Array = ^TInt64Array;

   TInt32Array = array [0..High(MaxInt) shr 3] of Int32;
   PInt32Array = ^TInt32Array;
   TUInt32Array = array [0..High(MaxInt) shr 3] of UInt32;
   PUInt32Array = ^TUInt32Array;

   TInt16Array = array [0..High(MaxInt) shr 2] of Int16;
   PInt16Array = ^TInt16Array;
   TUInt16Array = array [0..High(MaxInt) shr 2] of UInt16;
   PUInt16Array = ^TUInt16Array;

   TInt8Array = array [0..High(MaxInt) shr 1] of Int8;
   PInt8Array = ^TInt8Array;
   TUInt8Array = array [0..High(MaxInt) shr 1] of UInt8;
   PUInt8Array = ^TUInt8Array;

   TSingleArray = array [0..High(MaxInt) shr 4] of Single;
   PSingleArray = ^TSingleArray;
   TDoubleArray = array [0..High(MaxInt) shr 4] of Double;
   PDoubleArray = ^TDoubleArray;

   TStaticStringArray = array [0..High(MaxInt) shr 4] of String;
   PStringArray = ^TStaticStringArray;

   TStaticVariantArray = array [0..High(MaxInt) shr 5] of Variant;
   PVariantArray = ^TStaticVariantArray;

   TInt64DynArrayHelper = record helper for TInt64DynArray
      function High : Integer; inline;
      function Length : Integer; inline;
   end;

   // TRefCountedObject
   //
   // Uses Monitor hidden field to store refcount, so not compatible with monitor use
   // (but Monitor is buggy, so no great loss)
   {$ifndef FPC}
      {$ifdef WIN32}
         {$define USE_MONITOR_FOR_REFCOUNT}
      {$endif}
   {$endif}
   TRefCountedObject = class
      private
         {$ifndef USE_MONITOR_FOR_REFCOUNT}
         FRefCount : Integer;
         {$endif}
         {$ifdef DOUBLE_FREE_PROTECTOR}
         FDoubleFreeProtector : Int64;
         {$endif}
         function  GetRefCount : Integer; inline;
         procedure SetRefCount(n : Integer); inline;
      public
         {$ifdef DOUBLE_FREE_PROTECTOR}
         constructor Create;
         destructor Destroy; override;
         {$endif}
         function  IncRefCount : Integer; inline;
         function  DecRefCount : Integer;
         property  RefCount : Integer read GetRefCount write SetRefCount;
         procedure Free;
   end;
   PRefCountedObject = ^TRefCountedObject;

   // IGetSelf
   //
   IGetSelf = interface
      ['{77D8EA0B-311C-422B-B8DE-AA5BDE726E41}']
      function GetSelf : TObject;
      function ToString : String;
      function ScriptTypeName : String;
   end;

   // TInterfacedSelfObject
   //
   TInterfacedSelfObject = class (TRefCountedObject, IUnknown, IGetSelf)
      protected
         function GetSelf : TObject;
         function QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} IID: TGUID; out Obj): HResult; stdcall;
         function _AddRef : Integer; stdcall;
         function _Release : Integer; stdcall;

      public
         class function NewInstance: TObject; override;
         procedure AfterConstruction; override;
         procedure BeforeDestruction; override;
         function ScriptTypeName : String; virtual;
   end;

   // IAutoStrings
   //
   IAutoStrings = interface
      function GetValue : TStringList;
      function GetItem(index : Integer) : String;
      procedure SetItem(index : Integer; const value : String);
      function GetValues(const name : String) : String;
      procedure SetValues(const name, value : String);

      function Clone : IAutoStrings;

      property Value : TStringList read GetValue;

      property Items[index : Integer] : String read GetItem write SetItem; default;
      function Count : Integer;

      property Values[const name : String] : String read GetValues write SetValues;
      function IndexOfName(const name : String) : Integer;
   end;

   // TAutoStrings
   //
   TAutoStrings = class(TInterfacedObject, IAutoStrings)
      private
         FValue : TStringList;
      protected
         function GetValue : TStringList;
         function Clone : IAutoStrings;
         function GetItem(index : Integer) : String;
         procedure SetItem(index : Integer; const value : String);
         function Count : Integer;
         function IndexOfName(const name : String) : Integer;
         function GetValues(const name : String) : String;
         procedure SetValues(const name, value : String);
      public
         constructor Create;
         constructor CreateCapture(value : TStringList);
         constructor CreateClone(value : TStringList);
         destructor Destroy; override;
   end;

   // Interface for coalesce-able IUnknown
   ICoalesceable = interface
      ['{9F074F2A-2AAC-48E7-851B-FFA2CE3742F4}']
      function IsFalsey : Boolean;
   end;

   // Interface for null-able IUnknown
   INullable = interface
      ['{65BF25C1-837C-4C98-AF71-C8A6A028F678}']
      function IsNull : Boolean;
      function IsDefined : Boolean;
   end;

   IToNumeric = interface
      ['{11C48916-A0E4-4D4E-B534-89020D9842A7}']
      function ToFloat : Double;
      function ToInteger : Int64;
   end;

   IToVariant = interface
      ['{DE1A280D-5552-4F84-B557-957271D6EA62}']
      procedure ToVariant(var result : Variant);
      function IsArray : Boolean;
      function IsNumeric : Boolean;
      function IsString : Boolean;
   end;

   // TVarRecArrayContainer
   //
   TVarRecArrayContainer = class
      private
         FIntegers : array of Int64;
         FFloats : array of Extended;
         FStrings : array of String;

         function AddVarRec : PVarRec;

      public
         VarRecArray : array of TVarRec;

         procedure Add(const v : Variant);
         procedure AddBoolean(const b : Boolean);
         procedure AddInteger(const i : Int64);
         procedure AddFloat(const f : Double);
         procedure AddString(const s : String);

         procedure Initialize;
   end;

   // TTightList
   //
   {: Compact list embedded in a record.<p>
      If the list holds only 1 item, no dynamic memory is allocated
      (the list pointer is used).
      Make sure to Clear or Clean in the destructor of the Owner. }
   TTightListArray = array [0..MaxInt shr 4] of TRefCountedObject;
   PObjectTightList = ^TTightListArray;
   TPointerComparer = function (a, b : Pointer) : Integer;
   TTightList = record
      private
         FList : PObjectTightList;

         procedure RaiseIndexOutOfBounds;

         type TTightListEnumerator = record
            FIter : PPointer;
            FTail : PPointer;
            function MoveNext : Boolean; inline;
            function GetCurrent : TRefCountedObject; inline;
            property Current : TRefCountedObject read GetCurrent;
         end;

      public
         FCount : Integer;     // exposed so it can be used for direct property access

         function List : PObjectTightList; inline;
         property Count : Integer read FCount;

         procedure Initialize; // in case of use as a local variable
         procedure Free; inline; // to posture as a regular TList
         procedure Clean;  // clear the list and free the item objects
         procedure Clear;  // clear the list without freeing the items
         function Add(item : TRefCountedObject) : Integer;
         procedure Assign(const aList : TTightList);
         function IndexOf(item : TRefCountedObject) : Integer;
         function Remove(item : TRefCountedObject) : Integer;
         procedure Delete(index : Integer);
         procedure Insert(index : Integer; item : TRefCountedObject);
         procedure MoveItem(curIndex, newIndex : Integer); // Note: D2009 fails if this method is called Move (!) - HV
         procedure Exchange(index1, index2 : Integer);
         function ItemsAllOfClass(aClass : TClass) : Boolean;

         function GetEnumerator : TTightListEnumerator;

         procedure Sort(const comparer : TPointerComparer);
   end;
   PTightList = ^TTightList;

   // TTightStack
   //
   {: Embeddable stack functionality }
   TTightStack = record
      private
         FList : PObjectTightList;
         FCount : Integer;
         FCapacity : Integer;

         procedure Grow;

      public
         procedure Push(item : TRefCountedObject); inline;
         function  Peek : TRefCountedObject; overload; inline;
         function  Peek(n : Integer) : TRefCountedObject; overload;
         procedure Pop; inline;
         function  PeekAndPop : TRefCountedObject; inline;

         procedure Clear; inline;
         procedure Clean;
         procedure Free;

         property List : PObjectTightList read FList;
         property Count : Integer read FCount;
   end;

   TSimpleCallbackStatus = (csContinue, csAbort);

   TSimpleCallback<T> = function (var item : T) : TSimpleCallbackStatus;

   // TSimpleList<T>
   //
   {: A minimalistic generic list class. }
   TSimpleList<T> = class
      private
         type
            ArrayT = array of T;
         var
            FItems : ArrayT;
            FCount : Integer;
            FCapacity : Integer;

      protected
         procedure Grow;
         function GetItems(const idx : Integer) : T; {$IFDEF DELPHI_2010_MINUS}{$ELSE} inline; {$ENDIF}
         procedure SetItems(const idx : Integer; const value : T);

      public
         procedure Add(const item : T);
         procedure Extract(idx : Integer);
         procedure Clear;
         procedure Enumerate(const callback : TSimpleCallback<T>);
         property Items[const position : Integer] : T read GetItems write SetItems; default;
         property Count : Integer read FCount;

         type PItemPtr = ^T;
         function ItemPtr(idx : Integer) : PItemPtr; inline;
   end;

   // TSimpleStringList
   //
   // this list is really tailored for the needs of names lists by the compiler
   // so essentially short-lived short lists, holding limited data
   TSimpleStringList = class
      private
         FItems : TStringDynArray;
         FCapacity : Integer;
         FCount : Integer;

      protected
         function GetItems(i : Integer) : String; inline;
         procedure SetItems(i : Integer; const s : String); inline;

      public
         procedure Add(const s : String);
         function IndexOf(const s : String) : Integer;
         procedure Clear;

         property Items[i : Integer] : String read GetItems write SetItems; default;
         property Count : Integer read FCount;
   end;

   // TSimpleStringListPool
   //
   TSimpleStringListPool = record
      private
         FPool : array [0..7] of TSimpleStringList;
         FCount : Integer;

      public
         procedure Initialize; inline;
         procedure Finalize; inline;

         function  Acquire : TSimpleStringList;
         procedure Release(sl : TSimpleStringList);
         procedure Flush;
   end;

   // TObjectList
   //
   {: A simple generic object list, owns objects }
   TObjectList<T{$IFNDEF FPC}: TRefCountedObject{$ENDIF}> = class
      private
         type
            ArrayT = array of T;
         var
            FItems : ArrayT;
            FCount : Integer;

      protected
         function GetItem(index : Integer) : T; {$IFDEF DELPHI_2010_MINUS}{$ELSE} inline; {$ENDIF}
         procedure SetItem(index : Integer; const item : T);

      public
         destructor Destroy; override;
         function Add(const anItem : T) : Integer;
         function IndexOf(const anItem : T) : Integer;
         procedure Insert(idx : Integer; const anItem : T);
         function Extract(idx : Integer) : T;
         procedure ExtractAll;
         procedure Clear;
         property Items[index : Integer] : T read GetItem write SetItem; default;
         property Count : Integer read FCount;
         procedure Move(idx, idxNew, itemCount : Integer);
   end;

   // TSortedList
   //
   {: List that maintains its elements sorted, subclasses must override Compare }
   TSortedList<T{$IFNDEF FPC}: TRefCountedObject{$ENDIF}> = class
      private
         type
            ArrayT = array of T;
         var
            FItems : ArrayT;
            FCount : Integer;

      protected
         function GetItem(index : Integer) : T;
         function Find(const item : T; var index : Integer) : Boolean;
         function Compare(const item1, item2 : T) : Integer; virtual; abstract;
         procedure InsertItem(index : Integer; const anItem : T);

      public
         type TSortedListEnumerator = record
            private
               FList : TSortedList<T>;
               FIndex, FCountMinus1 : Integer;
            public
               function MoveNext: Boolean; inline;
               function GetCurrent : T; inline;
               property Current : T read GetCurrent;
         end;

         function Add(const anItem : T) : Integer;
         function AddOrFind(const anItem : T; var added : Boolean) : Integer;
         function Extract(const anItem : T) : Integer;
         function ExtractAt(index : Integer) : T;
         function IndexOf(const anItem : T) : Integer;
         procedure Clear;
         procedure Clean;
         procedure Enumerate(const callback : TSimpleCallback<T>);
         function GetEnumerator : TSortedListEnumerator;
         property Items[index : Integer] : T read GetItem; default;
         property Count : Integer read FCount;
   end;

   // TSimpleStack<T>
   //
   {: A minimalistic generic stack.
      Note that internal array items are NOT cleared on Pop, for refcounted types,
      you need to clear yourself manually via Peek. }
   TSimpleStack<T> = class
      private
      type ArrayT = array of T;
      var
         FItems : ArrayT;
         FCount : Integer;
         FCapacity : Integer;
      protected
         procedure Grow;
         function GetPeek : T; inline;
         procedure SetPeek(const item : T);
         function GetItems(const position : Integer) : T;
         procedure SetItems(const position : Integer; const value : T);
      public
         procedure Push(const item : T);
         procedure Pop; inline;
         procedure Clear;
         property Peek : T read GetPeek write SetPeek;
         property Items[const position : Integer] : T read GetItems write SetItems;
         property Count : Integer read FCount;
   end;

   PSimpleIntegerStackChunk = ^TSimpleIntegerStackChunk;
   TSimpleIntegerStackChunk = record
      public
         const ChunkSize = 16-2;
      public
         Data : array [0..ChunkSize-1] of Integer;
         Prev : PSimpleIntegerStackChunk;
   end;

   // TSimpleIntegerStack
   //
   {: A minimalistic chunked integer stack. }
   TSimpleIntegerStack = class
      private
         FChunk : PSimpleIntegerStackChunk;
         FChunkIndex : Integer;
         FCount : Integer;
         FPooledChunk : PSimpleIntegerStackChunk;
         FBaseChunk : TSimpleIntegerStackChunk;

         class var vTemplate : TSimpleIntegerStack;

      protected
         procedure Grow;
         procedure Shrink;
         function GetPeek : Integer; inline;
         procedure SetPeek(const item : Integer); inline;

      public
         constructor Create;
         destructor Destroy; override;
         class function Allocate : TSimpleIntegerStack; static;

         procedure Push(const item : Integer); inline;
         procedure Pop; inline;
         procedure Clear;
         property Peek : Integer read GetPeek write SetPeek;
         property Count : Integer read FCount;
   end;

   TSimpleHashBucket<T> = record
      HashCode : Cardinal;
      Value : T;
   end;
   TSimpleHashBucketArray<T> = array of TSimpleHashBucket<T>;
   TSimpleHashAction = (shaNone, shaRemove, shaAbort);
   TSimpleHashFunc<T> = function (const item : T) : TSimpleHashAction of object;

   {: Minimalistic open-addressing hash, subclasses must override SameItem and GetItemHashCode.
      HashCodes *MUST* be non zero }
   TSimpleHash<T> = class
      protected
         {$IFDEF DELPHI_XE3}
         // workaround for XE3 compiler bug
         FBuckets : array of TSimpleHashBucket<T>;
         {$ELSE}
         FBuckets : TSimpleHashBucketArray<T>;
         {$ENDIF}
         FCount : Integer;
         FGrowth : Integer;
         FCapacity : Integer;

         procedure Grow(capacityPreAdjusted : Boolean);
         function LinearFind(const item : T; var index : Integer) : Boolean;
         function SameItem(const item1, item2 : T) : Boolean; virtual; abstract;
         // hashCode must be non-null
         function GetItemHashCode(const item1 : T) : Cardinal; virtual; abstract;

         const cSimpleHashMinCapacity = 32;

      public
         function Add(const anItem : T) : Boolean; // true if added
         function AddHashed(const anItem : T; const hashCode : Cardinal) : Boolean; // true if added
         function Replace(const anItem : T) : Boolean; // true if added
         function Contains(const anItem : T) : Boolean;
         function Match(var anItem : T) : Boolean;
         function Remove(const anItem : T) : Boolean; // true if removed
         procedure Enumerate(const callBack : TSimpleHashFunc<T>);
         procedure Clear(resetCapacity : Boolean = True);
         procedure PreallocateCapacity(newCapacity : Integer);

         function HashBucketValue(index : Integer; var anItem : T) : Boolean; inline;
         procedure Delete(index : Integer);

         property Count : Integer read FCount;

         property Capacity : Integer read FCapacity;
   end;

   TSimpleObjectHash<T{$IFNDEF FPC}: TRefCountedObject{$ENDIF}> = class(TSimpleHash<T>)
      protected
         function SameItem(const item1, item2 : T) : Boolean; override;
         function GetItemHashCode(const item1 : T) : Cardinal; override;

      public
         procedure Clean;
   end;

   TNameObjectHashBucket = record
      HashCode : Cardinal;
      Name : String;
      Obj : TObject;
   end;
   PNameObjectHashBucket = ^TNameObjectHashBucket;
   TNameObjectHashBuckets = array of TNameObjectHashBucket;

   TNameObjectHash = class
      private
         FBuckets : TNameObjectHashBuckets;
         FCount : Integer;
         FGrowth : Integer;
         FHighIndex : Integer;
         class var vHashSalt : Cardinal;

      protected
         procedure Grow;
         procedure Resize(newSize : Integer);

         function GetHashedIndex(const aName : String; aHash : Cardinal) : Integer;
         function GetIndex(const aName : String) : Integer;

         function GetHashedObjects(const aName : String; aHash : Cardinal) : TObject; inline;
         function GetObjects(const aName : String) : TObject; inline;

         procedure SetHashedObjects(const aName : String; aHash : Cardinal; obj : TObject); inline;
         procedure SetObjects(const aName : String; obj : TObject); inline;

         function GetBucket(index : Integer) : PNameObjectHashBucket; inline;

         function GetBucketObject(index : Integer) : TObject; inline;
         procedure SetBucketObject(index : Integer; obj : TObject); inline;
         function GetBucketName(index : Integer) : String; inline;

      public
         constructor Create(initialCapacity : Integer = 0);

         class function HashName(const aName : String) : Cardinal; static; inline;
         function AddObject(const aName : String; aObj : TObject; replace : Boolean = False) : Boolean;
         function AddHashedObject(const aName : String; aHash : Cardinal; aObj : TObject; replace : Boolean = False) : Boolean;

         procedure Clean;
         procedure Clear;
         procedure Pack;

         property Objects[const aName : String] : TObject read GetObjects write SetObjects; default;
         property HashedObjects[const aName : String; aHash : Cardinal] : TObject read GetHashedObjects write SetHashedObjects;

         property Bucket[index : Integer] : PNameObjectHashBucket read GetBucket;
         property BucketObject[index : Integer] : TObject read GetBucketObject write SetBucketObject;
         property BucketName[index : Integer] : String read GetBucketName;
         property BucketIndex[const aName : String] : Integer read GetIndex;
         property BucketHashedIndex[const aName : String; aHash : Cardinal] : Integer read GetHashedIndex;

         property Count : Integer read FCount;
         property HighIndex : Integer read FHighIndex;

         function Names : TStringDynArray;
   end;

   TSimpleRefCountedObjectHash = class (TSimpleObjectHash<TRefCountedObject>);

   TSimpleNameObjectHash<T{$IFNDEF FPC}: class{$ENDIF}> = class
      private
         FHash : TNameObjectHash;

      protected
         function GetIndex(const aName : String) : Integer; inline;
         function GetObjects(const aName : String) : T; inline;
         procedure SetObjects(const aName : String; obj : T); inline;
         function GetBucketObject(index : Integer) : T; inline;
         procedure SetBucketObject(index : Integer; obj : T); inline;
         function GetBucketName(index : Integer) : String; inline;

      public
         constructor Create(initialCapacity : Integer = 0);
         destructor Destroy; override;

         function AddObject(const aName : String; aObj : T; replace : Boolean = False) : Boolean; inline;

         procedure Clean; inline;
         procedure Clear; inline;
         procedure Pack; inline;

         property Objects[const aName : String] : T read GetObjects write SetObjects; default;

         property BucketObject[index : Integer] : T read GetBucketObject write SetBucketObject;
         property BucketName[index : Integer] : String read GetBucketName;
         property BucketIndex[const aName : String] : Integer read GetIndex;

         function Count : Integer; inline;
         function HighIndex : Integer; inline;
   end;

   TObjectObjectHashBucket<TKey, TValue{$IFNDEF FPC}: TRefCountedObject{$ENDIF}> = record
      Key : TKey;
      Value : TValue;
   end;

   TSimpleObjectObjectHash<TKey, TValue{$IFNDEF FPC}: TRefCountedObject{$ENDIF}> = class
      type
         TObjectObjectHashBucket = record
            HashCode : Cardinal;
            Name : String;
            Key : TKey;
            Value : TValue;
         end;
         TObjectObjectHashBuckets = array of TObjectObjectHashBucket;

      private
         FBuckets : TObjectObjectHashBuckets;
         FCount : Integer;
         FGrowth : Integer;
         FCapacity : Integer;

      protected
         procedure Grow;
         function GetItemHashCode(const item1 : TObjectObjectHashBucket) : Integer;
         function LinearFind(const item : TObjectObjectHashBucket; var index : Integer) : Boolean;
         function Match(var anItem : TObjectObjectHashBucket) : Boolean;
         function Replace(const anItem : TObjectObjectHashBucket) : Boolean; // true if added

      public
         function  GetValue(aKey : TKey) : TValue;
         procedure SetValue(aKey : TKey; aValue : TValue);
         procedure Clear;

         procedure CleanValues;
   end;

   TObjectsLookup = class (TSortedList<TRefCountedObject>)
      protected
         function Compare(const item1, item2 : TRefCountedObject) : Integer; override;
   end;

   TStringUnifierBucket = record
      Hash : Cardinal;
      Str : String;
   end;
   PStringUnifierBucket = ^TStringUnifierBucket;
   TStringUnifierBuckets = array of TStringUnifierBucket;

   TStringUnifier = class
      private
         FBuckets : TStringUnifierBuckets;
         FCount : Integer;
         FGrowth : Integer;
         FCapacity : Integer;
         FMask : Integer;
         FTag : Integer;

      protected
         procedure Grow;

      public
         constructor Create;

         procedure UnifyAssign(const aString : String; var unifiedString : String); inline;
         procedure UnifyAssignP(p : PChar; size : Integer; var unifiedString : String);

         property Count : Integer read FCount;
         function DistinctStrings : TStringDynArray;
         procedure Clear;

         property Tag : Integer read FTag write FTag;

         const HighestUnifiedChar = #$007F;
   end;

   TThread<T: constructor, class> = class
      private
         FLock : TdwsCriticalSection;
         FValue : T;

      public
         constructor Create(const anObj : T);
         destructor Destroy; override;

         function Lock : T;
         procedure Unlock;
   end;

   TThreadCached<T> = class
      private
         FLock : TMultiReadSingleWrite;
         FExpiresAt : Int64;
         FMaxAge : Int64;
         FOnNeedValue : TSimpleCallback<T>;
         FValue : T;

      protected
         function GetValue : T;
         procedure SetValue(const v : T);

      public
         constructor Create(const aNeedValue : TSimpleCallback<T>; maxAgeMSec : Integer);
         destructor Destroy; override;

         procedure Invalidate;

         property Value : T read GetValue write SetValue;
         property MaxAge : Int64 read FMaxAge;
   end;

   // TSimpleQueue<T>
   //
   {: A minimalistic generic queue.
      Based on a linked list with an items pool, supports both FIFO & LIFO. }
   TSimpleQueue<T> = class
      private
      type
         PItemT = ^ItemT;
         ItemT = record
            Prev, Next : PItemT;
            Value : T;
         end;
      var
         FFirst, FLast : PItemT;
         FCount : Integer;
         FPool : PItemT;
         FPoolLeft : Integer;

      protected
         function Alloc : PItemT;
         procedure Release(i : PItemT);

      public
         constructor Create(poolSize : Integer = 8);
         destructor Destroy; override;

         // Adds to the end of the queue
         procedure Push(const v : T);
         // Removes from the end of the queue
         function  Pop(var v : T) : Boolean; overload;
         function  Pop : T; overload;
         // Adds to the beginning of the queue
         procedure Insert(const v : T);
         // Removes from the beginning of the queue
         function  Pull(var v : T) : Boolean; overload;
         function  Pull : T; overload;

         procedure Clear;

         property Count : Integer read FCount;
         property First : PItemT read FFirst;
         property Last : PItemT read FLast;
   end;

const
   cWriteOnlyBlockStreamBlockSize = $2000 - 2*SizeOf(Pointer);
   cNameObjectHashMinSize = 32;

type
   // TWriteOnlyBlockStream
   //
   {: Provides a write-only block-based stream. }
   TWriteOnlyBlockStream = class (TStream)
      private
         FFirstBlock : PPointerArray;
         FCurrentBlock : PPointerArray;
         FBlockRemaining : PInteger;
         FTotalSize : Integer;

      protected
         const cCRLF : array [0..1] of WideChar = (#13, #10);
         const cAsciiCRLF : array [0..1] of AnsiChar = (#13, #10);

         function GetSize: Int64; override;

         procedure AllocateCurrentBlock;
         procedure FreeBlocks;

         procedure WriteSpanning(source : PByteArray; count : Integer);
         procedure WriteLarge(source : PByteArray; count : Integer);
         procedure WriteBuf(source : PByteArray; count : Integer);

      public
         constructor Create;
         destructor Destroy; override;

         class function AllocFromPool : TWriteOnlyBlockStream; static;
         procedure ReturnToPool;

         function Seek(Offset: Longint; Origin: Word): Longint; override;
         function Read(var {%H-}Buffer; {%H-}Count: Longint): Longint; override;
         function Write(const buffer; count: Longint): Longint; override;

         procedure WriteBytes(const b : array of Byte); overload;
         procedure WriteBytes(const buffer : RawByteString); overload;

         procedure WriteByte(b : Byte); inline;
         procedure WriteShortInt(s : ShortInt); inline;
         procedure WriteWord(w : Word); inline;
         procedure WriteSmallInt(s : SmallInt); inline;
         procedure WriteInt32(const i : Integer); inline;
         procedure WriteInt64(const i : Int64); inline;
         procedure WriteDWord(const dw : DWORD); inline;
         procedure WriteQWord(const qw : UInt64); inline;
         procedure WriteSingle(const s : Single); inline;
         procedure WriteDouble(const d : Double); inline;

         procedure WriteP(p : PWideChar; nbWideChars : Integer); inline;

         // must be strictly an utf16 String
         procedure WriteString(const utf16String : UnicodeString); overload; inline;
         {$ifdef FPC}
         procedure WriteString(const s : String); overload; inline;
         {$endif}
         procedure WriteString(const i : Int32); overload; inline;
         procedure WriteString(const i : Int64); overload; inline;
         procedure WriteSubString(const utf16String : UnicodeString; startPos : Integer); overload;
         procedure WriteSubString(const utf16String : UnicodeString; startPos, aLength : Integer); overload;
         procedure WriteUTF8String(const utf8String : RawByteString); overload; inline;
         procedure WriteCRLF; inline;
         procedure WriteAsciiCRLF; inline;
         procedure WriteIndent(nb : Integer; indentChar : WideChar = ' ');
         procedure WriteChar(utf16Char : WideChar); inline;
         procedure WriteDigits(value : Int64; digits : Integer); overload;
         procedure WriteDigits(value : Cardinal; digits : Integer); overload;

         function TailWord : Word;

         function ToString : String; overload; override; final;
         function ToUnicodeString : UnicodeString; inline;
         function ToUTF8String : RawByteString;
         function ToBytes : TBytes;
         function ToRawBytes : RawByteString;

         procedure Clear;

         procedure StoreData(var buffer); overload;
         procedure StoreData(destStream : TStream); overload;
         procedure StoreData(var dest : UnicodeString); overload;
         procedure StoreUTF8Data(destStream : TStream); overload;
   end;

   TSimpleInt64List = class(TSimpleList<Int64>)
      public
         procedure Sort;
   end;

   TSimpleDoubleList = class(TSimpleList<Double>)
      protected
         procedure MedianSort(minIndex, maxIndex : Integer);

      public
         procedure Exchange(index1, index2 : Integer);
         procedure Sort;

         function QuickSum : Double;
         function KahanSum : Double;
         function NeumaierSum : Double;

         // Computes median using a partial sort (alters the list)
         function QuickMedian : Double;
   end;

   TSimpleStringHash = class(TSimpleHash<String>)
      protected
         function SameItem(const item1, item2 : String) : Boolean; override;
         function GetItemHashCode(const item1 : String) : Cardinal; override;
   end;

   TFastCompareTextList = class (TStringList)
      {$ifdef FPC}
      function DoCompareText(const s1,s2 : String) : PtrInt; override;
      {$else}
      function CompareStrings(const S1, S2: String): Integer; override;
      function FindName(const name : String; var index : Integer) : Boolean;
      function IndexOfName(const name : String): Integer; override;
      {$endif}
      procedure Reverse;

      function SameText(idx : Integer; const value : String) : Boolean;
   end;

   TClassCloneConstructor<T: class, constructor> = record
      private
         FTemplate : T;
         FSize : Integer;

      public
         procedure Initialize(aTemplate : T);
         procedure Finalize;
         function Create : T; inline;
   end;

   TClassInstanceTemplate<T: class> = record
      private
         FTemplate : Pointer;
         FPool : Pointer;

      public
         procedure Initialize;
         procedure Finalize;
         function Initialized : Boolean; inline;

         function CreateInstance : T; inline;
         procedure ReleaseInstance(instance : T); inline;
   end;

   ETightListOutOfBound = class(Exception)
   end;

   TQuickSort = record
      public
         CompareMethod : function (index1, index2 : NativeInt) : Integer of object;
         SwapMethod : procedure (index1, index2 : NativeInt) of object;

         procedure Sort(minIndex, maxIndex : NativeInt);
   end;

   PFormatSettings = ^TFormatSettings;

   TPooledObject = class
      private
         FNext : TPooledObject;

      public
         constructor Create; virtual;
   end;
   TPooledObjectClass = class of TPooledObject;

   TPool = record
      private
         FRoot : TPooledObject;
         FPoolClass : TPooledObjectClass;
         FLock : TMultiReadSingleWrite;
         FCount : Integer;
         FCapacity : Integer;

      public
         procedure Initialize(aClass : TPooledObjectClass);
         procedure Finalize;

         procedure Clean(nb : Integer);

         function Acquire : TPooledObject;
         procedure Release(obj : TPooledObject);

         property Count : Integer read FCount;
         property Capacity : Integer read FCapacity write FCapacity;
   end;

   TStringDynArrayHelper = record helper for TStringDynArray
      function Join(const separator : String) : String;
   end;

const
   cMSecToDateTime : Double = 1/(24*3600*1000);

function AcquireUnifier : TStringUnifier;
procedure ReleaseUnifier(unifier : TStringUnifier);

procedure UnifyAssignString(const fromStr : String; var toStr : String); inline;
procedure UnifyAssignP(p : PChar; size : Integer; var toStr : String);
procedure UnifyAssignChar(p : PChar; var toStr : String);
function  UnifiedString(const fromStr : String) : String; inline;
function  TidyStringsUnifier : Integer;

function UnicodeCompareLen(p1, p2 : PWideChar; n : Integer) : Integer;
function UnicodeCompareText(const s1, s2 : UnicodeString) : Integer; overload;
function UnicodeSameText(const s1, s2 : UnicodeString) : Boolean; overload;
function UnicodeCompareStr(const s1, s2 : UnicodeString) : Integer; overload; inline;

function StrEquals(const s1, s2 : UnicodeString) : Boolean;

{$ifdef FPC}
function UnicodeCompareText(const s1, s2 : String) : Integer; overload;
function UnicodeCompareStr(const s1, s2 : String) : Integer; overload; inline;
function UnicodeSameText(const s1, s2 : String) : Boolean; overload;
{$endif}

function AsciiCompareLen(p1, p2 : PAnsiChar; n : Integer) : Integer; overload;

function PosA(const sub, main : RawByteString) : Integer; inline;

function StrIsASCII(const s : String) : Boolean; overload;
function StrIsASCII(const s : RawByteString) : Boolean; overload;

function StrNonNilLength(const aString : String) : Integer; inline;

function StrIBeginsWith(const aStr, aBegin : String) : Boolean;
function StrBeginsWith(const aStr, aBegin : String) : Boolean;
function StrBeginsWithA(const aStr, aBegin : RawByteString) : Boolean;
function StrBeginsWithBytes(const aStr : RawByteString; const bytes : array of Byte) : Boolean;
function StrIEndsWith(const aStr, aEnd : String) : Boolean;
function StrIEndsWithA(const aStr, aEnd : RawByteString) : Boolean;
function StrEndsWith(const aStr, aEnd : String) : Boolean;
function StrEndsWithA(const aStr, aEnd : RawByteString) : Boolean;
function StrContains(const aStr, aSubStr : String) : Boolean; overload;
function StrContains(const aStr : String; aChar : Char) : Boolean; overload;
function StrIndexOfChar(const aStr : String; aChar : Char; startIndex : Integer = 1) : Integer;
function StrIndexOfCharA(const aStr : RawByteString; aChar : AnsiChar) : Integer;
function StrLastIndexOfCharA(const aStr : RawByteString; aChar : AnsiChar) : Integer;
function LowerCaseA(const aStr : RawByteString) : RawByteString;

function StrMatches(const aStr, aMask : String) : Boolean;

function StrDeleteLeft(const aStr : String; n : Integer) : String; inline;
function StrDeleteRight(const aStr : String; n : Integer) : String; inline;

function StrAfterChar(const aStr : String; aChar : Char) : String;
function StrBeforeChar(const aStr : String; aChar : Char) : String;

function StrReplaceChar(const aStr : String; oldChar, newChar : Char) : String;
function StrReplaceMacros(const aStr : String; const macros : array of String;
                          const startDelimiter, stopDelimiter : String) : String;

function StrCountString(const haystack, needle : String) : Integer;
function StrCountChar(const aStr : String; c : Char) : Integer;
function StrCountStartChar(const aStr : String; c : Char) : Integer;

function WhichPowerOfTwo(const v : Int64) : Integer;

{$ifdef FPC}
function SimpleStringHash(const s : String) : Cardinal; overload;
function SimpleStringHash(p : PAnsiChar; sizeInChars : Integer) : Cardinal; overload;
{$endif}
function SimpleStringHash(const s : UnicodeString) : Cardinal; overload; inline;
function SimpleStringHash(const s : RawByteString) : Cardinal; overload; inline;
function SimpleStringHash(p : PWideChar; sizeInChars : Integer) : Cardinal; overload; inline;
function SimpleStringCaseInsensitiveHash(const s : UnicodeString) : Cardinal;

function SimpleByteHash(p : PByte; n : Integer) : Cardinal;

function SimpleIntegerHash(x : Cardinal) : Cardinal;
function SimplePointerHash(x : Pointer) : Cardinal;
function SimpleInt64Hash(x : Int64) : Cardinal;

function RawByteStringToScriptString(const s : RawByteString) : UnicodeString; overload; inline;
procedure RawByteStringToScriptString(const s : RawByteString; var result : UnicodeString); inline; overload;
procedure RawByteStringToScriptString(const s : RawByteString; var result : Variant); overload;

function ScriptStringToRawByteString(const s : UnicodeString) : RawByteString; overload; inline;
procedure ScriptStringToRawByteString(const s : UnicodeString; var result : RawByteString); overload;

procedure StringBytesToWords(var buf : UnicodeString; swap : Boolean);
procedure StringWordsToBytes(var buf : UnicodeString; swap : Boolean);

procedure UTF8DecodeToUnicodeString(pUTF8 : PAnsiChar; utf8Length : Integer; var decoded : String);

function StringToUTF8(const unicodeString : String) : RawByteString; inline;
function UTF8ToString(const utf8String : RawByteString) : String; inline;

function IsValidUTF8(const buf : RawByteString) : Boolean; inline; overload;
function IsValidUTF8(p : PByte; byteSize : Integer) : Boolean; overload;

type
   EHexEncodingException = class (Exception)
   end;

function BinToHex(data : Pointer; n : Integer) : UnicodeString; overload;
function BinToHex(const data; n : Integer) : UnicodeString; overload; inline;
function BinToHex(const data : RawByteString) : UnicodeString; overload; inline;
function BinToHexA(data : Pointer; n : Integer) : RawByteString; overload;

function HexToBin(const data : String) : RawByteString; overload;
procedure HexToBin(src : PChar; dest : PByte; nbBytes : Integer); overload;

type
   TInt64StringBuffer = array [0..21] of Char;
   TInt32StringBuffer = array [0..11] of Char;

function FastInt32ToBuffer(const val : Int32; var buf : TInt32StringBuffer) : Integer;
function FastInt64ToBuffer(const val : Int64; var buf : TInt64StringBuffer) : Integer;
procedure FastInt64ToStr(const val : Int64; var s : String); overload;
function  FastInt64ToStr(const val : Int64) : String; overload; inline;
procedure FastInt64ToHex(val : Int64; digits : Integer; var s : String);

procedure FastFloatToStr(const val : Extended; var s : String; const fmtSettings : TFormatSettings);

function  Int32ToStrU(val : Integer) : UnicodeString;
function  StrUToInt64(const s : UnicodeString; const default : Int64) : Int64;
function  TryStrToIntBase(const s : UnicodeString; base : Integer; var value : Int64) : Boolean;

function Int64ToStrBase(val : Int64; base : Integer) : String;

function Int64ToHex(val : Int64; digits : Integer) : String; inline;

function TryStrToDouble(const s : String; var val : Double) : Boolean; overload; inline;
function TryStrToDouble(const s : String; var val : Double; const formatSettings : TFormatSettings) : Boolean; overload; inline;
function TryStrToDouble(p : PChar; var val : Double; formatSettings : PFormatSettings = nil) : Boolean; overload;

function StringToBoolean(const s : String) : Boolean;

function DivMod10(var dividend : Cardinal) : Cardinal;
function DivMod100(var dividend : Cardinal) : Cardinal;

procedure FastStringReplace(var str : UnicodeString; const sub, newSub : UnicodeString); overload;
{$ifdef FPC}
procedure FastStringReplace(var str : String; const sub, newSub : String); overload;
{$endif}

procedure VariantToString(const v : Variant; var s : String); overload;
{$ifdef FPC}
procedure VariantToString(const v : Variant; var s : UnicodeString); overload;
{$endif}
function  VariantToString(const v : Variant) : String; overload; inline;
function  VariantToUnicodeString(const v : Variant) : UnicodeString; inline;
procedure VariantToInt64(const v : Variant; var r : Int64); overload; inline;
function  VariantToInt64(const v : Variant) : Int64; overload;
function  VariantToBool(const v : Variant) : Boolean;
function  VariantToFloat(const v : Variant) : Double;

function  VariantType(const v : Variant) : TVarType; inline;
function  VariantIsOrdinal(const v : Variant) : Boolean; inline;
function  VariantIsFloat(const v : Variant) : Boolean; inline;
function  VariantIsString(const v : Variant) : Boolean; inline;
function  VariantIsArray(const v : Variant) : Boolean; inline;

procedure VarClearSafe(var v : Variant);
procedure VarCopySafe(var dest : Variant; const src : Variant); overload;
procedure VarCopySafe(var dest : Variant; const src : IUnknown); overload;
procedure VarCopySafe(var dest : Variant; const src : IDispatch); overload;
procedure VarCopySafe(var dest : Variant; const src : Int64); overload;
procedure VarCopySafe(var dest : Variant; const src : String); overload;
{$ifdef FPC}
procedure VarCopySafe(var dest : Variant; const src : UnicodeString); overload;
{$endif}
procedure VarCopySafe(var dest : Variant; const src : Double); overload;
procedure VarCopySafe(var dest : Variant; const src : Boolean); overload;

function VarCompareSafe(const left, right : Variant) : TVariantRelationship;

procedure VarSetNull(var v : Variant); inline;
procedure VarSetDefaultInt64(var dest : Variant); inline;
procedure VarSetDefaultDouble(var dest : Variant); inline;
procedure VarSetDefaultString(var dest : Variant); inline;
procedure VarSetDateTime(var dest : Variant; const dt : Double);

function  VariantArrayHighBound(const v : Variant; index : Integer) : Integer; inline;

procedure WriteVariant(writer: TWriter; const value: Variant);
function ReadVariant(reader: TReader): Variant;

function TryISO8601ToDateTime(const v : String; var aResult : TDateTime) : Boolean;
function ISO8601ToDateTime(const v : String) : TDateTime;

type
   TISO8601Precision = (
      iso8601precAuto,
      iso8601precSeconds,
      iso8601precMilliseconds
   );
function DateTimeToISO8601(dt : TDateTime; extendedFormat : Boolean; prec : TISO8601Precision = iso8601precAuto) : String;

procedure dwsFreeAndNil(var O); // transitional function, do not use

function CoalesceableIsFalsey(const unk : IUnknown) : Boolean;

function ApplyStringVariables(const str : TFilename; const variables : TStrings;
                              const delimiter : String = '%') : TFilename;

function ScanMemory(p : Pointer; size : IntPtr; const pattern : TBytes) : Pointer;

function PopCount32(v : Int32) : Integer;
function PopCount64(p : PInt64; nbInt64s : Integer) : Integer;
function PopCount(p : PByte; n : Integer) : Integer;

procedure SwapInt64(var a, b : Int64); inline;
procedure SwapSingles(var a, b : Single); inline;
procedure SwapDoubles(var a, b : Double); inline;
procedure SwapPointers(var a, b : Pointer); inline;

procedure ExchangeBytes(const a, b; size : Integer);

procedure TransferSimpleHashBuckets(const src, dest; nbSrcBuckets, nbDestBuckets, bucketSize : Integer);

procedure QuickSortDoublePrecision(a : PDoubleArray; minIndex, maxIndex : NativeInt);
procedure QuickSortInt64(a : PInt64Array; minIndex, maxIndex : NativeInt);
procedure QuickSortString(a : PStringArray; minIndex, maxIndex : NativeInt);
procedure QuickSortVariant(a : PVariantArray; minIndex, maxIndex : NativeInt);

type
   EISO8601Exception = class (Exception);

   TTwoChars = packed array [0..1] of Char;
   PTwoChars = ^TTwoChars;
const
   cTwoDigits : packed array [0..99] of TTwoChars = (
      ('0','0'), ('0','1'), ('0','2'), ('0','3'), ('0','4'), ('0','5'), ('0','6'), ('0','7'), ('0','8'), ('0','9'),
      ('1','0'), ('1','1'), ('1','2'), ('1','3'), ('1','4'), ('1','5'), ('1','6'), ('1','7'), ('1','8'), ('1','9'),
      ('2','0'), ('2','1'), ('2','2'), ('2','3'), ('2','4'), ('2','5'), ('2','6'), ('2','7'), ('2','8'), ('2','9'),
      ('3','0'), ('3','1'), ('3','2'), ('3','3'), ('3','4'), ('3','5'), ('3','6'), ('3','7'), ('3','8'), ('3','9'),
      ('4','0'), ('4','1'), ('4','2'), ('4','3'), ('4','4'), ('4','5'), ('4','6'), ('4','7'), ('4','8'), ('4','9'),
      ('5','0'), ('5','1'), ('5','2'), ('5','3'), ('5','4'), ('5','5'), ('5','6'), ('5','7'), ('5','8'), ('5','9'),
      ('6','0'), ('6','1'), ('6','2'), ('6','3'), ('6','4'), ('6','5'), ('6','6'), ('6','7'), ('6','8'), ('6','9'),
      ('7','0'), ('7','1'), ('7','2'), ('7','3'), ('7','4'), ('7','5'), ('7','6'), ('7','7'), ('7','8'), ('7','9'),
      ('8','0'), ('8','1'), ('8','2'), ('8','3'), ('8','4'), ('8','5'), ('8','6'), ('8','7'), ('8','8'), ('8','9'),
      ('9','0'), ('9','1'), ('9','2'), ('9','3'), ('9','4'), ('9','5'), ('9','6'), ('9','7'), ('9','8'), ('9','9')
      );

var
   vValueOfNullVariantAsString : String = 'Null';

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses
   System.Math, System.Masks,
   dwsStrings, dwsXXHash, dwsUTF8;

// CoalesceableIsFalsey
//
function CoalesceableIsFalsey(const unk : IUnknown) : Boolean;
var
   c : ICoalesceable;
begin
   Result :=    (unk=nil)
             or (    (unk.QueryInterface(ICoalesceable, c)=S_OK)
                 and c.IsFalsey);
end;

// StrEquals
//
function StrEquals(const s1, s2 : UnicodeString) : Boolean;
{$ifdef FPC}
begin
   Result := s1 = s2;
end;
{$else}
var
   n : Integer;
begin
   if s1 = '' then
      Result := (s2 = '')
   else begin
      n := Length(s1);
      if Length(s2) <> n then
         Result := False
      else Result := System.SysUtils.CompareMem(Pointer(s1), Pointer(s2), n*SizeOf(Char));
   end;
end;
{$endif}

// dwsFreeAndNil
//
procedure dwsFreeAndNil(var o);
var
   obj : TObject;
begin
   obj:=TObject(o);
   Pointer(o):=nil;
   if obj is TRefCountedObject then
      TRefCountedObject(obj).Free
   else obj.Free;
end;

{$ifdef FPC}
// SimpleStringHash
//
function SimpleStringHash(const s : String) : Cardinal;
begin
   Result := SimpleStringHash(UnicodeString(s));
end;

// SimpleStringHash
//
function SimpleStringHash(p : PAnsiChar; sizeInChars : Integer) : Cardinal;
var
   s : String;
begin
   SetString(s, p, sizeInChars);
   Result := SimpleStringHash(s);
end;
{$endif FPC}

// SimpleStringHash
//
function SimpleStringHash(const s : UnicodeString) : Cardinal;
begin
   Result := xxHash32.Full(Pointer(s), Length(s)*SizeOf(WideChar));
end;

// SimpleStringCaseInsensitiveHash
//
function SimpleStringCaseInsensitiveHash(const s : UnicodeString) : Cardinal;

   function Fallback(const s : UnicodeString) : Cardinal;
   var
      lc : String;
   begin
      UnicodeLowerCase(s, lc);
      Result := SimpleStringHash(lc);
   end;

var
   localBuffer : array [0..63] of WideChar;
   c : Cardinal;
begin
   var n := Length(s);
   if n > Length(localBuffer) then
      Exit(Fallback(s));

   var buf := PWideChar(@localBuffer);
   var p := PWideChar(Pointer(s));

   var i := n;
   while i >= 2 do begin
      c := PCardinal(p)^;
      if (c and $FF80FF80) <> 0 then
         Exit(Fallback(s));
      PInteger(buf)^ := c or $00200020;
      Inc(buf, 2);
      Inc(p, 2);
      Dec(i, 2);
   end;
   if i <> 0 then begin
      c := Ord(p^);
      if c > 127 then
         Exit(Fallback(s));
      buf^ := WideChar(c or $0020);
   end;
   Result := xxHash32.Full(@localBuffer, n*SizeOf(WideChar));
end;

// SimpleStringHash
//
function SimpleStringHash(const s : RawByteString) : Cardinal;
begin
   Result := xxHash32.Full(Pointer(s), Length(s)*SizeOf(AnsiChar));
end;

// SimpleStringHash
//
function SimpleStringHash(p : PWideChar; sizeInChars : Integer) : Cardinal;
begin
   Result := xxHash32.Full(p, sizeInChars*SizeOf(WideChar));
end;

// SimpleByteHash
//
function SimpleByteHash(p : PByte; n : Integer) : Cardinal;
begin
   Result := 2166136261;
   for n:=n downto 1 do begin
      Result:=(Result xor p^)*16777619;
      Inc(p);
   end;
   if Result = 0 then Result := 1;
end;

// SimpleIntegerHash
//
function SimpleIntegerHash(x : Cardinal) : Cardinal;
begin
   // simplified MurmurHash 3
   Result := x * $cc9e2d51;
   Result := (Result shl 15) or (Result shr 17);
   Result := Result * $1b873593 + $e6546b64;
   if Result = 0 then Result := 1;
end;

// SimplePointerHash
//
function SimplePointerHash(x : Pointer) : Cardinal;
var
   mix : NativeUInt;
begin
   // based on xxHash finalizers
   mix := (NativeUInt(x) shr 2) * Cardinal(2246822519);
   mix := (mix xor (mix shr 15)) * Cardinal(3266489917);
   Result := (mix xor (mix shr 16));
   if Result = 0 then Result := 1;
end;

// SimpleInt64Hash
//
function SimpleInt64Hash(x : Int64) : Cardinal;
var
   k : Cardinal;
begin
   // simplified MurmurHash 3
   Result := Cardinal(x) * $cc9e2d51;
   Result := (Result shl 15) or (Result shr 17);
   Result := Result * $1b873593 + $e6546b64;

   k := (x shr 32) * $cc9e2d51;
   k := (k shl 15) or (k shr 17);
   Result := k * $1b873593 xor Result;
   if Result = 0 then Result := 1;
end;

// StringBytesToWords
//
procedure StringBytesToWords(var buf : UnicodeString; swap : Boolean);
type
   TTwoBytes = array [0..1] of Byte;
   TTwoWords = array [0..1] of Word;
var
   pSrc : ^TTwoBytes;
   pDest : ^TTwoWords;
   i, n : Integer;
begin
   n := Length(buf);
   if n > 0 then begin
      SetLength(buf, 2*n);
      pSrc := @PWideChar(Pointer(buf))[n-1];
      pDest := @PWideChar(Pointer(buf))[2*n-2];
      if swap then begin
         for i := 1 to n do begin
            pDest[1] := pSrc[0];
            pDest[0] := pSrc[1];
            Dec(pDest);
            Dec(pSrc);
         end;
      end else begin
         for i := 1 to n do begin
            pDest[1] := pSrc[1];
            pDest[0] := pSrc[0];
            Dec(pDest);
            Dec(pSrc);
         end;
      end;
   end;
end;

// StringWordsToBytes
//
procedure StringWordsToBytes(var buf : UnicodeString; swap : Boolean);
type
   TTwoBytes = array [0..1] of Byte;
   TTwoWords = array [0..1] of Word;
var
   pSrc : ^TTwoWords;
   pDest : ^TTwoBytes;
   i, n, b : Integer;
begin
   n := Length(buf) div 2;
   if n > 0 then begin
      pSrc := Pointer(buf);
      pDest := Pointer(buf);
      if swap then begin
         for i := 1 to n do begin
            b := pSrc[0];
            pDest[0] := pSrc[1];
            pDest[1] := b;
            Inc(pDest);
            Inc(pSrc);
         end;
      end else begin
         for i := 1 to n do begin
            pDest[1] := pSrc[1];
            pDest[0] := pSrc[0];
            Inc(pDest);
            Inc(pSrc);
         end;
      end;
      SetLength(buf, n);
   end else buf := '';
end;

// UTF8DecodeToUnicodeString
//
procedure UTF8DecodeToUnicodeString(pUTF8 : PAnsiChar; utf8Length : Integer; var decoded : String);
begin
   if utf8Length = 0 then begin
      decoded := '';
   end else begin
      SetLength(decoded, utf8Length);
      var nb : Integer := Utf8ToUnicode(PChar(decoded), utf8Length+1, pUTF8, utf8Length) - 1;
      if nb <> utf8Length then
         SetLength(decoded, nb);
   end;
end;

// StringToUTF8
//
function StringToUTF8(const unicodeString : String) : RawByteString;
begin
   Result := dwsUTF8.StringToUTF8(unicodeString);
end;

// UTF8ToString
//
function UTF8ToString(const utf8String : RawByteString) : String;
begin
   Result := dwsUTF8.UTF8ToString(utf8String);
end;

// IsValidUTF8
//
function IsValidUTF8(const buf : RawByteString) : Boolean;
begin
   Result := dwsUTF8.IsValidUTF8(Pointer(buf), Length(buf));
end;

// IsValidUTF8
//
function IsValidUTF8(p : PByte; byteSize : Integer) : Boolean;
begin
   Result := dwsUTF8.IsValidUTF8(p, byteSize);
end;

// BinToHex
//
function BinToHex(data : Pointer; n : Integer) : UnicodeString;
const
   cHexDigits : array [0..15] of Char = (
      '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
      'a', 'b', 'c', 'd', 'e', 'f'
   );
var
   i : Integer;
   pDest : PLongWord;
   p : PByte;
begin
   if n=0 then Exit;

   SetLength(Result, n*2);

   pDest:=Pointer(Result);
   p := data;
   for i:=1 to n do begin
      pDest^ := Ord(cHexDigits[p^ shr 4]) + (Ord(cHexDigits[p^ and 15]) shl 16);
      Inc(pDest);
      Inc(p);
   end;
end;

// BinToHexA
//
function BinToHexA(data : Pointer; n : Integer) : RawByteString; overload;
const
   cHexDigits : array [0..15] of AnsiChar = (
      '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
      'a', 'b', 'c', 'd', 'e', 'f'
   );
var
   i : Integer;
   pSrc : PByte;
   pDest : PWord;
begin
   if n=0 then Exit;

   SetLength(Result, n*2);

   pDest := Pointer(Result);
   pSrc := data;
   for i := 1 to n do begin
      pDest^ := Ord(cHexDigits[pSrc^ shr 4]) + (Ord(cHexDigits[pSrc^ and 15]) shl 8);
      Inc(pDest);
      Inc(pSrc);
   end;
end;

// BinToHex
//
function BinToHex(const data; n : Integer) : UnicodeString;
begin
   Result := BinToHex(@data, n);
end;

// BinToHex
//
function BinToHex(const data : RawByteString) : UnicodeString; overload;
begin
   Result:=BinToHex(Pointer(data)^, Length(data));
end;

// HexToBin
//
function HexToBin(const data : String) : RawByteString;
var
   n : Integer;
begin
   n := Length(data);
   if (n and 1)<>0 then
      raise EHexEncodingException.Create('Even hexadecimal character count expected');

   n:=n shr 1;
   SetLength(Result, n);
   if n > 0 then
      HexToBin(PChar(Pointer(data)), PByte(Pointer(Result)), n);
end;

// HexToBin
//
procedure HexToBin(src : PChar; dest : PByte; nbBytes : Integer);
var
   i, b : Integer;
   c : Char;
begin
   for i := 1 to nbBytes do begin
      c := src[0];
      case c of
         '0'..'9' : b := (Ord(c) shl 4)-(Ord('0') shl 4);
         'A'..'F' : b := (Ord(c) shl 4)+(160-(Ord('A') shl 4));
         'a'..'f' : b := (Ord(c) shl 4)+(160-(Ord('a') shl 4));
      else
         raise EHexEncodingException.CreateFmt('Invalid hexadecimal character at index %d', [2*i-1]);
      end;
      c := src[1];
      case c of
         '0'..'9' : b := b + Ord(c) - Ord('0');
         'A'..'F' : b := b + Ord(c) + (10-Ord('A'));
         'a'..'f' : b := b + Ord(c) + (10-Ord('a'));
      else
         raise EHexEncodingException.CreateFmt('Invalid hexadecimal character at index %d', [2*i]);
      end;
      dest^ := b;
      Inc(dest);
      Inc(src, 2);
   end;
end;


// DivMod10
//
function DivMod10(var dividend : Cardinal) : Cardinal;
{$ifndef WIN32_ASM}
var
   divided : Cardinal;
begin
   divided:=dividend div 10;
   Result:=dividend-divided*10;
   dividend:=divided;
{$else}
const
   c10 : Cardinal = 10;
asm
   mov   ecx, eax

   mov   eax, [eax]
   xor   edx, edx
   div   c10

   mov   [ecx], eax
   mov   eax, edx
{$endif}
end;

// DivMod100
//
function DivMod100(var dividend : Cardinal) : Cardinal;
{$ifndef WIN32_ASM}
var
   divided : Cardinal;
begin
   divided:=dividend div 100;
   Result:=dividend-divided*100;
   dividend:=divided;
{$else}
const
   c100 : Cardinal = 100;
asm
   mov   ecx, eax

   mov   eax, [eax]
   xor   edx, edx
   div   c100

   mov   [ecx], eax
   mov   eax, edx
{$endif}
end;

function EightDigits(i : Cardinal; p : PChar) : Integer;
var
   r : Integer;
begin
   Result:=0;
   Dec(p);
   repeat
      if i<100 then begin
         r:=i;
         i:=0;
      end else begin
         r:=DivMod100(i);
      end;
      if r>=10 then begin
         PTwoChars(p)^:=cTwoDigits[r];
         Dec(p, 2);
         Inc(Result, 2);
      end else begin
         p[1]:=Char(Ord('0')+r);
         if i>0 then begin
            p[0]:='0';
            Dec(p, 2);
            Inc(Result, 2);
         end else begin
            Inc(Result);
            Break;
         end;
      end;
   until i=0;
end;

// FastInt32ToBuffer
//
function FastInt32ToBuffer(const val : Int32; var buf : TInt32StringBuffer) : Integer;
var
   n, nd : Integer;
   neg : Boolean;
   i, next : UInt32;
begin
   if val<0 then begin
      neg:=True;
      i:=-val;
   end else begin
      if val=0 then begin
         Result:=High(buf);
         buf[Result]:='0';
         Exit;
      end else i:=val;
      neg:=False;
   end;
   nd:=High(buf);
   n:=nd;
   while True do begin
      if i>100000000 then begin
         next:=i div 100000000;
         n:=n-EightDigits(i-next*100000000, @buf[n]);
         i:=next;
      end else begin
         n:=n-EightDigits(i, @buf[n]);
         Break;
      end;
      Dec(nd, 8);
      while n>nd do begin
         buf[n]:='0';
         Dec(n);
      end;
   end;
   if neg then
      buf[n]:='-'
   else Inc(n);
   Result:=n;
end;

// FastInt64ToBuffer
//
function FastInt64ToBuffer(const val : Int64; var buf : TInt64StringBuffer) : Integer;
var
   n, nd : Integer;
   neg : Boolean;
   i, next : UInt64;
begin
   if val<0 then begin
      neg:=True;
      i:=-val;
   end else begin
      if val=0 then begin
         Result:=High(buf);
         buf[Result]:='0';
         Exit;
      end else i:=val;
      neg:=False;
   end;
   nd:=High(buf);
   n:=nd;
   while True do begin
      if i>100000000 then begin
         next:=i div 100000000;
         n:=n-EightDigits(i-next*100000000, @buf[n]);
         i:=next;
      end else begin
         n:=n-EightDigits(i, @buf[n]);
         Break;
      end;
      Dec(nd, 8);
      while n>nd do begin
         buf[n]:='0';
         Dec(n);
      end;
   end;
   if neg then
      buf[n]:='-'
   else Inc(n);
   Result:=n;
end;

// InitializeSmallIntegers
//
var
   vSmallIntegers : array [0..39] of String;
procedure InitializeSmallIntegers;
var
   i : Integer;
begin
   // we can't use a constant array here, as obtaining a string from a constant
   // array implies a memory allocations, which would defeat the whole purpose
   for i := 0 to High(vSmallIntegers) do
      vSmallIntegers[i] := IntToStr(i);
end;

// FastInt64ToStr
//
procedure FastInt64ToStr(const val : Int64; var s : String);
var
   buf : TInt64StringBuffer;
   n : Integer;
begin
   if (Int64Rec(val).Hi=0) and (Int64Rec(val).Lo<=High(vSmallIntegers)) then
      s:=vSmallIntegers[Int64Rec(val).Lo]
   else begin
      n:=FastInt64ToBuffer(val, buf{%H-});
      SetString(s, PChar(@buf[n]), (High(buf)+1)-n);
   end;
end;

// FastInt64ToStr
//
function FastInt64ToStr(const val : Int64) : String;
begin
   FastInt64ToStr(val, Result);
end;

// FastInt64ToHex
//
procedure FastInt64ToHex(val : Int64; digits : Integer; var s : String);
const
   cIntToHex : array [0..15] of Char = (
      '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F'
   );
var
   buf : array [0..15] of Char;
   p : PChar;
   d, i : Integer;
begin
   if Cardinal(digits)>16 then begin
      if digits<=0 then
         digits:=1
      else digits:=16;
   end;
   p:=@buf[15];
   if PIntegerArray(@val)[1]=0 then begin
      i:=PIntegerArray(@val)[0];
      repeat
         d:=i and 15;
         i:=i shr 4;
         p^:=cIntToHex[d];
         Dec(p);
         Dec(digits);
      until i=0;
   end else begin
      repeat
         d:=val and 15;
         val:=val shr 4;
         p^:=cIntToHex[d];
         Dec(p);
         Dec(digits);
      until val=0;
   end;
   for i:=1 to digits do begin
      p^:='0';
      Dec(p);
   end;
   SetString(s, PChar(@p[1]), (NativeUInt(@buf[15])-NativeUInt(p)) div SizeOf(Char));
end;

// FastFloatToStr
//
procedure FastFloatToStr(const val : Extended; var s : String; const fmtSettings : TFormatSettings);
var
   buffer : array [0..63] of Char;
   n : Integer;
begin
   if val = 0 then
      s := '0'
   else begin
      n := FloatToText(buffer, val, fvExtended, ffGeneral, 15, 0, fmtSettings);
      SetString(s, buffer, n);
   end;
end;

// Int64ToHex
//
function Int64ToHex(val : Int64; digits : Integer) : String;
begin
   FastInt64ToHex(val, digits, Result);
end;

// TryStrToDouble (string)
//
function TryStrToDouble(const s : String; var val : Double) : Boolean;
begin
   Result := TryStrToDouble(PChar(s), val);
end;

// TryStrToDouble (string, fmt)
//
function TryStrToDouble(const s : String; var val : Double; const formatSettings : TFormatSettings) : Boolean;
begin
   Result := TryStrToDouble(PChar(s), val, @formatSettings);
end;

// TryStrToDouble (PChar)
//
function TryStrToDouble(p : PChar; var val : Double; formatSettings : PFormatSettings = nil) : Boolean;
const
   cMaxExponent = 308;
   cMinExponent = -323;
   cSignificantDigits = 17;

   cExp10table: array [0..31] of Double = (
      1e00, 1e01, 1e02, 1e03, 1e04, 1e05, 1e06, 1e07, 1e08, 1e09,
      1e10, 1e11, 1e12, 1e13, 1e14, 1e15, 1e16, 1e17, 1e18, 1e19,
      1e20, 1e21, 1e22, 1e23, 1e24, 1e25, 1e26, 1e27, 1e28, 1e29,
      1e30, 1e31
    );
   cExp10postable32: array [0..9] of Double = (
      1e00, 1e32, 1e64, 1e96, 1e128, 1e160, 1e192, 1e224, 1e256, 1e288
    );
   cExp10negtable32: array [0..10] of Double = (
      1e-00, 1e-32, 1e-64, 1e-96, 1e-128, 1e-160, 1e-192, 1e-224, 1e-256, 1e-288, 1e-320
    );

var
   intMantissa : Int64;
   mantissaExp, mantissaDigits, exp : Integer;
   sign, expSign, gotDec, gotFrac : Boolean;
   mantissa : Double;
begin
   if p = nil then Exit(False);

   // skip leading whitespace & tabs
   while (p^ = ' ') or (p^ = #9) do
      Inc(p);

   // grab the sign
   case p^ of
      '-' : begin
         sign := True;
         Inc(p);
      end;
      '+' : begin
         sign := False;
         Inc(p);
      end;
   else
      sign := False;
   end;

   mantissaDigits := 0;
   mantissaExp := 0;
   case p^ of
      '0'..'9' : begin
         gotDec := True;
         // skip leading zeroes, needed to figure out mantissa exponent
         while p^ = '0' do
            Inc(p);
         if (p^ >= '0') and (p^ <= '9') then begin
            intMantissa := Ord(p^)-Ord('0');
            Inc(p);
            while (p^ >= '0') and (p^ <= '9') do begin
               if mantissaDigits < cSignificantDigits then begin
                  intMantissa := intMantissa*10 + (Ord(p^)-Ord('0'));
                  Inc(mantissaDigits);
               end else begin
                  Inc(mantissaExp);
               end;
               Inc(p);
            end;
         end else begin
            intMantissa := 0;
         end;
      end;
   else
      gotDec := False;
      intMantissa := 0;
   end;

   if    ((formatSettings = nil) and ((p^ = '.') or (p^ = ',')))
      or ((formatSettings <> nil) and (p^ = formatSettings.DecimalSeparator)) then begin
      Inc(p);
      gotFrac := (p^ >= '0') and (p^ <= '9');
      if (p^ = '0') and (intMantissa = 0) then begin
         while p^ = '0' do begin
            Dec(mantissaExp);
            Inc(p);
         end;
      end;
      while (p^ >= '0') and (p^ <= '9') do begin
         if mantissaDigits < cSignificantDigits then begin
            intMantissa := intMantissa*10 + (Ord(p^)-Ord('0'));
            Inc(mantissaDigits);
            Dec(mantissaExp);
         end;
         Inc(p);
      end;
   end else gotFrac := False;

   // mantissa cannot be empty or a '.'/','
   if not (gotFrac or gotDec) then
      Exit(False);

   // grab exponent, if there is one
   if (p^ = 'e') or (p^ = 'E') then begin
      Inc(p);
      case p^ of
         '-' : begin
            expSign := True;
            Inc(p);
         end;
         '+' : begin
            expSign := False;
            Inc(p);
         end;
      else
         expSign := False;
      end;
      case p[0] of
         '0'..'9' : begin
            while p^ = '0' do
               Inc(p);
            if (p^ >= '0') and (p^ <= '9') then begin
               exp := Ord(p[0]) - Ord('0');
               case p[1] of
                  '0'..'9' : begin
                     exp := exp * 10 + Ord(p[1]) - Ord('0');
                     case p[2] of
                        '0'..'9' : begin
                           exp := exp * 10 + Ord(p[2]) - Ord('0');
                           Inc(p, 3);
                        end
                     else
                        Inc(p, 2);
                     end
                  end
               else
                  Inc(p);
               end
            end else begin
               exp := 0;
            end;
         end
      else
         Exit(False);
      end;
      if expSign then
         exp := mantissaExp - exp
      else exp := exp + mantissaExp;
   end else begin
      exp := mantissaExp;
   end;

   case p^ of
      #0, #9, #13, #10, ' ' : begin
      end;
   else
      Exit(False);
   end;

   case exp of
      cMinExponent .. -32 :
         mantissa := intMantissa * cExp10negtable32[(-exp and not 31) shr 5] / cExp10table[-exp and 31];
      -31 .. -1 :
         mantissa := intMantissa / cExp10table[-exp];
      0 :
         mantissa := intMantissa;
      1 .. 31 : begin
         mantissa := intMantissa;
         mantissa := mantissa * cExp10table[exp];
      end;
      32 .. cMaxExponent : begin
         if mantissaDigits + exp > cMaxExponent then
            Exit(False);
         mantissa := intMantissa * cExp10postable32[(exp and not 31) shr 5] * cExp10table[exp and 31]
      end;
   else
      Exit(False);
   end;

   if sign then begin
      // workaround for compiler bug that implements negation as subtraction in Delphi 64,
      // thus preventing a negative zero (cf. https://en.wikipedia.org/wiki/Signed_zero)
      PByteArray(@mantissa)[7] := PByteArray(@mantissa)[7] xor $80;
   end;

   val := mantissa;
   Result := True;
end;

// StringToBoolean
//
function StringToBoolean(const s : String) : Boolean;
var
   p : PChar;
begin
   if s = '' then Exit(False);
   p := Pointer(s);
   case Length(s) of
      1 : case p[0] of
         '1', 'T', 't', 'Y', 'y' : Exit(True);
      end;
      3 : begin
         if     ((p[0] = 'Y') or (p[0] = 'y'))
            and ((p[1] = 'E') or (p[1] = 'e'))
            and ((p[2] = 'S') or (p[2] = 's')) then Exit(True);
      end;
      4 : begin
         if     ((p[0] = 'T') or (p[0] = 't'))
            and ((p[1] = 'R') or (p[1] = 'r'))
            and ((p[2] = 'U') or (p[2] = 'u'))
            and ((p[3] = 'E') or (p[3] = 'e')) then Exit(True);
      end;
   end;
   // special case of integer: zero is false, everything else is true
   Result := False;
   case p^ of
      '0'..'9' : begin
         Inc(p);
         repeat
            case p^ of
               '0' :  Inc(p);
               '1'..'9' : begin
                  Inc(p);
                  Result := True;
               end;
               #0 : Exit;
            else
               Exit(False);
            end;
         until False;
      end;
   end;
end;

// Int32ToStrU
//
function Int32ToStrU(val : Integer) : UnicodeString;
var
   buf : TInt32StringBuffer;
   n : Integer;
begin
   n := FastInt32ToBuffer(val, buf);
   SetString(Result, PChar(@buf[n]), (High(buf)+1)-n);
end;

// StrUToInt64
//
function StrUToInt64(const s : UnicodeString; const default : Int64) : Int64;
begin
   Result := StrToInt64Def(String(s), default);
end;

// TryStrToIntBase
//
function TryStrToIntBase(const s : UnicodeString; base : Integer; var value : Int64) : Boolean;
var
   p : PChar;
   neg : Boolean;
   d : Integer;
   temp : Int64;
begin
   Result := False;
   if (base < 2) or (base > 36) then
      raise EConvertError.CreateFmt('Invalid base for string to integer conversion (%d)', [ base ]);
   if s = '' then Exit;

   p := Pointer(s);

   try
      neg := False;
      case p^ of
         '+' : Inc(p);
         '-' : begin
            neg := True;
            Inc(p);
         end;
         '0' : begin
            while p[1] = '0' do
               Inc(p);
         end;
      end;

      temp := 0;

      while True do begin
         case p^ of
            #0 : Break;
            '0'..'9' : d := Ord(p^) - Ord('0');
            'a'..'z' : d := Ord(p^) + (10 - Ord('a'));
            'A'..'Z' : d := Ord(p^) + (10 - Ord('A'));
         else
            Exit;
         end;
         if d >= base then
            Exit;
{$Q+}
         temp := temp * base + d;
{$Q-}
         Inc(p);
      end;
      if neg then
         value := -temp
      else value := temp;
      Result := True;
   except
      on E: EIntOverflow do
         Exit(False);
      else raise;
   end;
end;

// Int64ToStrBase
//
function Int64ToStrBase(val : Int64; base : Integer) : String;
var
   uv : UInt64;
   buf : array [0..64] of Char;
   p, digit : Integer;
   neg : Boolean;
begin
   if (base < 2) or (base > 36) then
      raise EConvertError.CreateFmt('Invalid base for integer to string conversion (%d)', [ base ]);

   if val = 0 then Exit('0');

   neg := (val < 0);
   if neg then
      uv := -val
   else uv := val;
   p := High(buf);

   while uv <> 0 do begin
      digit := uv mod Cardinal(base);
      uv := uv div Cardinal(base);
      if digit < 10 then
         buf[p] := Char(Ord('0') + digit)
      else buf[p] := Char((Ord('a') - 10) + digit);
      Dec(p);
   end;

   if neg then begin
      buf[p] := '-';
      Dec(p);
   end;

   SetString(Result, PChar(@buf[p+1]), High(buf)-p);
end;

// FastStringReplace
//
procedure FastStringReplace(var str : UnicodeString; const sub, newSub : UnicodeString);

   procedure FallBack;
   begin
      str := StringReplace(str, sub, newSub, [rfReplaceAll]);
   end;

   procedure ReplaceChars(pStr : PWideChar; oldChar, newChar : WideChar; n : Integer);
   begin
      pStr^:=newChar;
      for n:=1 to n do begin
         if pStr[n]=oldChar then
            pStr[n]:=newChar;
      end;
   end;

var
   p, dp, np : Integer;
   subLen, newSubLen : Integer;
   pStr, pNewSub : PWideChar;
begin
   if (str='') or (sub='') then Exit;

   p:=Pos(sub, str);
   if p<=0 then Exit;

   subLen:=Length(sub);
   newSubLen:=Length(newSub);

   pNewSub:=PWideChar(newSub);

   if subLen=newSubLen then begin

      // same length, replace in-place
      UniqueString(str);
      pStr:=PWideChar(Pointer(str));

      if subLen=1 then begin

         // special case of character replacement
         ReplaceChars(@pStr[p-1], sub[1], pNewSub^, Length(str)-p);

      end else begin

         repeat
            System.Move(pNewSub^, pStr[p-1], subLen*SizeOf(WideChar));
            p := Pos(sub, str, p+subLen);
         until p <= 0;

      end;

   end else if newSubLen<subLen then begin

      // shorter replacement, replace & pack in-place
      UniqueString(str);
      pStr:=PWideChar(Pointer(str));

      dp:=p-1;
      while True do begin
         if newSubLen>0 then begin
            System.Move(pNewSub^, pStr[dp], newSubLen*SizeOf(WideChar));
            dp:=dp+newSubLen;
         end;
         p:=p+subLen;
         np := Pos(sub, str, p);
         if np>0 then begin
            if np>p then begin
               System.Move(pStr[p-1], pStr[dp], (np-p)*SizeOf(WideChar));
               dp:=dp+np-p;
            end;
            p:=np;
         end else begin
            np:=Length(str)+1-p;
            if np>0 then
               System.Move(pStr[p-1], pStr[dp], np*SizeOf(WideChar));
            SetLength(str, dp+np);
            Break;
         end;
      end;

   end else begin

      // growth required (not optimized yet, todo)
      FallBack;

   end;
end;
{$ifdef FPC}
procedure FastStringReplace(var str : String; const sub, newSub : String);
var
   buf : String;
begin
   buf := StringReplace(str, sub, newSub, [rfReplaceAll]);
   str := buf;
end;
{$endif}

// VariantToString
//
procedure VariantToString(const v : Variant; var s : String);

   procedure UnknownAsString(const unknown : IUnknown; var result : String);
   var
      intf : IGetSelf;
   begin
      if unknown=nil then
         Result:='nil'
      else if unknown.QueryInterface(IGetSelf, intf)=0 then
         Result := intf.ToString
      else Result := '[IUnknown]';
   end;

   procedure FloatAsString(var v : Double; var Result : String);
   begin
      Result := FloatToStr(v);
   end;

   procedure VariantArrayToString(const v : Variant; var Result : String);
   var
      i : Integer;
   begin
      Result := '';
      for i := VarArrayLowBound(v, 1) to VarArrayHighBound(v, 1) do begin
         if Result <> '' then
            Result := Result + ', ';
         Result := Result + VariantToString(v[i]);
      end;
      Result := '[ ' + Result + ' ]';
   end;

var
   varData : PVarData;
begin
   varData:=PVarData(@v);
   case varData^.VType of
      {$ifdef FPC}
      varString :
         s := String(varData^.VString);
      {$else}
      varString :
         RawByteStringToScriptString(RawByteString(varData^.VString), s);
      varUString :
         s:=String(varData^.VUString);
      {$endif}
      varInt64 :
         FastInt64ToStr(varData^.VInt64, s);
      varDouble :
         FloatAsString(varData^.VDouble, s);
      varBoolean :
         if varData^.VBoolean then
            s := 'True'
         else s := 'False';
      varNull :
         s := vValueOfNullVariantAsString;
      varUnknown :
         UnknownAsString(IUnknown(varData^.VUnknown), s);
      varError :
         s := '[varError]';
   else
      if VarIsArray(v) then
         VariantArrayToString(v, s)
      else s := v;
   end;
end;

{$ifdef FPC}
// VariantToString
//
procedure VariantToString(const v : Variant; var s : UnicodeString);
begin
   s := UnicodeString(VariantToString(v));
end;
{$endif FPC}

// VariantToString
//
function VariantToString(const v : Variant) : String;
begin
   VariantToString(v, Result);
end;

// VariantToString
//
function VariantToUnicodeString(const v : Variant) : UnicodeString;
begin
   Result := UnicodeString(VariantToString(v));
end;

// VariantToInt64
//
procedure VariantToInt64(const v : Variant; var r : Int64);
begin
   r := VariantToInt64(v);
end;

// VariantToInt64
//
function VariantToInt64(const v : Variant) : Int64;

   function UnknownAsInteger(const unknown : IUnknown) : Int64;
   var
      intf : IToNumeric;
   begin
      if unknown = nil then
         Result := 0
      else if unknown.QueryInterface(IToNumeric, intf)=0 then
         Result := intf.ToInteger
      else raise EVariantTypeCastError.CreateFmt(RTE_VariantCastFailed, [ 'IUnknown', SYS_INTEGER ]);
   end;

   function StringToInt64(const s : String) : Int64;
   begin
      if not TryStrToInt64(s, Result) then begin
         raise EVariantTypeCastError.CreateFmt(RTE_VariantCastFailed, [ SYS_STRING, SYS_INTEGER ]);
      end;
   end;

   function DefaultCast(const v : Variant) : Int64;
   begin
      try
         Result := v;
      except
         // workaround for RTL bug that will sometimes report a failed cast to Int64
         // as being a failed cast to Boolean
         on E : EVariantError do begin
            raise EVariantTypeCastError.CreateFmt(
               RTE_VariantVTCastFailed,
               [ VarType(v), SYS_INTEGER ]
            );
         end else raise;
      end;
   end;

begin
   case TVarData(v).VType of
      varInt64 :
         Result := TVarData(v).VInt64;
      varBoolean :
         Result := Ord(Boolean(TVarData(v).VBoolean));
      varUnknown :
         Result := UnknownAsInteger(IUnknown(TVarData(v).VUnknown));
      varUString :
         Result := StringToInt64(String(TVarData(v).VString));
      varNull :
         Result := 0;
   else
      Result := DefaultCast(v);
   end;
end;

// VariantToBool
//
function VariantToBool(const v : Variant) : Boolean;
begin
   case TVarData(v).VType of
      varBoolean :
         Result := TVarData(v).VBoolean;
      varInt64 :
         Result := (TVarData(v).VInt64<>0);
      varUnknown :
         Result := not CoalesceableIsFalsey(IUnknown(TVarData(v).VUnknown));
      {$ifdef FPC}
      varString :
         Result := StringToBoolean(String(TVarData(v).VString));
      {$else}
      varUString :
         Result := StringToBoolean(String(TVarData(v).VUString));
      {$endif}
      varDouble :
         Result := TVarData(v).VDouble <> 0;
      varNull, varEmpty :
         Result := False;
   else
      Result := v;
   end;
end;

// VariantToFloat
//
function VariantToFloat(const v : Variant) : Double;

   procedure UnknownAsFloat(const unknown : IUnknown; var Result : Double);
   var
      intf : IToNumeric;
   begin
      if unknown = nil then
         Result := 0
      else if unknown.QueryInterface(IToNumeric, intf)=0 then
         Result := intf.ToFloat
      else Result := Variant(unknown);// raise EVariantTypeCastError.CreateFmt(RTE_VariantCastFailed, [ 'IUnknown', SYS_FLOAT]);
   end;

begin
   case TVarData(v).VType of
      varDouble :
         Result := TVarData(v).VDouble;
      varInt64 :
         Result := TVarData(v).VInt64;
      varBoolean :
         Result := Ord(Boolean(TVarData(v).VBoolean));
      varNull :
         Result := 0;
      varUnknown :
         UnknownAsFloat(IUnknown(TVarData(v).VUnknown), Result);
      varUString :
         if not TryStrToDouble(PChar(TVarData(v).VUString), Result) then
            raise EConvertError.CreateFmt(CPE_InvalidFloatFormat, [ String(TVarData(v).VUString) ]);
   else
      Result := v;
   end;
end;

// VariantType
//
function  VariantType(const v : Variant) : TVarType; inline;
begin
   Result := TVarData(v).VType;
end;

// VariantIsOrdinal
//
function  VariantIsOrdinal(const v : Variant) : Boolean; inline;
begin
   Result := TVarData(v).VType in [
      varSmallInt, varInteger, varShortInt, varByte, varWord, varLongWord, varInt64, varUInt64,
      varBoolean
      ];
end;

// VariantIsFloat
//
function  VariantIsFloat(const v : Variant) : Boolean; inline;
begin
   Result := VarIsFloat(v);
end;

// VariantIsString
//
function  VariantIsString(const v : Variant) : Boolean; inline;
begin
   Result := VarIsStr(v);
end;

// VariantIsArray
//
function  VariantIsArray(const v : Variant) : Boolean; inline;
begin
   Result := VarIsArray(v, False);
end;

// VarClearSafe
//
procedure VarClearSafe(var v : Variant);
// This procedure exists because of a bug in Variants / Windows where if you clear
// a varUnknown, the _Release method will be called before the variable is niled
// So if _Release's code assigns a nil to the same variable, it will result in
// an invalid refcount. _IntfClear does not suffer from that issue
begin
   case TVarData(v).VType of
      varEmpty : begin
         TVarData(v).VInt64:=0;
      end;
      varBoolean, varInt64, varDouble : begin
         TVarData(v).VType:=varEmpty;
         TVarData(v).VInt64:=0;
      end;
      varUnknown : begin
         TVarData(v).VType:=varEmpty;
         IUnknown(TVarData(v).VUnknown):=nil;
      end;
      varDispatch : begin
         TVarData(v).VType:=varEmpty;
         IDispatch(TVarData(v).VDispatch):=nil;
      end;
      {$ifndef FPC}
      varUString : begin
         TVarData(v).VType:=varEmpty;
         String(TVarData(v).VUString):='';
      end;
      {$endif}
      varString : begin
         TVarData(v).VType:=varEmpty;
         AnsiString(TVarData(v).VString):='';
      end;
      varRecord : begin
         TVarData(v).VType := varEmpty;
         TVarData(v).VInt64 := 0;
      end;
   else
      VarClear(v);
      TVarData(v).VInt64:=0;
   end;
end;

// VarCopySafe (variant)
//
procedure VarCopySafe(var dest : Variant; const src : Variant);
begin
   if @dest=@src then Exit;

   VarClearSafe(dest);

   case TVarData(src).VType of
      varEmpty : ;
      varNull : TVarData(dest).VType:=varNull;
      varBoolean : begin
         TVarData(dest).VType:=varBoolean;
         TVarData(dest).VBoolean:=TVarData(src).VBoolean;
      end;
      varInt64 : begin
         TVarData(dest).VType:=varInt64;
         TVarData(dest).VInt64:=TVarData(src).VInt64;
      end;
      varDouble : begin
         TVarData(dest).VType:=varDouble;
         TVarData(dest).VDouble:=TVarData(src).VDouble;
      end;
      varUnknown : begin
         {$ifdef DEBUG} Assert(TVarData(dest).VUnknown=nil); {$endif}
         TVarData(dest).VType:=varUnknown;
         IUnknown(TVarData(dest).VUnknown):=IUnknown(TVarData(src).VUnknown);
      end;
      varDispatch : begin
         {$ifdef DEBUG} Assert(TVarData(dest).VDispatch=nil); {$endif}
         TVarData(dest).VType:=varDispatch;
         IDispatch(TVarData(dest).VDispatch):=IDispatch(TVarData(src).VDispatch);
      end;
      {$ifndef FPC}
      varUString : begin
         {$ifdef DEBUG} Assert(TVarData(dest).VUString=nil); {$endif}
         TVarData(dest).VType := varUString;
         String(TVarData(dest).VString) := String(TVarData(src).VString);
      end;
      {$endif}
      varString : begin
         {$ifdef DEBUG} Assert(TVarData(dest).VString=nil); {$endif}
         TVarData(dest).VType := varString;
         AnsiString(TVarData(dest).VString) := AnsiString(TVarData(src).VString);
      end;
      varSmallint..varSingle, varCurrency..varDate, varError, varShortInt..varLongWord, varUInt64 : begin
         TVarData(dest).VType := TVarData(src).VType;
         TVarData(dest).VLongs[0]:=TVarData(src).VLongs[0];
         TVarData(dest).VLongs[1]:=TVarData(src).VLongs[1];
         TVarData(dest).VLongs[2]:=TVarData(src).VLongs[2];
      end;
   else
      dest:=src;
   end;
end;

// VarCompareSafe
//
function VarCompareSafe(const left, right : Variant) : TVariantRelationship;

   function CompareStrings(const left, right : String) : TVariantRelationship;
   var
      c : Integer;
   begin
      c := CompareStr(left, right);
      if c < 0 then
         Result := vrLessThan
      else if c = 0 then
         Result := vrEqual
      else Result := vrGreaterThan;
   end;

   function CompareDoubles(const left, right : Double) : TVariantRelationship;
   begin
      if left < right then
         Result := vrLessThan
      else if left > right then
         Result := vrGreaterThan
      else if left = right then
         Result := vrEqual
      else Result := vrNotEqual;
   end;

   function CompareInt64s(const left, right : Int64) : TVariantRelationship;
   begin
      if left < right then
         Result := vrLessThan
      else if left > right then
         Result := vrGreaterThan
      else Result := vrEqual
   end;

   function CompareDoubleToString(const left : Double; const right : String) : TVariantRelationship;
   var
      rv : Double;
   begin
      if TryStrToDouble(right, rv) then
         Result := CompareDoubles(left, rv)
      else Result := vrNotEqual;
   end;

   function CompareStringToDouble(const left : String; const right : Double) : TVariantRelationship;
   var
      lv : Double;
   begin
      if TryStrToDouble(left, lv) then
         Result := CompareDoubles(lv, right)
      else Result := vrNotEqual;
   end;

   function CompareInt64ToString(const left : Int64; const right : String) : TVariantRelationship;
   var
      rv : Int64;
   begin
      if TryStrToInt64(right, rv) then
         Result := CompareInt64s(left, rv)
      else Result := CompareDoubleToString(left, right);
   end;

   function CompareStringToInt64(const left : String; const right : Int64) : TVariantRelationship;
   var
      lv : Int64;
   begin
      if TryStrToInt64(left, lv) then
         Result := CompareInt64s(lv, right)
      else Result := CompareStringToDouble(left, right);
   end;

   function CompareUnknowns(const left, right : IUnknown) : TVariantRelationship;
   var
      intfLeft, intfRight : IToVariant;
      varLeft, varRight : Variant;
   begin
      if left = right then
         Result := vrEqual
      else if     (left <> nil)  and (right <> nil)
              and (left.QueryInterface(IToVariant, intfLeft)=0) and (right.QueryInterface(IToVariant, intfRight)=0) then begin
         intfLeft.ToVariant(varLeft);
         intfRight.ToVariant(varRight);
         Result := VarCompareSafe(varLeft, varRight);
      end else Result := vrNotEqual;
   end;

   function CompareUnknownToVar(const left : IUnknown; const right : Variant) : TVariantRelationship;
   var
      intfLeft : IToVariant;
      varLeft : Variant;
   begin
      if (left <> nil) and (left.QueryInterface(IToVariant, intfLeft)=0) then begin
         intfLeft.ToVariant(varLeft);
         Result := VarCompareSafe(varLeft, right);
      end else Result := vrNotEqual;
   end;

   function CompareInt64ToUnknown(const left : Int64; const right : IUnknown) : TVariantRelationship; forward;

   function CompareInt64ToVar(const left : Int64; const right : Variant) : TVariantRelationship;
   begin
      case VarType(right) of
         varDouble : Exit(CompareDoubles(left, TVarData(right).VDouble));
         varInt64 : Exit(CompareInt64s(left, TVarData(right).VInt64));
         varUString : Exit(CompareInt64ToString(left, String(TVarData(right).VUString)));
         varUnknown : Exit(CompareInt64ToUnknown(left, IUnknown(TVarData(right).VUnknown)));
      end;
      if VarIsArray(right) then
         Exit(vrNotEqual)
      else Exit(VarCompareValue(left, right));
   end;

   function CompareInt64ToUnknown(const left : Int64; const right : IUnknown) : TVariantRelationship;
   var
      intfRight : IToVariant;
      varRight : Variant;
   begin
      if (right <> nil) and (right.QueryInterface(IToVariant, intfRight)=0) then begin
         intfRight.ToVariant(varRight);
         Result := CompareInt64ToVar(left, varRight);
      end else Result := vrNotEqual;
   end;

   function CompareVarToUnknown(const left : Variant; const right : IUnknown) : TVariantRelationship;
   var
      intfRight : IToVariant;
      varRight : Variant;
   begin
      if (right <> nil) and (right.QueryInterface(IToVariant, intfRight)=0) then begin
         intfRight.ToVariant(varRight);
         Result := VarCompareSafe(left, varRight);
      end else Result := vrNotEqual;
   end;

begin
   case VarType(left) of
      varUnknown : begin
         if VarType(right) = varUnknown then
            Result := CompareUnknowns(IUnknown(TVarData(left).VUnknown), IUnknown(TVarData(right).VUnknown))
         else Result := CompareUnknownToVar(IUnknown(TVarData(left).VUnknown), right);
         Exit;
      end;
      varEmpty : begin
         case VarType(right) of
            varInt64 : if TVarData(right).VInt64 = 0 then Exit(vrEqual) else Exit(vrNotEqual);
            varUString : if TVarData(right).VUString = nil then Exit(vrEqual) else Exit(vrNotEqual);
            varDouble : if TVarData(right).VDouble = 0 then Exit(vrEqual) else Exit(vrNotEqual);
            varEmpty, varNull : Exit(vrEqual);
         end;
      end;
      varDouble : begin
         case VarType(right) of
            varDouble : Exit(CompareDoubles(TVarData(left).VDouble, TVarData(right).VDouble));
            varInt64 : Exit(CompareDoubles(TVarData(left).VDouble, TVarData(right).VInt64));
            varUString : Exit(CompareDoubleToString(TVarData(left).VDouble, String(TVarData(right).VUString)));
            varDate : Exit(CompareDoubles(TVarData(left).VDouble, TVarData(right).VDate));
         end;
         if VarIsArray(right) then
            Exit(vrNotEqual)
         else Exit(VarCompareValue(left, right));
      end;
      varInt64 :
         Exit(CompareInt64ToVar(TVarData(left).VInt64, right));
      varUString : begin
         case VarType(right) of
            varDouble : Exit(CompareStringToDouble(String(TVarData(left).VUString), TVarData(right).VDouble));
            varInt64 : Exit(CompareStringToInt64(String(TVarData(left).VUString), TVarData(right).VInt64));
            varUString : Exit(CompareStrings(String(TVarData(left).VUString), String(TVarData(right).VUString)));
            varDate : Exit(CompareStringToDouble(String(TVarData(left).VUString), TVarData(right).VDate));
         end;
         if VarIsArray(right) then
            Exit(vrNotEqual)
         else Exit(VarCompareValue(left, right));
      end;
      varDate : begin
         case VarType(right) of
            varDouble : Exit(CompareDoubles(TVarData(left).VDate, TVarData(right).VDouble));
            varInt64 : Exit(CompareDoubles(TVarData(left).VDate, TVarData(right).VInt64));
            varUString : Exit(CompareDoubleToString(TVarData(left).VDate, String(TVarData(right).VUString)));
         else
            if VarIsArray(right) then
               Exit(vrNotEqual)
            else Exit(VarCompareValue(left, right));
         end;
      end;
   else
      if VarIsArray(left) then begin
         if VarIsArray(right) then
            Exit(VarCompareValue(left, right))
         else Exit(vrNotEqual);
      end;
   end;
   case VarType(right) of
      varUnknown :
         Result := CompareVarToUnknown(left, IUnknown(TVarData(right).VUnknown));
   else
      if VarIsArray(right) then
         Result := vrNotEqual
      else Result := VarCompareValue(left, right);
   end;
end;

// VarSetNull
//
procedure VarSetNull(var v : Variant);
begin
   VarClearSafe(v);
   TVarData(v).VType := varNull;
end;

// VarCopySafe (iunknown)
//
procedure VarCopySafe(var dest : Variant; const src : IUnknown);
begin
   VarClearSafe(dest);

   TVarData(dest).VType:=varUnknown;
   IUnknown(TVarData(dest).VUnknown):=src;
end;

// VarCopySafe (idispatch)
//
procedure VarCopySafe(var dest : Variant; const src : IDispatch);
begin
   VarClearSafe(dest);

   TVarData(dest).VType:=varDispatch;
   IDispatch(TVarData(dest).VDispatch):=src;
end;

// VarCopySafe (int64)
//
procedure VarCopySafe(var dest : Variant; const src : Int64);
begin
   VarClearSafe(dest);

   TVarData(dest).VType:=varInt64;
   TVarData(dest).VInt64:=src;
end;

// VarCopySafe (string)
//
procedure VarCopySafe(var dest : Variant; const src : String);
begin
   VarClearSafe(dest);

   {$ifdef FPC}
   TVarData(dest).VType := varString;
   String(TVarData(dest).VString) := src;
   {$else}
   TVarData(dest).VType := varUString;
   String(TVarData(dest).VUString) := src;
   {$endif}
end;

{$ifdef FPC}
// VarCopySafe (unicode)
//
procedure VarCopySafe(var dest : Variant; const src : UnicodeString);
begin
   VarCopySafe(dest, String(src));
end;
{$endif}

// VarCopySafe (double)
//
procedure VarCopySafe(var dest : Variant; const src : Double);
begin
   VarClearSafe(dest);

   TVarData(dest).VType:=varDouble;
   TVarData(dest).VDouble:=src;
end;

// VarCopySafe (bool)
//
procedure VarCopySafe(var dest : Variant; const src : Boolean);
begin
   VarClearSafe(dest);

   TVarData(dest).VType:=varBoolean;
   TVarData(dest).VBoolean:=src;
end;

// VarSetDefaultInt64
//
procedure VarSetDefaultInt64(var dest : Variant);
begin
   VarClearSafe(dest);

   TVarData(dest).VType := varInt64;
end;

// VarSetDefaultDouble
//
procedure VarSetDefaultDouble(var dest : Variant); inline;
begin
   VarClearSafe(dest);

   TVarData(dest).VType := varDouble;
end;

// VarSetDefaultString
//
procedure VarSetDefaultString(var dest : Variant);
begin
   VarClearSafe(dest);

   {$ifdef FPC}
   TVarData(dest).VType := varString;
   {$else}
   TVarData(dest).VType := varUString;
   {$endif}
end;

// VarSetDateTime
//
procedure VarSetDateTime(var dest : Variant; const dt : Double);
begin
   VarClearSafe(dest);
   TVarData(dest).VType := varDate;
   TVarData(dest).VDate := dt;
end;

// VariantArrayHighBound
//
function  VariantArrayHighBound(const v : Variant; index : Integer) : Integer;
begin
   Result := VarArrayHighBound(v, index);
end;

// WriteVariant
//
procedure WriteVariant(writer: TWriter; const value: Variant);

   procedure WriteValue(const value: TValueType);
   begin
      writer.Write(value, SizeOf(value));
   end;

begin
   case VarType(Value) of
      varInt64 :
         writer.WriteInteger(PVarData(@value).VInt64);
      {$ifndef FPC}
      varUString :
         writer.WriteString(String(PVarData(@value).VUString));
      {$endif}
      varDouble :
         writer.WriteFloat(PVarData(@value).VDouble);
      varBoolean :
         writer.WriteBoolean(PVarData(@value).VBoolean);
      varEmpty :
         WriteValue(vaNil);
      varNull :
         WriteValue(vaNull);
      varByte, varSmallInt, varInteger :
         writer.WriteInteger(value);
      varString :
         writer.WriteString(value);
      varOleStr :
         {$ifdef DELPHI_TOKYO_PLUS}
         writer.WriteString(value);
         {$else}
         writer.WriteWideString(value);
         {$endif}
      varSingle :
         writer.WriteSingle(value);
      varCurrency :
         writer.WriteCurrency(value);
      varDate :
         writer.WriteDate(value);
   else
      try
         writer.WriteString(Value);
      except
         raise EWriteError.Create('Streaming not supported');
      end;
   end;
end;

// ReadVariant
//
function ReadVariant(reader: TReader): Variant;

  function ReadValue: TValueType;
  begin
    reader.Read(Result, SizeOf(Result));
  end;

const
   {$ifdef FPC}
   cValTtoVarT: array[TValueType] of Integer = (
      varNull, varError, varByte, varSmallInt, varInteger, varDouble,
      varString, varError, varBoolean, varBoolean, varError, varError, varString,
      varEmpty, varError, varSingle, varCurrency, varDate, varOleStr,
      varUInt64, varString, varDouble{$ifdef FPC}, varQWord{$endif}
    );
   {$else}
   cValTtoVarT: array[TValueType] of Integer = (
      varNull, varError, varByte, varSmallInt, varInteger, varDouble,
      varUString, varError, varBoolean, varBoolean, varError, varError, varUString,
      varEmpty, varError, varSingle, varCurrency, varDate, varOleStr,
      varUInt64, varUString, varDouble{$ifdef FPC}, varQWord{$endif}
    );
   {$endif}

var
   valType: TValueType;
begin
   valType := reader.NextValue;
   case valType of
      vaNil, vaNull : begin
         if ReadValue = vaNil then
            VarClearSafe(Result)
         else Result := Null;
      end;
      vaInt8: TVarData(Result).VByte := Byte(reader.ReadInteger);
      vaInt16: TVarData(Result).VSmallint := Smallint(reader.ReadInteger);
      vaInt32: TVarData(Result).VInteger := reader.ReadInteger;
      vaInt64: TVarData(Result).VInt64 := reader.ReadInt64;
      vaExtended: TVarData(Result).VDouble := reader.ReadFloat;
      vaSingle: TVarData(Result).VSingle := reader.ReadSingle;
      vaCurrency: TVarData(Result).VCurrency := reader.ReadCurrency;
      vaDate: TVarData(Result).VDate := reader.ReadDate;
      vaString, vaLString, vaUTF8String:
         Result := String(reader.ReadString);
      vaWString: Result := reader.ReadString;
      vaFalse, vaTrue:
         TVarData(Result).VBoolean := (reader.ReadValue = vaTrue);
   else
      raise EReadError.Create('Invalid variant stream');
   end;
   TVarData(Result).VType := cValTtoVarT[ValType];
end;

// DateTimeToISO8601
//
function DateTimeToISO8601(dt : TDateTime; extendedFormat : Boolean; prec : TISO8601Precision = iso8601precAuto) : String;
var
   buf : array [0..35] of Char;
   p : PChar;

   procedure WriteChar(c : Char);
   begin
      p^:=c;
      Inc(p);
   end;

   procedure Write2Digits(v : Integer);
   begin
      PTwoChars(p)^:=cTwoDigits[v];
      Inc(p, 2);
   end;

   procedure Write3Digits(v : Integer);
   begin
      PChar(p)^ := Chr(Ord('0') + (v div 100));
      Inc(p);
      PTwoChars(p)^ := cTwoDigits[v mod 100];
      Inc(p, 2);
   end;

var
   y, m, d, h, n, s, z : Word;
begin
   p:=@buf;
   DecodeDate(dt, y, m, d);
   Write2Digits((y div 100) mod 100);
   Write2Digits(y mod 100);
   if extendedFormat then
      WriteChar('-');
   Write2Digits(m);
   if extendedFormat then
      WriteChar('-');
   Write2Digits(d);
   DecodeTime(dt, h, n, s, z);

   WriteChar('T');
   Write2Digits(h);
   if extendedFormat then
      WriteChar(':');
   Write2Digits(n);
   if (s <> 0) or (prec in [ iso8601precSeconds, iso8601precMilliseconds ]) then begin
      if extendedFormat then
         WriteChar(':');
      Write2Digits(s);
   end;
   if prec = iso8601precMilliseconds then begin
      WriteChar('.');
      Write3Digits(z);
   end;
   WriteChar('Z');

   p^:=#0;
   Result:=buf;
end;

// ParseDateTimeISO8601
//
procedure ParseDateTimeISO8601(const v : String ; var dt, utcOffset : TDateTime);
var
   p : PChar;

   function ReadDigit : Integer;
   var
      c : Char;
   begin
      c := p^;
      case c of
         '0'..'9' : Result:=Ord(c)-Ord('0');
      else
         raise EISO8601Exception.CreateFmt('Unexpected character (%d) instead of digit', [Ord(c)]);
      end;
      Inc(p);
   end;

   function Read2Digits : Integer; inline;
   begin
      Result := ReadDigit*10;
      Result := Result + ReadDigit;
   end;

   function Read4Digits : Integer; inline;
   begin
      Result := Read2Digits*100;
      Result := Result + Read2Digits;
   end;

   function ReadUpTo6Digits : Integer;
   begin
      var nbDigits := 1;
      Result := ReadDigit;
      repeat
         case p^ of
            '0'..'9' : begin
               Inc(nbDigits);
               Result := Result * 10 + ReadDigit;
            end;
         else
            Break;
         end;
      until nbDigits >= 6;
      case nbDigits of
         1 : Result := Result * 100;
         2 : Result := Result * 10;
         3 : ;
         4 : Result := Result div 10;
         5 : Result := Result div 100;
         6 : Result := Result div 1000;
      end;
   end;

var
   y, m, d, h, n, s, z : Integer;
   separator : Boolean;
begin
   dt := 0;
   utcOffset := 1;

   if v = '' then Exit;
   p := Pointer(v);

   y := Read4Digits;
   separator := (p^ = '-');
   if separator then
      Inc(p);
   m := Read2Digits;
   if separator then begin
      if p^ <> '-' then
         raise EISO8601Exception.Create('"-" expected after month');
      Inc(p);
   end;
   d := Read2Digits;
   try
      dt := EncodeDate(y, m, d);
   except
      on E: Exception do
         raise EISO8601Exception.Create(E.Message);
   end;

   case p^ of
      #0 : Exit;
      'T', 't', ' ' : Inc(p);
   else
      raise EISO8601Exception.Create('"T" expected after date');
   end;

   h := Read2Digits;
   separator := (p^ = ':');
   if separator then
      Inc(p);
   n := Read2Digits;
   case p^ of
      ':', '0'..'9' : begin
         if separator then begin
            if (p^ <> ':') then
               raise EISO8601Exception.Create('":" expected after minutes');
            Inc(p);
         end else if (p^ = ':') then
            raise EISO8601Exception.Create('Unexpected ":" after minutes');
         s := Read2Digits;
         if p^ = '.' then begin
            Inc(p);
            z := ReadUpTo6Digits;
         end else z := 0;
      end;
   else
      s := 0;
      z := 0;
   end;

   dt := dt + EncodeTime(h, n, s, z);

   case p^ of
      #0 : exit;
      'Z', 'z' : Inc(p);
      '+', '-' : begin
         if p^ = '-' then
            utcOffset := -1;
         Inc(p);
         h := Read2Digits;
         case p^ of
            ':', '0'..'9' : begin
               if separator then begin
                  if (p^ <> ':') then
                     raise EISO8601Exception.Create('":" expected after offset hours');
                  Inc(p);
               end;
               n := Read2Digits;
            end;
         else
            n := 0;
         end;
         utcOffset := utcOffset * EncodeTime(h, n, 0, 0);
      end;
   end;

   if p^ <> #0 then
      raise EISO8601Exception.Create('Unsupported or invalid ISO8601 format');
end;

// ISO8601ToDateTime
//
function ISO8601ToDateTime(const v : String) : TDateTime;
var
   utcOffset : TDateTime;
begin
   ParseDateTimeISO8601(v, Result, utcOffset);
   if utcOffset <> 1 then
      Result := Result - utcOffset;
end;

// TryISO8601ToDateTime
//
function TryISO8601ToDateTime(const v : String; var aResult : TDateTime) : Boolean;
begin
   try
      aResult:=ISO8601ToDateTime(v);
      Result:=True;
   except
      on E : EISO8601Exception do
         Result:=False;
      else raise
   end;
end;

// RawByteStringToScriptString
//
function RawByteStringToScriptString(const s : RawByteString) : UnicodeString;
begin
   RawByteStringToScriptString(s, Result);
end;

// RawByteStringToScriptString
//
procedure RawByteStringToScriptString(const s : RawByteString; var result : UnicodeString); overload;
begin
   if s = '' then
      result := ''
   else BytesToScriptString(Pointer(s), Length(s), result)
end;

// RawByteStringToScriptString
//
procedure RawByteStringToScriptString(const s : RawByteString; var result : Variant); overload;
begin
   if s = '' then
      VarSetDefaultString(result)
   else begin
      VarClearSafe(result);
      TVarData(result).VType := varUString;
      RawByteStringToScriptString(s, String(TVarData(result).VString));
   end;
end;

// ScriptStringToRawByteString
//
function ScriptStringToRawByteString(const s : UnicodeString) : RawByteString;
begin
   ScriptStringToRawByteString(s, Result);
end;

// ScriptStringToRawByteString
//
procedure ScriptStringToRawByteString(const s : UnicodeString; var result : RawByteString); overload;
var
   n : Integer;
begin
   if s = '' then
      result := ''
   else begin
      n := Length(s);
      SetLength(Result, n);
      WordsToBytes(Pointer(s), Pointer(Result), n);
   end;
end;

// ------------------
// ------------------ TInt64DynArrayHelper ------------------
// ------------------

// High
//
function TInt64DynArrayHelper.High : Integer;
begin
   Result := System.High(Self);
end;

// Length
//
function TInt64DynArrayHelper.Length : Integer;
begin
   Result := System.Length(Self);
end;

// ------------------
// ------------------ TStringUnifier ------------------
// ------------------

var
   vUnifiedStrings : array [0..1] of Pointer;
   vUnifiedCharacters : array [0..Ord(TStringUnifier.HighestUnifiedChar)] of String;

// Create
//
constructor TStringUnifier.Create;
begin
   inherited;
   Grow;
end;

// UnifyAssign
//
procedure TStringUnifier.UnifyAssign(const aString : String; var unifiedString : String);
begin
   UnifyAssignP(Pointer(aString), Length(aString), unifiedString);
end;

// UnifyAssignP
//
procedure TStringUnifier.UnifyAssignP(p : PChar; size : Integer; var unifiedString : String);
var
   i : Integer;
   h : Cardinal;
   bucket : PStringUnifierBucket;
begin
   case size of
      0 : begin
         unifiedString := '';
         Exit;
      end;
      1 : if Cardinal(Ord(p^)) <= Cardinal(High(vUnifiedCharacters)) then begin
         unifiedString := vUnifiedCharacters[Ord(p^)];
         Exit;
      end;
   end;

   h := SimpleStringHash(p, size);
   i := (h and FMask);

   repeat
      bucket := @FBuckets[i];
      if bucket^.Hash = h then begin
         if (Length(bucket^.Str)=size) and CompareMem(p, Pointer(bucket^.Str), size*SizeOf(Char)) then begin
            unifiedString := bucket^.Str;
            Exit;
         end;
      end else if bucket^.Hash=0 then begin
         bucket^.Hash := h;
         SetString(unifiedString, p, size);
         bucket^.Str := unifiedString;
         Inc(FCount);
         Dec(FGrowth);
         if FGrowth = 0 then Grow;
         Exit;
      end;
      i := (i+1) and FMask;
   until False;
end;

// DistinctStrings
//
function TStringUnifier.DistinctStrings : TStringDynArray;
var
   n, i : Integer;
   bucket : PStringUnifierBucket;
begin
   SetLength(Result, FCount);
   n := 0;
   for i := 0 to FCapacity-1 do begin
      bucket := @FBuckets[i];
      if bucket.Hash <> 0 then begin
         Result[n] := bucket.Str;
         Inc(n);
      end;
   end;
end;

// Clear
//
procedure TStringUnifier.Clear;
begin
   FBuckets := nil;
   FCount := 0;
   FCapacity := 0;
   Grow;
end;

// Grow
//
procedure TStringUnifier.Grow;
var
   i, j, n : Integer;
   oldBuckets : TStringUnifierBuckets;
begin
   if FCapacity = 0 then
      FCapacity := 64
   else FCapacity := FCapacity*2;
   FGrowth := (FCapacity*5) div 8 - FCount;

   oldBuckets := FBuckets;
   FBuckets := nil;
   SetLength(FBuckets, FCapacity);

   n := FCapacity-1;
   FMask := n;
   for i := 0 to High(oldBuckets) do begin
      if oldBuckets[i].Hash=0 then continue;
      j := (oldBuckets[i].Hash and (FCapacity-1));
      while FBuckets[j].Hash <> 0 do
         j := (j+1) and n;
      FBuckets[j].Hash := oldBuckets[i].Hash;
      Pointer(FBuckets[j].Str) := Pointer(oldBuckets[i].Str);
      Pointer(oldBuckets[i].Str) := nil;
   end;
end;

// ------------------
// ------------------ String Unifier ------------------
// ------------------

{$IFNDEF FPC}
type
   {$IF CompilerVersion > 22}
   TStringListList = TStringItemList;
   {$ELSE}
   TStringListList = PStringItemList;
   {$IFEND}

   TStringListCracker = class (TStrings)
      private
         FList : TStringListList;
   end;
{$ENDIF}

// InitializeStringsUnifier
//
procedure InitializeStringsUnifier;
var
   i : Integer;
begin
   for i := Low(vUnifiedStrings) to High(vUnifiedStrings) do begin
      vUnifiedStrings[i] := TStringUnifier.Create;
      TStringUnifier(vUnifiedStrings[i]).Tag := i;
   end;

   for i := Low(vUnifiedCharacters) to High(vUnifiedCharacters) do
      vUnifiedCharacters[i] := Char(Ord(i));
end;

// FinalizeStringsUnifier
//
procedure FinalizeStringsUnifier;
var
   i : Integer;
begin
   for i:=Low(vUnifiedStrings) to High(vUnifiedStrings) do begin
      TStringUnifier(vUnifiedStrings[i]).Free;
      vUnifiedStrings[i]:=nil;
   end;
end;

// AcquireUnifier
//
function AcquireUnifier : TStringUnifier;
var
   p : Pointer;
begin
   Result := nil;
   repeat
      p := vUnifiedStrings[0];
      if p <> nil then
         Result := TStringUnifier(InterlockedCompareExchangePointer(vUnifiedStrings[0], nil, p));
      if Result <> nil then Exit;
      p := vUnifiedStrings[1];
      if p <> nil then
         Result := TStringUnifier(InterlockedCompareExchangePointer(vUnifiedStrings[1], nil, p));
      if Result <> nil then Exit;
      Sleep(0);
   until False;
end;

// ReleaseUnifier
//
procedure ReleaseUnifier(unifier : TStringUnifier);
begin
   vUnifiedStrings[unifier.Tag] := unifier;
end;

// UnifyAssignString
//
procedure UnifyAssignString(const fromStr : String; var toStr : String);
begin
   UnifyAssignP(Pointer(fromStr), Length(fromStr), toStr);
end;

// UnifyAssignP
//
procedure UnifyAssignP(p : PChar; size : Integer; var toStr : String);
var
   su : TStringUnifier;
begin
   case size of
      0 : begin
         toStr := '';
         Exit;
      end;
      1 : begin
         if (Cardinal(Ord(p^)) <= Cardinal(High(vUnifiedCharacters))) then begin
            toStr := vUnifiedCharacters[Ord(p^)];
            Exit;
         end;
      end;
   end;
   su := AcquireUnifier;
   su.UnifyAssignP(p, size, toStr);
   ReleaseUnifier(su);
end;

// UnifyAssignChar
//
procedure UnifyAssignChar(p : PChar; var toStr : String);
begin
   if Cardinal(Ord(p^)) <= Cardinal(High(vUnifiedCharacters)) then
      toStr := vUnifiedCharacters[Ord(p^)]
   else UnifyAssignP(p, 1, toStr);
end;

// UnifiedString
//
function UnifiedString(const fromStr : String) : String;
begin
   UnifyAssignString(fromStr, Result);
end;

// TidyStringsUnifier
//
function TidyStringsUnifier : Integer;
var
   i : Integer;
   su : array [0..High(vUnifiedStrings)] of TStringUnifier;
   p : Pointer;
begin
   Result := 0;
   for i := Low(vUnifiedStrings) to High(vUnifiedStrings) do begin
      repeat
         p := vUnifiedStrings[i];
         if p <> nil then
            su[i] := TStringUnifier(InterlockedCompareExchangePointer(vUnifiedStrings[i], nil, p))
         else su[i] := nil;
      until su[i] <> nil;
      Result := Result + su[i].FCount;
      su[i].Clear;
   end;
   for i := Low(vUnifiedStrings) to High(vUnifiedStrings) do
      vUnifiedStrings[i] := su[i];
end;

// UnicodeCompareLen
//
function UnicodeCompareLen(p1, p2 : PWideChar; n : Integer) : Integer;
var
   c1, c2 : Integer;
begin
   for n := n downto 1 do begin
      c1 := Ord(p1^);
      c2 := Ord(p2^);
      if c1 <> c2 then begin
         if (c1 <= 127) and (c2 <= 127) then begin
            //if c1 in [Ord('a')..Ord('z')] then
            if Cardinal(c1 - Ord('a')) <= Cardinal(Ord('z') - Ord('a')) then
               c1 := c1 + (Ord('A')-Ord('a'));
            // if c2 in [Ord('a')..Ord('z')] then
            if Cardinal(c2 - Ord('a')) <= Cardinal(Ord('z') - Ord('a')) then
               c2 := c2 + (Ord('A')-Ord('a'));
            if c1 <> c2 then begin
               Result := c1 - c2;
               Exit;
            end;
         end else begin
            Result := UnicodeCompareP(p1, p2, n);
            Exit;
         end;
      end;
      Inc(p1);
      Inc(p2);
   end;
   Result := 0;
end;

// UnicodeCompareText
//
function UnicodeCompareText(const s1, s2 : UnicodeString) : Integer;
var
   n1, n2 : Integer;
   ps1, ps2 : PWideChar;
begin
   ps1 := PWideChar(Pointer(s1));
   ps2 := PWideChar(Pointer(s2));
   if ps1 = ps2 then Exit(0);
   if ps1 <> nil then begin
      {$if Defined(WIN64_ASM) or Defined(WIN32_ASM)}
      n1 := PInteger(NativeUInt(ps1)-4)^;
      {$else}
      n1:=Length(s1);
      {$endif}
      if ps2 <> nil then begin
         {$if Defined(WIN64_ASM) or Defined(WIN32_ASM)}
         n2 := PInteger(NativeUInt(ps2)-4)^;
         {$else}
         n2:=Length(s2);
         {$endif}
         if n1 < n2 then begin
            Result := UnicodeCompareLen(ps1, ps2, n1);
            if Result = 0 then
               Result := -1;
         end else begin
            Result := UnicodeCompareLen(ps1, ps2, n2);
            if (Result = 0) and (n1 > n2) then
               Result := 1;
         end;
      end else Result := 1;
   end else if ps2 <> nil then
      Result := -1
   else Result := 0;
end;

// UnicodeSameText
//
function UnicodeSameText(const s1, s2 : UnicodeString) : Boolean;
begin
   Result:=(Length(s1)=Length(s2)) and (UnicodeCompareText(s1, s2)=0)
end;

// UnicodeCompareStr
//
function UnicodeCompareStr(const s1, s2 : UnicodeString) : Integer;
begin
   {$ifdef FPC}
   Result := UnicodeCompareStr(s1, s2);
   {$else}
   Result := CompareStr(s1, s2);
   {$endif}
end;

{$ifdef FPC}
// UnicodeCompareText
//
function UnicodeCompareText(const s1, s2 : String) : Integer; overload;
begin
   Result := UnicodeCompareText(UnicodeString(s1), UnicodeString(s2));
end;

// UnicodeCompareStr
//
function UnicodeCompareStr(const s1, s2 : String) : Integer; inline;
begin
   Result := CompareStr(s1, s2);
end;

// UnicodeSameText
//
function UnicodeSameText(const s1, s2 : String) : Boolean;
begin
   Result := UnicodeSameText(UnicodeString(s1), UnicodeString(s2));
end;
{$endif FPC}

// AsciiCompareLen
//
function AsciiCompareLen(p1, p2 : PAnsiChar; n : Integer) : Integer;
var
   c1, c2 : Integer;
begin
   for n:=n downto 1 do begin
      c1:=Ord(p1^);
      c2:=Ord(p2^);
      if (c1<>c2) then begin
         if c1 in [Ord('a')..Ord('z')] then
            c1:=c1+(Ord('A')-Ord('a'));
         if c2 in [Ord('a')..Ord('z')] then
            c2:=c2+(Ord('A')-Ord('a'));
         if c1<>c2 then begin
            Result:=c1-c2;
            Exit;
         end;
      end;
      Inc(p1);
      Inc(p2);
   end;
   Result:=0;
end;

// PosA
//
function PosA(const sub, main : RawByteString) : Integer; inline;
begin
   Result:=Pos(sub, main);
end;

// StrIsASCII
//
function StrIsASCII(const s : String) : Boolean;
begin
   var n := Length(s);
   var p : PChar := Pointer(s);
   {$ifndef FPC}
   var mask := UInt64($FF80FF80FF80FF80);
   while n >= 4 do begin
      if (PUInt64(p)^ and mask) <> 0 then
         Exit(False);
      Dec(n, 4);
      Inc(p, 4);
   end;
   {$endif}
   while n > 0 do begin
      if Ord(p^) > 127  then
         Exit(False);
      Inc(p);
      Dec(n);
   end;
   Result := True;
end;

// StrIsASCII
//
function StrIsASCII(const s : RawByteString) : Boolean;
begin
   var n := Length(s);
   var p : PAnsiChar := Pointer(s);
   {$ifndef FPC}
   var mask := UInt32($80808080);
   while n >= 4 do begin
      if (PUInt32(p)^ and mask) <> 0 then
         Exit(False);
      Dec(n, 4);
      Inc(p, 4);
   end;
   {$endif}
   while n > 0 do begin
      if Ord(p^) > 127  then
         Exit(False);
      Inc(p);
      Dec(n);
   end;
   Result := True;
end;

// StrNonNilLength
//
function StrNonNilLength(const aString : String) : Integer;
begin
   Result:=PInteger(NativeUInt(Pointer(aString))-4)^;
end;

// StrIBeginsWith
//
function StrIBeginsWith(const aStr, aBegin : String) : Boolean;
var
   n1, n2 : Integer;
begin
   n1:=Length(aStr);
   n2:=Length(aBegin);
   if (n2>n1) or (n2=0) then
      Result:=False
   {$ifdef FPC}
   else Result:=(UnicodeCompareText(Copy(aStr, 1, n2), Copy(aBegin, 1, n2))=0);
   {$else}
   else Result:=(UnicodeCompareLen(PWideChar(aStr), PWideChar(aBegin), n2)=0);
   {$endif}
end;

// StrBeginsWith
//
function StrBeginsWith(const aStr, aBegin : String) : Boolean;
var
   n1, n2 : Integer;
begin
   n1:=Length(aStr);
   n2:=Length(aBegin);
   if (n2>n1) or (n2=0) then
      Result:=False
   else Result:=CompareMem(Pointer(aStr), Pointer(aBegin), n2*SizeOf(Char));
end;

// StrBeginsWithA
//
function StrBeginsWithA(const aStr, aBegin : RawByteString) : Boolean;
var
   n1, n2 : Integer;
begin
   n1:=Length(aStr);
   n2:=Length(aBegin);
   if (n2>n1) or (n2=0) then
      Result:=False
   else Result:=CompareMem(Pointer(aStr), Pointer(aBegin), n2);
end;

// StrBeginsWithBytes
//
function StrBeginsWithBytes(const aStr : RawByteString; const bytes : array of Byte) : Boolean;
var
   i, n : Integer;
begin
   n:=Length(bytes);
   if Length(aStr)<n then
      Result:=False
   else begin
      for i:=0 to n-1 do
         if Ord(PAnsiChar(Pointer(aStr))[i])<>bytes[i] then Exit(False);
      Result:=True;
   end;
end;

// StrIEndsWith
//
function StrIEndsWith(const aStr, aEnd : String) : Boolean;
var
   n1, n2 : Integer;
begin
   n1:=Length(aStr);
   n2:=Length(aEnd);
   if (n2>n1) or (n2=0) then
      Result:=False
   {$ifdef FPC}
   else Result:=(UnicodeCompareText(Copy(aStr, n1-n2+1), aEnd)=0);
   {$else}
   else Result:=(UnicodeCompareLen(@aStr[n1-n2+1], Pointer(aEnd), n2)=0);
   {$endif}
end;

// StrIEndsWithA
//
function StrIEndsWithA(const aStr, aEnd : RawByteString) : Boolean;
var
   n1, n2 : Integer;
begin
   n1:=Length(aStr);
   n2:=Length(aEnd);
   if (n2>n1) or (n2=0) then
      Result:=False
   else Result:=(AsciiCompareLen(@aStr[n1-n2+1], Pointer(aEnd), n2)=0);
end;

// StrEndsWith
//
function StrEndsWith(const aStr, aEnd : String) : Boolean;
var
   n1, n2 : Integer;
begin
   n1:=Length(aStr);
   n2:=Length(aEnd);
   if (n2>n1) or (n2=0) then
      Result:=False
   else Result:=CompareMem(@aStr[n1-n2+1], Pointer(aEnd), n2*SizeOf(Char));
end;

// StrEndsWithA
//
function StrEndsWithA(const aStr, aEnd : RawByteString) : Boolean;
var
   n1, n2 : Integer;
begin
   n1:=Length(aStr);
   n2:=Length(aEnd);
   if (n2>n1) or (n2=0) then
      Result:=False
   else Result:=CompareMem(@aStr[n1-n2+1], Pointer(aEnd), n2);
end;

// StrContains (sub string)
//
function StrContains(const aStr, aSubStr : String) : Boolean;
begin
   if aSubStr='' then
      Result:=True
   else if StrNonNilLength(aSubStr)=1 then
      Result:=StrContains(aStr, aSubStr[1])
   else Result:=(Pos(aSubStr, aStr)>0);
end;

// StrContains (sub character)
//
function StrContains(const aStr : String; aChar : Char) : Boolean;
begin
   Result := (StrIndexOfChar(aStr, aChar) > 0)
end;

// StrIndexOfChar
//
function StrIndexOfChar(const aStr : String; aChar : Char; startIndex : Integer = 1) : Integer;
begin
   for var i := startIndex to Length(aStr) do
      if aStr[i] = aChar then Exit(i);
   Result := 0;
end;

// StrIndexOfCharA
//
function StrIndexOfCharA(const aStr : RawByteString; aChar : AnsiChar) : Integer;
begin
   for var i := 1 to Length(aStr) do
      if aStr[i] = aChar then Exit(i);
   Result := 0;
end;

// StrLastIndexOfCharA
//
function StrLastIndexOfCharA(const aStr : RawByteString; aChar : AnsiChar) : Integer;
begin
   for var i := Length(aStr) downto 1 do
      if aStr[i] = aChar then Exit(i);
   Result := 0;
end;

// LowerCaseA
//
function LowerCaseA(const aStr : RawByteString) : RawByteString;
begin
   var n := Length(aStr);
   SetLength(Result, n);
   if n <= 0 then Exit;

   var dst := PByte(Pointer(Result));
   var src := PByte(Pointer(aStr));
   for var i := 1 to n do begin
      var c := src^;
      if c in [Ord('A')..Ord('Z')] then
         c := c or $20;
      dst^ := c;
      Inc(src);
      Inc(dst);
   end;
end;

// StrMatches
//
function StrMatches(const aStr, aMask : String) : Boolean;
begin
   var mask := TMask.Create(aMask);
   try
      Result := mask.Matches(aStr);
   finally
      mask.Free;
   end;
end;

// StrDeleteLeft
//
function StrDeleteLeft(const aStr : String; n : Integer) : String;
begin
   Result:=Copy(aStr, n+1);
end;

// StrDeleteRight
//
function StrDeleteRight(const aStr : String; n : Integer) : String;
begin
   Result:=Copy(aStr, 1, Length(aStr)-n);
end;

// StrAfterChar
//
function StrAfterChar(const aStr : String; aChar : Char) : String;
begin
   var p := StrIndexOfChar(aStr, aChar);
   if p > 0 then
      Result := Copy(aStr, p+1)
   else Result := '';
end;

// StrBeforeChar
//
function StrBeforeChar(const aStr : String; aChar : Char) : String;
begin
   var p := StrIndexOfChar(aStr, aChar);
   if p > 0 then
      Result := Copy(aStr, 1, p-1)
   else Result := aStr;
end;

// StrReplaceChar
//
function StrReplaceChar(const aStr : String; oldChar, newChar : Char) : String;
begin
   var n := Length(aStr);
   SetLength(Result, n);
   var pSrc := PChar(Pointer(aStr));
   var pDst := PChar(Pointer(Result));
   for var i := 0 to n-1 do begin
      if pSrc[i] = oldChar then
         pDst[i] := newChar
      else pDst[i] := pSrc[i]
   end;
end;

// StrReplaceMacros
//
function StrReplaceMacros(const aStr : String; const macros : array of String;
                          const startDelimiter, stopDelimiter : String) : String;
begin
   Assert(not Odd(Length(macros)));
   if aStr = '' then Exit('');
   if startDelimiter = '' then Exit('');

   var p := 1;
   var wobs := TWriteOnlyBlockStream.Create;
   try
      while True do begin
         var pStart := Pos(startDelimiter, aStr, p);
         if pStart <= 0 then Break;

         pStart := pStart + Length(startDelimiter);

         var pStop :  Integer;
         if stopDelimiter <> '' then
            pStop := Pos(stopDelimiter, aStr, pStart)
         else pStop := Pos(startDelimiter, aStr, pStart);
         if pStop <= 0 then Break;

         var replaced := False;
         var candidateLength := pStop - pStart;
         for var i := 0 to (Length(macros) div 2) - 1 do begin
            if     (Length(macros[i*2]) = candidateLength)
               and CompareMem(@aStr[pStart], Pointer(macros[i*2]), candidateLength*SizeOf(Char)) then begin
               wobs.WriteSubString(aStr, p, pStart - p - Length(startDelimiter));
               wobs.WriteString(macros[i*2+1]);
               if stopDelimiter <> '' then
                  p := pStop + Length(stopDelimiter)
               else p := pStop + Length(startDelimiter);
               replaced := True;
               Break;
            end;
         end;

         if not replaced then begin
            wobs.WriteSubString(aStr, p, pStop - p);
            p := pStop;
         end;
      end;
      wobs.WriteSubString(aStr, p);
      Result := wobs.ToString;
   finally
      wobs.Free;
   end;
end;

// StrCountString
//
function StrCountString(const haystack, needle : String) : Integer;
var
   p, n : Integer;
begin
   Result := 0;
   if (haystack = '') or (needle = '') then Exit;
   n := Length(needle);
   p := 1;
   while True do begin
      p := Pos(needle, haystack, p);
      if p > 0 then begin
         p := p + n;
         Inc(Result);
      end else Break;
   end;
end;

// StrCountChar
//
function StrCountChar(const aStr : String; c : Char) : Integer;
begin
   Result := 0;
   for var i := 1 to Length(aStr) do
      if aStr[i] = c then
         Inc(Result);
end;

// StrCountStartChar
//
function StrCountStartChar(const aStr : String; c : Char) : Integer;
begin
   Result := 0;
   for var i := 1 to Length(aStr) do
      if aStr[i] = c then
         Inc(Result)
      else Break;
end;

// WhichPowerOfTwo
//
function WhichPowerOfTwo(const v : Int64) : Integer;
var
   n : Int64;
begin
   if (v>0) and ((v and (v-1))=0) then begin
      for Result:=0 to 63 do begin
         n:=Int64(1) shl Result;
         if n>v then Break;
         if n=v then Exit;
      end;
   end;
   Result:=-1;
end;

// ApplyStringVariables
//
function ApplyStringVariables(const str : TFilename; const variables : TStrings;
                              const delimiter : String = '%') : TFilename;
var
   p1, p2 : Integer;
begin
   Result := str;
   if (str='') or (variables.Count=0) then Exit;

   p1:=Pos(delimiter, Result);
   while p1>0 do begin
      p2 := Pos(delimiter, Result, p1+1);
      if p2<p1 then Break;
      Result:= Copy(Result, 1, p1-1)
              +variables.Values[Copy(Result, p1+1, p2-p1-1)]
              +Copy(Result, p2+1);
      p1 := Pos('%', Result, p1);
   end;
end;

// ------------------
// ------------------ TFastCompareTextList ------------------
// ------------------

// CompareStrings
//
{$ifdef FPC}
function TFastCompareTextList.DoCompareText(const S1, S2: String): PtrInt;
begin
   Result:=UnicodeCompareText(s1, s2);
end;
{$else}
function TFastCompareTextList.CompareStrings(const S1, S2: String): Integer;
begin
   Result:=UnicodeCompareText(s1, s2);
end;

// FindName
//
function TFastCompareTextList.FindName(const name : String; var index : Integer) : Boolean;
var
   lo, hi, mid, cmp, n, nc : Integer;
   initial : String;
   list : TStringListList;
begin
   Result:=False;
   list:=TStringListCracker(Self).FList;
   initial:=Name+NameValueSeparator;
   n:=Length(initial);
   lo:=0;
   hi:=Count-1;
   while lo<=hi do begin
      mid:=(lo+hi) shr 1;
      nc:=Length(list[mid].FString);
      if nc>=n then begin
         cmp:=UnicodeCompareLen(PWideChar(Pointer(list[mid].FString)), PWideChar(Pointer(initial)), n);
      end else begin
         cmp:=UnicodeCompareLen(PWideChar(Pointer(list[mid].FString)), PWideChar(Pointer(initial)), nc);
         if cmp=0 then
            cmp:=-1;
      end;
      if cmp<0 then
         lo:=mid+1
      else begin
         hi:=mid-1;
         if cmp=0 then begin
            Result:=True;
            if Duplicates<>dupAccept then
               lo:=mid;
         end;
      end;
   end;
   index:=lo;
end;

// IndexOfName
//
function TFastCompareTextList.IndexOfName(const name : String): Integer;
var
   n, nc : Integer;
   nvs : WideChar;
   list : TStringListList;
begin
   if not Sorted then begin
      nvs:=NameValueSeparator;
      n:=Length(name);
      list:=TStringListCracker(Self).FList;
      for Result:=0 to Count-1 do begin
         nc:=Length(list[Result].FString);
         if     (nc>n) and (list[Result].FString[n+1]=nvs)
            and (UnicodeCompareLen(PWideChar(Pointer(name)),
                                   PWideChar(Pointer(list[Result].FString)), n)=0) then Exit;
      end;
      Result:=-1;
   end else begin
      if not FindName(name, Result) then
         Result:=-1;
   end;
end;
{$endif}

// Reverse
//
procedure TFastCompareTextList.Reverse;
var
   i, j : Integer;
begin
   i := 0;
   j := Count-1;
   while i < j do begin
      Exchange(i, j);
      Inc(i);
      Dec(j);
   end;
end;

// SameText
//
function TFastCompareTextList.SameText(idx : Integer; const value : String) : Boolean;
begin
   Result := UnicodeSameText(TStringListCracker(Self).FList[idx].FString, value);
end;

// ------------------
// ------------------ TVarRecArrayContainer ------------------
// ------------------

// AddVarRec
//
function TVarRecArrayContainer.AddVarRec : PVarRec;
var
   n : Integer;
begin
   n:=Length(VarRecArray);
   SetLength(VarRecArray, n+1);
   Result:=@VarRecArray[n];
end;

// Add
//
procedure TVarRecArrayContainer.Add(const v : Variant);
begin
   if VarType(v)=varBoolean then
      AddBoolean(v)
   else if VarIsOrdinal(v) then
      AddInteger(v)
   else if VarIsNumeric(v) then
      AddFloat(v)
   else if VarIsStr(v) then
      AddString(v)
   else begin
      // not really supported yet, use a nil placeholder
      with AddVarRec^ do begin
         VType:=vtPointer;
         VPointer:=nil;
      end;
   end;
end;

// AddBoolean
//
procedure TVarRecArrayContainer.AddBoolean(const b : Boolean);
begin
   with AddVarRec^ do begin
      VType:=vtBoolean;
      VBoolean:=b;
   end;
end;

// AddInteger
//
procedure TVarRecArrayContainer.AddInteger(const i : Int64);
var
   n : Integer;
begin
   n:=Length(FIntegers);
   SetLength(FIntegers, n+1);
   FIntegers[n]:=i;
   with AddVarRec^ do begin
      VType:=vtInt64;
      VInteger:=n;
   end;
end;

// AddFloat
//
procedure TVarRecArrayContainer.AddFloat(const f : Double);
var
   n : Integer;
begin
   n:=Length(FFloats);
   SetLength(FFloats, n+1);
   FFloats[n]:=f;
   with AddVarRec^ do begin
      VType:=vtExtended;
      VInteger:=n;
   end;
end;

// AddString
//
procedure TVarRecArrayContainer.AddString(const s : String);
var
   n : Integer;
begin
   n:=Length(FStrings);
   SetLength(FStrings, n+1);
   FStrings[n]:=s;
   with AddVarRec^ do begin
      VType:=vtUnicodeString;
      VInteger:=n;
   end;
end;

// Initialize
//
procedure TVarRecArrayContainer.Initialize;
var
   i : Integer;
   rec : PVarRec;
begin
   for i:=0 to High(VarRecArray) do begin
      rec:=@VarRecArray[i];
      case rec.VType of
         vtInt64 : rec.VInt64:=@FIntegers[rec.VInteger];
         vtExtended : rec.VExtended:=@FFloats[rec.VInteger];
         vtUnicodeString : rec.VString:=Pointer(FStrings[rec.VInteger]);
      end;
   end;
end;

// ------------------
// ------------------ TVarRecArrayContainer ------------------
// ------------------

// Clean
//
procedure TTightList.Clean;
var
   i : Integer;
begin
   case Count of
      0 : Exit;
      1 : TRefCountedObject(FList).Free;
   else
      for i:=Count-1 downto 0 do
         FList[i].Free;
   end;
   Clear;
end;

// Clear
//
procedure TTightList.Clear;
begin
   case Count of
      0 : Exit;
      1 : ;
   else
      FreeMem(FList);
   end;
   FList:=nil;
   FCount:=0;
end;

// Initialize
//
procedure TTightList.Initialize; // in case of use as a local variable
begin
   FList:=nil;
   FCount:=0;
end;

// Free
//
procedure TTightList.Free;
begin
   Clear;
end;

// List
//
function TTightList.List : PObjectTightList;
begin
   if Count=1 then
      Result:=@FList
   else Result:=FList;
end;

// Add
//
function TTightList.Add(item : TRefCountedObject) : Integer;
var
   buf : Pointer;
begin
   case Count of
      0 : begin
         FList:=PObjectTightList(item);
         FCount:=1;
      end;
      1 : begin
         buf:=FList;
         FList:=AllocMem(2*SizeOf(Pointer));
         FList[0]:=buf;
         FList[1]:=item;
         FCount:=2;
      end;
   else
      Inc(FCount);
      ReallocMem(FList, Count*SizeOf(Pointer));
      FList[Count-1]:=item;
   end;
   Result:=FCount-1;
end;

// Assign
//
procedure TTightList.Assign(const aList : TTightList);
begin
   Clear;
   FCount:=aList.FCount;
   case Count of
      0 : Exit;
      1 : FList:=aList.FList;
   else
      ReallocMem(FList, Count*SizeOf(Pointer));
      System.Move(aList.FList^, FList^, Count*SizeOf(Pointer));
   end;
end;

// IndexOf
//
function TTightList.IndexOf(item : TRefCountedObject) : Integer;
begin
   case Count of
      0 : Result:=-1;
      1 : begin
         if FList=PObjectTightList(item) then
            Result:=0
         else Result:=-1;
      end;
   else
      Result:=0;
      while Result<FCount do begin
         if FList[Result]=item then Exit;
         Inc(Result);
      end;
      Result:=-1;
   end;
end;

// Remove
//
function TTightList.Remove(item : TRefCountedObject) : Integer;
begin
   Result:=IndexOf(item);
   if Result>=0 then
      Delete(Result);
end;

// Delete
//
procedure TTightList.Delete(index : Integer);
var
   i : Integer;
   buf : Pointer;
begin
   if Cardinal(index)>=Cardinal(Count) then
      RaiseIndexOutOfBounds
   else begin
      case Count of
         1 : begin
            FList:=nil;
            FCount:=0;
         end;
         2 : begin
            buf:=FList;
            if index=0 then
               FList:=PObjectTightList(FList[1])
            else FList:=PObjectTightList(FList[0]);
            FreeMem(buf);
            FCount:=1;
         end;
      else
         for i:=index+1 to Count-1 do
            FList[i-1]:=FList[i];
         Dec(FCount);
      end;
   end;
end;

// Insert
//
procedure TTightList.Insert(index : Integer; item : TRefCountedObject);
var
   i : Integer;
   locList : PObjectTightList;
begin
   if Cardinal(index)>Cardinal(FCount) then
      RaiseIndexOutOfBounds
   else case Count of
      0 : begin
         FList:=PObjectTightList(item);
         FCount:=1;
      end;
      1 : begin
         if index=1 then
            Add(item)
         else begin
            Add(TRefCountedObject(FList));
            FList[0]:=item;
         end;
      end;
   else
      ReallocMem(FList, (FCount+1)*SizeOf(Pointer));
      locList:=FList;
      for i:=Count-1 downto index do
         locList[i+1]:=locList[i];
      locList[index]:=item;
      Inc(FCount);
   end;
end;

// Move
//
procedure TTightList.MoveItem(curIndex, newIndex : Integer);
var
   item : Pointer;
begin
   if (Cardinal(curIndex)>=Cardinal(FCount)) or (Cardinal(newIndex)>=Cardinal(FCount)) then
      RaiseIndexOutOfBounds
   else begin
      case curIndex-newIndex of
         0 : ; // ignore
         -1, 1 : Exchange(curIndex, newIndex);
      else
         item:=FList[curIndex];
         Delete(curIndex);
         Insert(newIndex, item);
      end;
   end;
end;

// Exchange
//
procedure TTightList.Exchange(index1, index2 : Integer);
var
   item : Pointer;
begin
   if index1<>index2 then begin
      item:=FList[index1];
      FList[index1]:=FList[index2];
      FList[index2]:=item;
   end;
end;

// ItemsAllOfClass
//
function TTightList.ItemsAllOfClass(aClass : TClass) : Boolean;
var
   i : Integer;
begin
   if FCount = 1 then
      Result := (TRefCountedObject(FList).ClassType = aClass)
   else begin
      for i := 0 to FCount-1 do
         if FList[i].ClassType <> aClass then Exit(False);
      Result := True;
   end;
end;

// RaiseIndexOutOfBounds
//
procedure TTightList.RaiseIndexOutOfBounds;
begin
   raise ETightListOutOfBound.Create('List index out of bounds');
end;

// Sort
//
procedure TTightList.Sort(const comparer : TPointerComparer);
var
   i, j : Integer;
begin
   case Count of
      0, 1 : Exit;
      2 : begin
         if comparer(FList[0], FList[1]) > 0 then
            Exchange(0, 1);
      end;
      3 : begin
         if comparer(FList[0], FList[1]) > 0 then
            Exchange(0, 1);
         if comparer(FList[1], FList[2]) > 0 then begin
            Exchange(1, 2);
            if comparer(FList[0], FList[1]) > 0 then
               Exchange(0, 1);
         end;
      end;
   else
      // use an insertion sort, tight list is for very small lists
      i := 1;
      while i < Count do begin
         j := i;
         while (j > 0) and (comparer(FList[j-1], FList[j]) > 0) do begin
            Exchange(j, j-1);
            Dec(j);
         end;
         Inc(i);
      end;
   end;
end;

// GetEnumerator
//
function TTightList.GetEnumerator : TTightListEnumerator;
begin
   case FCount of
      0 : begin
         Result.FIter := nil;
         Result.FTail := nil;
      end;
      1 : begin
         Result.FIter := @FList;
         Dec(Result.FIter);
         Result.FTail := @FList;
      end;
   else
      Result.FIter := PPointer(IntPtr(@FList[0])-SizeOf(Pointer));
      Result.FTail := @PObjectTightList(Result.FIter)[FCount];
   end;
end;

// TTightListEnumerator.MoveNext
//
function TTightList.TTightListEnumerator.MoveNext : Boolean;
begin
   if FIter <> FTail then begin
      Inc(FIter);
      Result := True;
   end else Result := False;
end;

// TTightListEnumerator.Current
//
function TTightList.TTightListEnumerator.GetCurrent : TRefCountedObject;
begin
   Result := FIter^;
end;

// ------------------
// ------------------ TObjectList<T> ------------------
// ------------------

// Destroy
//
destructor TObjectList<T>.Destroy;
begin
   Clear;
   inherited;
end;

// GetItem
//
function TObjectList<T>.GetItem(index : Integer) : T;
begin
   Assert(Cardinal(index)<Cardinal(FCount), 'Index out of range');
   Result:=FItems[index];
end;

// SetItem
//
procedure TObjectList<T>.SetItem(index : Integer; const item : T);
begin
   Assert(Cardinal(index)<Cardinal(FCount), 'Index out of range');
   FItems[index]:=item;
end;

// Add
//
function TObjectList<T>.Add(const anItem : T) : Integer;
begin
   if Count=Length(FItems) then
      SetLength(FItems, Count+8+(Count shr 4));
   FItems[FCount]:=anItem;
   Result:=FCount;
   Inc(FCount);
end;

// IndexOf
//
function TObjectList<T>.IndexOf(const anItem : T) : Integer;
var
   i : Integer;
begin
   for i:=0 to Count-1 do
      if FItems[i]=anItem then Exit(i);
   Result:=-1;
end;

// Insert
//
procedure TObjectList<T>.Insert(idx : Integer; const anItem : T);
begin
   Assert(Cardinal(idx) <= Cardinal(FCount), 'Index out of range');
   if FCount = Length(FItems) then
      SetLength(FItems, Count+8+(Count shr 4));
   if idx < FCount then
      System.Move(FItems[idx], FItems[idx+1], SizeOf(T)*(FCount-idx));
   FItems[idx] := anItem;
   Inc(FCount);
end;

// Move
//
procedure TObjectList<T>.Move(idx, idxNew, itemCount : Integer);
var
   saveItems : ArrayT;
   saveByteCount : NativeInt;
   idxSave, idxRestore, saveItemCount : Integer;
begin
   Assert(itemCount >= 0 , 'Negative item count');
   Assert(Cardinal(idx) <= Cardinal(FCount), 'Index out of range');
   Assert(Cardinal(idx + itemCount - 1) <= Cardinal(FCount), 'Last item index out of range');
   Assert(Cardinal(idxNew) <= Cardinal(FCount), 'New index out of range');
   Assert(Cardinal(idxNew + itemCount - 1) <= Cardinal(FCount), 'New last item index out of range');
   if idx > idxNew then begin
      idxSave := idxNew;
      saveItemCount := idx - idxNew;
      idxRestore := idxNew + itemCount;
   end else begin
      idxSave := idx + itemCount;
      saveItemCount := idxNew - idx;
      idxRestore := idx;
   end;
   if saveItemCount > 0 then begin
      SetLength(saveItems, saveItemCount);
      saveByteCount := SizeOf(T) * saveItemCount;
      System.Move(FItems[idxSave], saveItems[0], saveByteCount);
      System.Move(FItems[idx], FItems[idxNew], SizeOf(T)*itemCount);
      System.Move(saveItems[0], FItems[idxRestore], saveByteCount);
   end;
end;

// Extract
//
function TObjectList<T>.Extract(idx : Integer) : T;
var
   n : Integer;
begin
   Assert(Cardinal(idx)<Cardinal(FCount), 'Index out of range');
   n:=Count-1-idx;
   Dec(FCount);
   Result:=FItems[idx];
   if n>0 then
      System.Move(FItems[idx+1], FItems[idx], SizeOf(T)*n);
end;

// ExtractAll
//
procedure TObjectList<T>.ExtractAll;
begin
   FCount:=0;
end;

// Clear
//
procedure TObjectList<T>.Clear;
var
   i : Integer;
begin
   for i:=FCount-1 downto 0 do
      FItems[i].Free;
   FCount:=0;
end;

// ------------------
// ------------------ TSortedList<T> ------------------
// ------------------

// GetItem
//
function TSortedList<T>.GetItem(index : Integer) : T;
begin
   Result:=FItems[index];
end;

// Find
//
function TSortedList<T>.Find(const item : T; var index : Integer) : Boolean;
var
   lo, hi, mid, compResult : Integer;
begin
   Result:=False;
   lo:=0;
   hi:=FCount-1;
   while lo<=hi do begin
      mid:=(lo+hi) shr 1;
      compResult:=Compare(FItems[mid], item);
      if compResult<0 then
         lo:=mid+1
      else begin
         hi:=mid- 1;
         if compResult=0 then
            Result:=True;
      end;
   end;
   index:=lo;
end;

// InsertItem
//
procedure TSortedList<T>.InsertItem(index : Integer; const anItem : T);
begin
   if Count=Length(FItems) then
      SetLength(FItems, Count+8+(Count shr 4));
   if index<Count then
      System.Move(FItems[index], FItems[index+1], (Count-index)*SizeOf(Pointer));
   Inc(FCount);
   FItems[index]:=anItem;
end;

// Add
//
function TSortedList<T>.Add(const anItem : T) : Integer;
begin
   Find(anItem, Result{%H-});
   InsertItem(Result, anItem);
end;

// AddOrFind
//
function TSortedList<T>.AddOrFind(const anItem : T; var added : Boolean) : Integer;
begin
   added:=not Find(anItem, Result{%H-});
   if added then
      InsertItem(Result, anItem);
end;

// Extract
//
function TSortedList<T>.Extract(const anItem : T) : Integer;
begin
   if Find(anItem, Result{%H-}) then
      ExtractAt(Result)
   else Result:=-1;
end;

// ExtractAt
//
function TSortedList<T>.ExtractAt(index : Integer) : T;
var
   n : Integer;
begin
   Dec(FCount);
   Result:=FItems[index];
   n:=FCount-index;
   if n>0 then
      System.Move(FItems[index+1], FItems[index], n*SizeOf(T));
   SetLength(FItems, FCount);
end;

// IndexOf
//
function TSortedList<T>.IndexOf(const anItem : T) : Integer;
begin
   if not Find(anItem, Result{%H-}) then
      Result:=-1;
end;

// Clear
//
procedure TSortedList<T>.Clear;
begin
   SetLength(FItems, 0);
   FCount:=0;
end;

// Clean
//
procedure TSortedList<T>.Clean;
var
   i : Integer;
begin
   for i:=0 to FCount-1 do
      FItems[i].Free;
   Clear;
end;

// Enumerate
//
procedure TSortedList<T>.Enumerate(const callback : TSimpleCallback<T>);
var
   i : Integer;
begin
   for i:=0 to Count-1 do
      if callback(FItems[i])=csAbort then
         Break;
end;

// GetEnumerator
//
function TSortedList<T>.GetEnumerator : TSortedListEnumerator;
begin
   Result.FIndex := 0;
   Result.FList := Self;
   Result.FCountMinus1 := Count-1;
end;

// TSortedListEnumerator.MoveNext
//
function TSortedList<T>.TSortedListEnumerator.MoveNext: Boolean;
begin
   Result := FIndex < FCountMinus1;
   Inc(FIndex, Integer(Result));
end;

// TSortedListEnumerator.GetCurrent
//
function TSortedList<T>.TSortedListEnumerator.GetCurrent : T;
begin
   Result := FList.FItems[FIndex];
end;

// ------------------
// ------------------ TSimpleStack<T> ------------------
// ------------------

// Grow
//
procedure TSimpleStack<T>.Grow;
begin
   FCapacity:=FCapacity+8+(FCapacity shr 2);
   SetLength(FItems, FCapacity);
end;

// Push
//
procedure TSimpleStack<T>.Push(const item : T);
begin
   if FCount=FCapacity then Grow;
   FItems[FCount]:=item;
   Inc(FCount);
end;

// Pop
//
procedure TSimpleStack<T>.Pop;
begin
   Dec(FCount);
end;

// GetPeek
//
function TSimpleStack<T>.GetPeek : T;
begin
   Result:=FItems[FCount-1];
end;

// SetPeek
//
procedure TSimpleStack<T>.SetPeek(const item : T);
begin
   FItems[FCount-1]:=item;
end;

// GetItems
//
function TSimpleStack<T>.GetItems(const position : Integer) : T;
begin
   Result:=FItems[FCount-1-position];
end;

// SetItems
//
procedure TSimpleStack<T>.SetItems(const position : Integer; const value : T);
begin
   FItems[FCount-1-position]:=value;
end;

// Clear
//
procedure TSimpleStack<T>.Clear;
begin
   SetLength(FItems, 0);
   FCount:=0;
   FCapacity:=0;
end;

// ------------------
// ------------------ TWriteOnlyBlockStream ------------------
// ------------------

// Create
//
constructor TWriteOnlyBlockStream.Create;
begin
   inherited Create;
   AllocateCurrentBlock;
end;

// Destroy
//
destructor TWriteOnlyBlockStream.Destroy;
begin
   inherited;
   FreeBlocks;
end;

var
   vWOBSPool : Pointer;

// AllocFromPool
//
class function TWriteOnlyBlockStream.AllocFromPool : TWriteOnlyBlockStream;
begin
   Result:=InterlockedExchangePointer(vWOBSPool, nil);
   if Result=nil then
      Result:=TWriteOnlyBlockStream.Create;
end;

// ReturnToPool
//
procedure TWriteOnlyBlockStream.ReturnToPool;
var
   wobs : TWriteOnlyBlockStream;
begin
   if Self=nil then Exit;
   Clear;
   if vWOBSPool=nil then begin
      wobs:=InterlockedExchangePointer(vWOBSPool, Self);
      if wobs<>nil then
         wobs.Destroy;
   end else Destroy;
end;

// FreeBlocks
//
procedure TWriteOnlyBlockStream.FreeBlocks;
var
   iterator, next : PPointerArray;
begin
   iterator:=FFirstBlock;
   while iterator<>nil do begin
      next:=PPointerArray(iterator[0]);
      FreeMem(iterator);
      iterator:=next;
   end;
   FCurrentBlock:=nil;
   FFirstBlock:=nil;
   FTotalSize:=0;
end;

// AllocateCurrentBlock
//
procedure TWriteOnlyBlockStream.AllocateCurrentBlock;
var
   newBlock : PPointerArray;
begin
   GetMem(newBlock, cWriteOnlyBlockStreamBlockSize+2*SizeOf(Pointer));
   newBlock[0]:=nil;
   FBlockRemaining:=@newBlock[1];
   FBlockRemaining^:=0;

   if FCurrentBlock<>nil then
      FCurrentBlock[0]:=newBlock
   else FFirstBlock:=newBlock;
   FCurrentBlock:=newBlock;
end;

// Clear
//
procedure TWriteOnlyBlockStream.Clear;
begin
   FreeBlocks;
   AllocateCurrentBlock;
end;

// StoreData
//
procedure TWriteOnlyBlockStream.StoreData(var buffer);
var
   n : Integer;
   iterator : PPointerArray;
   dest : PByteArray;
begin
   dest:=@buffer;
   iterator:=FFirstBlock;
   while iterator<>nil do begin
      n:=PInteger(@iterator[1])^;
      if n>0 then begin
         System.Move(iterator[2], dest^, n);
         dest:=@dest[n];
      end;
      iterator:=iterator[0];
   end;
end;

// StoreData
//
procedure TWriteOnlyBlockStream.StoreData(destStream : TStream);
var
   n : Integer;
   iterator : PPointerArray;
begin
   iterator:=FFirstBlock;
   while iterator<>nil do begin
      n:=PInteger(@iterator[1])^;
      destStream.Write(iterator[2], n);
      iterator:=iterator[0];
   end;
end;

// StoreUTF8Data
//
procedure TWriteOnlyBlockStream.StoreData(var dest : UnicodeString);
begin
   if FTotalSize > 0 then begin

      Assert((FTotalSize and 1) = 0);
      SetLength(dest, FTotalSize div SizeOf(WideChar));
      StoreData(Pointer(dest)^);

   end else dest := '';
end;

// StoreUTF8Data
//
procedure TWriteOnlyBlockStream.StoreUTF8Data(destStream : TStream);
var
   buf : UTF8String;
begin
   buf := UTF8Encode(ToString);
   if buf <> '' then
      destStream.Write(Pointer(buf)^, Length(buf));
end;

// Seek
//
function TWriteOnlyBlockStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
   if (Origin=soFromCurrent) and (Offset=0) then
      Result:=FTotalSize
   else raise EStreamError.Create('not allowed');
end;

// Read
//
function TWriteOnlyBlockStream.{%H-}Read(var Buffer; Count: Longint): Longint;
begin
   raise EStreamError.Create('not allowed');
end;

// WriteSpanning
//
procedure TWriteOnlyBlockStream.WriteSpanning(source : PByteArray; count : Integer);
begin
   // current block contains some data, write fraction, allocate new block
   System.Move(source^, PByteArray(@FCurrentBlock[2])[FBlockRemaining^], count);
   FBlockRemaining^:=cWriteOnlyBlockStreamBlockSize;

   AllocateCurrentBlock;
end;

// WriteLarge
//
procedure TWriteOnlyBlockStream.WriteLarge(source : PByteArray; count : Integer);
var
   newBlock : PPointerArray;
begin
   // large amount still to be written, insert specific block
   newBlock:=GetMemory(count+2*SizeOf(Pointer));
   newBlock[0]:=FCurrentBlock;
   PInteger(@newBlock[1])^:=count;
   System.Move(source^, newBlock[2], count);
   FCurrentBlock[0]:=newBlock;
   FCurrentBlock:=newBlock;
   AllocateCurrentBlock;
end;

// WriteBuf
//
procedure TWriteOnlyBlockStream.WriteBuf(source : PByteArray; count : Integer);
type
   TThreeBytes = packed array [1..3] of Byte;
   PThreeBytes = ^TThreeBytes;
   TFiveBytes = packed array [1..5] of Byte;
   PFiveBytes = ^TFiveBytes;
   TSixBytes = packed array [1..6] of Byte;
   PSixBytes = ^TSixBytes;
   TSevenBytes = packed array [1..7] of Byte;
   PSevenBytes = ^TSevenBytes;
var
   dest : PByteArray;
   fraction : Integer;
begin
   Inc(FTotalSize, count);

   fraction := cWriteOnlyBlockStreamBlockSize-FBlockRemaining^;
   if count > fraction then begin
      // does not fit in current block
      // was current block started?
      if FBlockRemaining^ > 0 then begin
         WriteSpanning(source, fraction);
         Dec(count, fraction);
         source := @source[fraction];
      end;
      if count > cWriteOnlyBlockStreamBlockSize div 2 then begin
         WriteLarge(source, count);
         Exit;
      end;
   end;

   // if we reach here, everything fits in current block
   dest := @PByteArray(@FCurrentBlock[2])[FBlockRemaining^];
   Inc(FBlockRemaining^, count);
   {$ifdef WIN32}
   case Cardinal(count) of
      0 : ;
      1 : dest[0] := source[0];
      2 : PWord(dest)^ := PWord(source)^;
      3 : PThreeBytes(dest)^ := PThreeBytes(source)^;
      4 : PCardinal(dest)^ := PCardinal(source)^;
      5 : PFiveBytes(dest)^ := PFiveBytes(source)^;
      6 : PSixBytes(dest)^ := PSixBytes(source)^;
      7 : PSevenBytes(dest)^ := PSevenBytes(source)^;
      8 : PInt64(dest)^ := PInt64(source)^;
   else
      System.Move(source^, dest^, count);
   end;
   {$else}
   // "case of" got nerfed in non-32bit compilers, while System.Move was improved
   System.Move(source^, dest^, count);
   {$endif}
end;

// Write
//
function TWriteOnlyBlockStream.Write(const buffer; count: Longint): Longint;
begin
   WriteBuf(@buffer, count);
   Result:=count;
end;

// WriteByte
//
procedure TWriteOnlyBlockStream.WriteByte(b : Byte);
begin
   WriteBuf(@b, 1);
end;

// WriteShortInt
//
procedure TWriteOnlyBlockStream.WriteShortInt(s : ShortInt);
begin
   WriteBuf(@s, 1);
end;

// WriteWord
//
procedure TWriteOnlyBlockStream.WriteWord(w : Word);
begin
   WriteBuf(@w, 2);
end;

// WriteSmallInt
//
procedure TWriteOnlyBlockStream.WriteSmallInt(s : SmallInt);
begin
   WriteBuf(@s, 2);
end;

// WriteBytes
//
procedure TWriteOnlyBlockStream.WriteBytes(const b : array of Byte);
var
   n : Integer;
begin
   n:=Length(b);
   if n>0 then
      WriteBuf(@b[0], Length(b));
end;

// WriteBytes
//
procedure TWriteOnlyBlockStream.WriteBytes(const buffer : RawByteString);
var
   n : Integer;
begin
   n:=Length(buffer);
   if n>0 then
      WriteBuf(Pointer(buffer), Length(buffer));
end;

// WriteInt32
//
procedure TWriteOnlyBlockStream.WriteInt32(const i : Integer);
begin
   WriteBuf(@i, 4);
end;

// WriteInt64
//
procedure TWriteOnlyBlockStream.WriteInt64(const i : Int64);
begin
   WriteBuf(@i, 8);
end;

// WriteDWord
//
procedure TWriteOnlyBlockStream.WriteDWord(const dw : DWORD);
begin
   WriteBuf(@dw, 4);
end;

// WriteQWord
//
procedure TWriteOnlyBlockStream.WriteQWord(const qw : UInt64);
begin
   WriteBuf(@qw, 8);
end;

// WriteSingle
//
procedure TWriteOnlyBlockStream.WriteSingle(const s : Single);
begin
   WriteBuf(@s, 4);
end;

// WriteDouble
//
procedure TWriteOnlyBlockStream.WriteDouble(const d : Double);
begin
   WriteBuf(@d, 8);
end;

// WriteP
//
procedure TWriteOnlyBlockStream.WriteP(p : PWideChar; nbWideChars : Integer);
begin
   WriteBuf(Pointer(p), nbWideChars*SizeOf(WideChar));
end;

// WriteString
//
procedure TWriteOnlyBlockStream.WriteString(const utf16String : UnicodeString);
{$ifdef FPC}
begin
   if utf16String<>'' then
      WriteBuf(PByteArray(utf16String), Length(utf16String)*SizeOf(WideChar));
{$else}
var
   stringCracker : NativeUInt;
begin
   stringCracker:=NativeUInt(utf16String);
   if stringCracker<>0 then
      WriteBuf(Pointer(stringCracker), PInteger(stringCracker-SizeOf(Integer))^*SizeOf(WideChar));
{$endif}
end;

{$ifdef FPC}
// WriteString
//
procedure TWriteOnlyBlockStream.WriteString(const s : String);
begin
   if s<>'' then
      WriteString(UnicodeString(s));
end;
{$endif}

// WriteString (Int32)
//
procedure TWriteOnlyBlockStream.WriteString(const i : Int32);
var
   buf : TInt32StringBuffer;
   n : Integer;
begin
   n:=FastInt32ToBuffer(i, buf{%H-});
   WriteBuf(@buf[n], (High(buf)+1-n)*SizeOf(WideChar));
end;

// WriteString (Int64)
//
procedure TWriteOnlyBlockStream.WriteString(const i : Int64);
var
   buf : TInt64StringBuffer;
   n : Integer;
begin
   n:=FastInt64ToBuffer(i, buf{%H-});
   WriteBuf(@buf[n], (High(buf)+1-n)*SizeOf(WideChar));
end;

// WriteChar
//
procedure TWriteOnlyBlockStream.WriteChar(utf16Char : WideChar);
begin
   WriteBuf(@utf16Char, SizeOf(WideChar));
end;

// WriteDigits
//
procedure TWriteOnlyBlockStream.WriteDigits(value : Int64; digits : Integer);
var
   buf : array [0..19] of WideChar;
   n : Integer;
begin
   if digits<=0 then Exit;

   Assert(digits<Length(buf));
   n:=Length(buf);
   while digits>0 do begin
      Dec(n);
      if value<>0 then begin
         buf[n]:=WideChar(Ord('0')+(value mod 10));
         value:=value div 10;
      end else buf[n]:='0';
      Dec(digits);
   end;

   WriteBuf(@buf[n], (Length(buf)-n)*SizeOf(WideChar));
end;

// TailWord
//
function TWriteOnlyBlockStream.TailWord : Word;
var
   n : Integer;
   iter : PPointerArray;
begin
   if FTotalSize = 0 then Exit(0);

   n := FBlockRemaining^;
   if n > 1 then
      Exit(PWord(@PByteArray(@FCurrentBlock[2])[n-2])^)
   else begin
      Result := 0;
      iter := FFirstBlock;
      while iter <> nil do begin
         n := PInteger(@iter[1])^;
         if n > 1 then
            Result := PWord(@PByteArray(@iter[2])[n-2])^;
         iter := iter[0];
      end;
   end;
end;

// WriteDigits
//
procedure TWriteOnlyBlockStream.WriteDigits(value : Cardinal; digits : Integer);
var
   buf : array [0..10] of WideChar;
   n : Integer;
begin
   if digits<=0 then Exit;

   Assert(digits<Length(buf));
   n:=Length(buf);
   while digits>0 do begin
      Dec(n);
      if value<>0 then
         buf[n] := WideChar(Ord('0')+DivMod10(value))
      else buf[n] := '0';
      Dec(digits);
   end;

   WriteBuf(@buf[n], (Length(buf)-n)*SizeOf(WideChar));
end;

// ToString
//
function TWriteOnlyBlockStream.ToString : String;
begin
   StoreData(Result);
end;

// ToUnicodeString
//
function TWriteOnlyBlockStream.ToUnicodeString : UnicodeString;
begin
   StoreData(Result);
end;

// ToUTF8String
//
function TWriteOnlyBlockStream.ToUTF8String : RawByteString;
begin
   Result := UTF8Encode(ToString);
end;

// ToBytes
//
function TWriteOnlyBlockStream.ToBytes : TBytes;
var
   s : Int64;
begin
   s:=Size;
   SetLength(Result, s);
   if s>0 then
      StoreData(Result[0]);
end;

// ToRawBytes
//
function TWriteOnlyBlockStream.ToRawBytes : RawByteString;
var
   s : Int64;
begin
   s:=Size;
   SetLength(Result, s);
   if s>0 then
      StoreData(Pointer(Result)^);
end;

// GetSize
//
function TWriteOnlyBlockStream.GetSize: Int64;
begin
   Result:=FTotalSize;
end;

// WriteSubString
//
procedure TWriteOnlyBlockStream.WriteSubString(const utf16String : UnicodeString; startPos : Integer);
begin
   WriteSubString(utf16String, startPos, Length(utf16String)-startPos+1);
end;

// WriteSubString
//
procedure TWriteOnlyBlockStream.WriteSubString(const utf16String : UnicodeString;
                                               startPos, aLength : Integer);
var
   p, n : Integer;
begin
   Assert(startPos>=1);

   if aLength<=0 then Exit;
   p:=startPos+aLength-1;

   n:=System.Length(utf16String);
   if startPos>n then Exit;

   if p>n then
      n:=n-startPos+1
   else n:=p-startPos+1;

   if n>0 then
      WriteBuf(@utf16String[startPos], n*SizeOf(WideChar));
end;

// WriteUTF8String
//
procedure TWriteOnlyBlockStream.WriteUTF8String(const utf8String : RawByteString);
begin
   WriteBuf(Pointer(utf8String), Length(utf8String));
end;

// WriteCRLF
//
procedure TWriteOnlyBlockStream.WriteCRLF;
begin
   WriteBuf(@cCRLF[0], 2*SizeOf(WideChar));
end;

// WriteAsciiCRLF
//
procedure TWriteOnlyBlockStream.WriteAsciiCRLF;
begin
   WriteBuf(@cAsciiCRLF[0], 2*SizeOf(AnsiChar));
end;

// WriteIndent
//
procedure TWriteOnlyBlockStream.WriteIndent(nb : Integer; indentChar : WideChar = ' ');
begin
   while nb > 0 do begin
      WriteChar(indentChar);
      Dec(nb);
   end;
end;

// ------------------
// ------------------ TTightStack ------------------
// ------------------

// Grow
//
procedure TTightStack.Grow;
begin
   FCapacity:=FCapacity+8+FCapacity shr 1;
   ReallocMem(FList, FCapacity*SizeOf(Pointer));
end;

// Push
//
procedure TTightStack.Push(item : TRefCountedObject);
begin
   if FCount=FCapacity then Grow;
   FList[FCount]:=item;
   Inc(FCount);
end;

// Peek
//
function TTightStack.Peek : TRefCountedObject;
begin
   if FCount>0 then
      Result:=FList[FCount-1]
   else Result:=nil;
end;

// Peek
//
function TTightStack.Peek(n : Integer) : TRefCountedObject;
begin
   if n>=0 then
      n:=FCount-n-1;
   if n<0 then
      Result:=nil
   else Result:=FList[n];
end;

// Pop
//
procedure TTightStack.Pop;
begin
   Dec(FCount);
end;

// PeekAndPop
//
function TTightStack.PeekAndPop : TRefCountedObject;
begin
   var n := FCount;
   if n > 0 then begin
      Result := FList[n-1];
      FCount := n-1;
   end else Result := nil;
end;

// Clear
//
procedure TTightStack.Clear;
begin
   FCount:=0;
end;

// Clean
//
procedure TTightStack.Clean;
begin
   while Count>0 do begin
      TRefCountedObject(Peek).Free;
      Pop;
   end;
end;

// Free
//
procedure TTightStack.Free;
begin
   FCount:=0;
   FCapacity:=0;
   FreeMem(FList);
   FList:=nil;
end;

// TransferSimpleHashBuckets
//
procedure TransferSimpleHashBuckets(const src, dest; nbSrcBuckets, nbDestBuckets, bucketSize : Integer);
var
   ptrOld, ptrNew : IntPtr;
   destMask : Integer;
   i, j : Integer;
begin
   ptrOld := IntPtr(@src);
   destMask := nbDestBuckets - 1;
   for i := nbSrcBuckets downto 1 do begin
      if PCardinal(ptrOld)^ <> 0 then begin
         j := PCardinal(ptrOld)^ and destMask;
         ptrNew := IntPtr(@dest) + j * bucketSize;
         while PCardinal(ptrNew)^ <> 0 do begin
            if j = destMask then begin
               j := 0;
               ptrNew := IntPtr(@dest);
            end else begin
               Inc(j);
               Inc(ptrNew, bucketSize);
            end;
         end;
         j := bucketSize;
         while j >= 8 do begin
            PUInt64(ptrNew)^ := PUInt64(ptrOld)^;
            PUInt64(ptrOld)^ := 0;
            Inc(ptrNew, 8);
            Inc(ptrOld, 8);
            Dec(j, 8);
         end;
         if j = 4 then begin
            PUInt32(ptrNew)^ := PUInt32(ptrOld)^;
            PUInt32(ptrOld)^ := 0;
            Inc(ptrOld, 4);
         end;
      end else Inc(ptrOld, bucketSize);
   end;
end;

// QuickSortDoublePrecision
//
procedure QuickSortDoublePrecision(a : PDoubleArray; minIndex, maxIndex : NativeInt);

   procedure Swap(a : PDoubleArray; i1, i2 : NativeInt); inline;
   begin
      var buf : Double := a[i1];
      a[i1] := a[i2];
      a[i2] := buf;
   end;

var
   i, j, p, n : NativeInt;
begin
   n := maxIndex-minIndex;
   case n of
      1 : begin
         var v1 := a[minIndex];
         var v2 := a[maxIndex];
         if v1 > v2 then begin
            a[minIndex] := v2;
            a[maxIndex] := v1;
         end;
      end;
      2 : begin
         i := minIndex+1;
         if a[minIndex] > a[i] then
            Swap(a, minIndex, i);
         if a[i] > a[maxIndex] then begin
            Swap(a, i, maxIndex);
            if a[minIndex] > a[i] then
               Swap(a, minIndex, i);
         end;
      end;
   else
      if n <= 0 then Exit;
      repeat
         i := minIndex;
         j := maxIndex;
         p := ((i+j) shr 1);
         repeat
            var pivotValue : Double := a[p];
            while a[i] < pivotValue do Inc(i);
            while a[j] > pivotValue do Dec(j);
            if i <= j then begin
               if i <> j then begin
                  var v1 := a[i];
                  var v2 := a[j];
                  a[i] := v2;
                  a[j] := v1;
               end;
               if p = i then
                  p := j
               else if p = j then
                  p := i;
               Inc(i);
               Dec(j);
            end;
         until i > j;
         if minIndex < j then
            QuickSortDoublePrecision(a, minIndex, j);
         minIndex := i;
      until i >= maxIndex;
   end;
end;

// QuickSortInt64
//
procedure QuickSortInt64(a : PInt64Array; minIndex, maxIndex : NativeInt);

   procedure Swap(a : PInt64Array; i1, i2 : NativeInt); inline;
   begin
      var buf : Int64 := a[i1];
      a[i1] := a[i2];
      a[i2] := buf;
   end;

var
   i, j, p, n : NativeInt;
begin
   n := maxIndex-minIndex;
   case n of
      1 : begin
         var v1 := a[minIndex];
         var v2 := a[maxIndex];
         if v1 > v2 then begin
            a[minIndex] := v2;
            a[maxIndex] := v1;
         end;
      end;
      2 : begin
         i := minIndex+1;
         if a[minIndex] > a[i] then
            Swap(a, minIndex, i);
         if a[i] > a[maxIndex] then begin
            Swap(a, i, maxIndex);
            if a[minIndex] > a[i] then
               Swap(a, minIndex, i);
         end;
      end;
   else
      if n <= 0 then Exit;
      repeat
         i := minIndex;
         j := maxIndex;
         p := ((i+j) shr 1);
         repeat
            var pivotValue : Int64 := a[p];
            while a[i] < pivotValue do Inc(i);
            while a[j] > pivotValue do Dec(j);
            if i <= j then begin
               if i <> j then begin
                  var v1 := a[i];
                  var v2 := a[j];
                  a[i] := v2;
                  a[j] := v1;
               end;
               if p = i then
                  p := j
               else if p = j then
                  p := i;
               Inc(i);
               Dec(j);
            end;
         until i > j;
         if minIndex < j then
            QuickSortInt64(a, minIndex, j);
         minIndex := i;
      until i >= maxIndex;
   end;
end;

// QuickSortString
//
procedure QuickSortString(a : PStringArray; minIndex, maxIndex : NativeInt);

   procedure Swap(a : PStringArray; i1, i2 : NativeInt); inline;
   begin
      var buf := Pointer(a[i1]);
      Pointer(a[i1]) := Pointer(a[i2]);
      Pointer(a[i2]) := buf;
   end;

var
   i, j, p, n : NativeInt;
begin
   n := maxIndex-minIndex;
   case n of
      1 : begin
         if a[minIndex] > a[maxIndex] then
            Swap(a, minIndex, maxIndex);
      end;
      2 : begin
         i := minIndex+1;
         if a[minIndex] > a[i] then
            Swap(a, minIndex, i);
         if a[i] > a[maxIndex] then begin
            Swap(a, i, maxIndex);
            if a[minIndex] > a[i] then
               Swap(a, minIndex, i);
         end;
      end;
   else
      if n <= 0 then Exit;
      repeat
         i := minIndex;
         j := maxIndex;
         p := ((i+j) shr 1);
         repeat
            while a[i] < a[p] do Inc(i);
            while a[j] > a[p] do Dec(j);
            if i <= j then begin
               if i <> j then
                  Swap(a, i, j);
               if p = i then
                  p := j
               else if p = j then
                  p := i;
               Inc(i);
               Dec(j);
            end;
         until i > j;
         if minIndex < j then
            QuickSortString(a, minIndex, j);
         minIndex := i;
      until i >= maxIndex;
   end;
end;

// QuickSortVariant
//
procedure QuickSortVariant(a : PVariantArray; minIndex, maxIndex : NativeInt);

   procedure Swap(a : PVariantArray; i1, i2 : NativeInt); inline;
   begin
      var buf := TVarData(a[i1]);
      TVarData(a[i1]) := TVarData(a[i2]);
      TVarData(a[i2]) := buf;
   end;

var
   i, j, p, n : NativeInt;
begin
   n := maxIndex-minIndex;
   case n of
      1 : begin
         if VarCompareSafe(a[minIndex], a[maxIndex]) = vrGreaterThan then
            Swap(a, minIndex, maxIndex);
      end;
      2 : begin
         i := minIndex+1;
         if VarCompareSafe(a[minIndex], a[i]) = vrGreaterThan then
            Swap(a, minIndex, i);
         if VarCompareSafe(a[i], a[maxIndex]) = vrGreaterThan then begin
            Swap(a, i, maxIndex);
            if VarCompareSafe(a[minIndex], a[i]) = vrGreaterThan then
               Swap(a, minIndex, i);
         end;
      end;
   else
      if n <= 0 then Exit;
      repeat
         i := minIndex;
         j := maxIndex;
         p := ((i+j) shr 1);
         repeat
            while VarCompareSafe(a[i], a[p]) = vrLessThan do Inc(i);
            while VarCompareSafe(a[j], a[p]) = vrGreaterThan do Dec(j);
            if i <= j then begin
               if i <> j then
                  Swap(a, i, j);
               if p = i then
                  p := j
               else if p = j then
                  p := i;
               Inc(i);
               Dec(j);
            end;
         until i > j;
         if minIndex < j then
            QuickSortVariant(a, minIndex, j);
         minIndex := i;
      until i >= maxIndex;
   end;
end;

// Grow
//
procedure TSimpleHash<T>.Grow(capacityPreAdjusted : Boolean);
var
   {$IFDEF DELPHI_XE3}
   oldBuckets : array of TSimpleHashBucket<T>;
   {$ELSE}
   oldBuckets : TSimpleHashBucketArray<T>;
   {$ENDIF}
begin
   if not capacityPreAdjusted then begin
      if FCapacity = 0 then
         FCapacity := cSimpleHashMinCapacity
      else FCapacity := FCapacity*2;
   end;
   FGrowth := (FCapacity*11) div 16;

   if FBuckets = nil then begin
      SetLength(FBuckets, FCapacity);
      Exit;
   end;

   {$IFDEF DELPHI_XE3}
   SetLength(oldBuckets, Length(FBuckets));
   for i := 0 to Length(FBuckets) - 1 do
     oldBuckets[i] := FBuckets[i];
   {$ELSE}
   oldBuckets := FBuckets;
   {$ENDIF}

   FBuckets := nil;
   SetLength(FBuckets, FCapacity);

   TransferSimpleHashBuckets(
      oldBuckets[0], FBuckets[0],
      Length(oldBuckets), FCapacity,
      SizeOf(TSimpleHashBucket<T>)
   );
end;

// LinearFind
//
function TSimpleHash<T>.LinearFind(const item : T; var index : Integer) : Boolean;
begin
   repeat
      if FBuckets[index].HashCode=0 then
         Exit(False)
      else if SameItem(item, FBuckets[index].Value) then
         Exit(True);
      index:=(index+1) and (FCapacity-1);
   until False;
end;

// Add
//
function TSimpleHash<T>.Add(const anItem : T) : Boolean;
begin
   Result := AddHashed(anItem, GetItemHashCode(anItem));
end;

// AddHashed
//
function TSimpleHash<T>.AddHashed(const anItem : T; const hashCode : Cardinal) : Boolean;
var
   i : Integer;
begin
   if FCount>=FGrowth then Grow(False);
   i := (hashCode and (FCapacity-1));
   if LinearFind(anItem, i) then Exit(False);
   FBuckets[i].HashCode:=hashCode;
   FBuckets[i].Value:=anItem;
   Inc(FCount);
   Result:=True;
end;

// Replace
//
function TSimpleHash<T>.Replace(const anItem : T) : Boolean;
var
   i : Integer;
   hashCode : Integer;
begin
   if FCount>=FGrowth then Grow(False);

   hashCode:=GetItemHashCode(anItem);
   i:=(hashCode and (FCapacity-1));
   if LinearFind(anItem, i) then begin
      FBuckets[i].Value:=anItem;
      Result := False;
   end else begin
      FBuckets[i].HashCode:=hashCode;
      FBuckets[i].Value:=anItem;
      Inc(FCount);
      Result:=True;
   end;
end;

// Contains
//
function TSimpleHash<T>.Contains(const anItem : T) : Boolean;
var
   i : Integer;
begin
   if FCount=0 then Exit(False);
   i:=(GetItemHashCode(anItem) and (FCapacity-1));
   Result:=LinearFind(anItem, i);
end;

// Match
//
function TSimpleHash<T>.Match(var anItem : T) : Boolean;
var
   i : Integer;
begin
   if FCount=0 then Exit(False);
   i:=(GetItemHashCode(anItem) and (FCapacity-1));
   Result:=LinearFind(anItem, i);
   if Result then
      anItem:=FBuckets[i].Value;
end;

// Remove
//
function TSimpleHash<T>.Remove(const anItem : T) : Boolean;
var
   i : Integer;
begin
   if FCount=0 then Exit(False);
   i := (GetItemHashCode(anItem) and (FCapacity-1));
   Result := LinearFind(anItem, i);
   if Result then Delete(i);
end;

// Delete
//
procedure TSimpleHash<T>.Delete(index : Integer);
var
   k, gap : Integer;
begin
   if FBuckets[index].HashCode = 0 then Exit;

   gap := index;
   repeat
      index := (index+1) and (FCapacity-1);

      if FBuckets[index].HashCode = 0 then
         Break;

      k := FBuckets[index].HashCode and (FCapacity - 1);

      if ((gap >= k) or (k > index)) and ((index >= gap) or (k <= gap)) and ((index >= gap) or (k > index)) then begin
         FBuckets[gap] := FBuckets[index];
         FBuckets[index].HashCode := 0;
         gap := index;
      end;
   until False;

   FBuckets[gap].HashCode := 0;
   FBuckets[gap].Value := Default(T);
   Dec(FCount);
end;


// Enumerate
//
procedure TSimpleHash<T>.Enumerate(const callBack : TSimpleHashFunc<T>);
var
   i, initialCount : Integer;
begin
   if FCount = 0 then Exit;
   initialCount := FCount;
   for i := 0 to High(FBuckets) do begin
      with FBuckets[i] do if HashCode <> 0 then begin
         case callBack(Value) of
            shaRemove : begin
               HashCode := 0;
               Value := Default(T);
               Dec(FCount);
            end;
            shaAbort : break;
         end;
      end;
   end;
   if FCount < initialCount then begin
      case FCount of
         0 : Clear;
         1 .. cSimpleHashMinCapacity div 2 : begin
            FCapacity := cSimpleHashMinCapacity;
            Grow(True);
         end;
      else
         // adjust capacity
         i := 2 * FCount;
         if i < cSimpleHashMinCapacity then
            i  := cSimpleHashMinCapacity;
         while FCapacity > i do
            FCapacity := FCapacity div 2;
         Grow(True);
      end;
   end;
end;

// Clear
//
procedure TSimpleHash<T>.Clear(resetCapacity : Boolean = True);
var
   i : Integer;
begin
   FCount:=0;
   if resetCapacity then begin
      FCapacity:=0;
      FGrowth:=0;
      FBuckets:=nil;
   end else begin
      for i := 0 to High(FBuckets) do begin
         if FBuckets[i].HashCode <> 0 then begin
            FBuckets[i].HashCode := 0;
            FBuckets[i].Value := Default(T);
         end;
      end;
   end;
end;

// PreallocateCapacity
//
procedure TSimpleHash<T>.PreallocateCapacity(newCapacity : Integer);
var
   cap : Integer;
begin
   cap := FCapacity;
   if cap = 0 then
      cap := cSimpleHashMinCapacity;
   while cap < newCapacity do
      cap := 2*cap;
   FCapacity := cap;
   Grow(True);
end;

// HashBucketValue
//
function TSimpleHash<T>.HashBucketValue(index : Integer; var anItem : T) : Boolean;
begin
   with FBuckets[index] do begin
      if HashCode <> 0 then begin
         anItem := Value;
         Result := True;
      end else Result := False;
   end;
end;

// ------------------
// ------------------ TSimpleObjectHash<T> ------------------
// ------------------

// SameItem
//
function TSimpleObjectHash<T>.SameItem(const item1, item2 : T) : Boolean;
begin
   Result:=(item1=item2);
end;

// GetItemHashCode
//
function TSimpleObjectHash<T>.GetItemHashCode(const item1 : T) : Cardinal;
var
   p : NativeUInt;
begin
   p:=PNativeUInt(@item1)^; // workaround compiler issue
   Result:=(p shr 4)+1;
end;

// Clean
//
procedure TSimpleObjectHash<T>.Clean;
var
   i : Integer;
begin
   for i:=0 to FCapacity-1 do
      if FBuckets[i].HashCode<>0 then
         FBuckets[i].Value.Free;
   Clear;
end;

// ------------------
// ------------------ TSimpleList<T> ------------------
// ------------------

// Add
//
procedure TSimpleList<T>.Add(const item : T);
begin
   if FCount=FCapacity then Grow;
   FItems[FCount]:=item;
   Inc(FCount);
end;

// Extract
//
procedure TSimpleList<T>.Extract(idx : Integer);
var
   n : Integer;
begin
   FItems[idx]:=Default(T);
   n:=FCount-idx-1;
   if n>0 then begin
      Move(FItems[idx+1], FItems[idx], n*SizeOf(T));
      FillChar(FItems[FCount-1], SizeOf(T), 0);
   end;
   Dec(FCount);
end;

// Clear
//
procedure TSimpleList<T>.Clear;
begin
   SetLength(FItems, 0);
   FCapacity:=0;
   FCount:=0;
end;

// Enumerate
//
procedure TSimpleList<T>.Enumerate(const callback : TSimpleCallback<T>);
var
   i : Integer;
begin
   for i:=0 to Count-1 do
      if callBack(FItems[i])=csAbort then
         Break;
end;

// ItemPtr
//
function TSimpleList<T>.ItemPtr(idx : Integer) : PItemPtr;
begin
   Result := @FItems[idx];
end;

// Grow
//
procedure TSimpleList<T>.Grow;
begin
   FCapacity:=FCapacity+8+(FCapacity shr 2);
   SetLength(FItems, FCapacity);
end;

// GetItems
//
function TSimpleList<T>.GetItems(const idx : Integer) : T;
begin
   Result:=FItems[idx];
end;

// SetItems
//
procedure TSimpleList<T>.SetItems(const idx : Integer; const value : T);
begin
   FItems[idx]:=value;
end;

// ------------------
// ------------------ TObjectsLookup ------------------
// ------------------

// Compare
//
function TObjectsLookup.Compare(const item1, item2 : TRefCountedObject) : Integer;
begin
   if NativeUInt(item1)<NativeUInt(item2) then
      Result:=-1
   else if NativeUInt(item1)=NativeUInt(item2) then
      Result:=0
   else Result:=1;
end;

// ------------------
// ------------------ TInterfacedSelfObject ------------------
// ------------------

// GetSelf
//
function TInterfacedSelfObject.GetSelf : TObject;
begin
   Result:=Self;
end;

// QueryInterface
//
function TInterfacedSelfObject.QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} IID: TGUID; out Obj): HResult;
begin
   if GetInterface(IID, Obj) then
      Result:=0
   else Result:=E_NOINTERFACE;
end;

// _AddRef
//
function TInterfacedSelfObject._AddRef: Integer;
begin
   Result:=IncRefCount;
end;

// _Release
//
function TInterfacedSelfObject._Release: Integer;
begin
   Result:=DecRefCount;
   if Result=0 then Destroy;
end;

// NewInstance
//
class function TInterfacedSelfObject.NewInstance: TObject;
begin
   Result:=inherited NewInstance;
   TRefCountedObject(Result).IncRefCount;
end;

// AfterConstruction
//
procedure TInterfacedSelfObject.AfterConstruction;
begin
   // Release the constructor's implicit refcount
   DecRefCount;
end;

// BeforeDestruction
//
procedure TInterfacedSelfObject.BeforeDestruction;
begin
   Assert(RefCount=0);
end;

// ScriptTypeName
//
function TInterfacedSelfObject.ScriptTypeName : String;
begin
   Result := ClassName;
end;

// ------------------
// ------------------ TAutoStrings ------------------
// ------------------

// Create
//
constructor TAutoStrings.Create;
begin
   FValue:=TStringList.Create;
end;

// Destroy
//
destructor TAutoStrings.Destroy;
begin
   FValue.Free;
end;

// CreateCapture
//
constructor TAutoStrings.CreateCapture(value : TStringList);
begin
   Assert(value <> nil);
   FValue:=value;
end;

// CreateClone
//
constructor TAutoStrings.CreateClone(value : TStringList);
var
   sl : TStringList;
begin
   sl:=TStringList.Create;
   FValue:=sl;
   sl.Assign(value);
   sl.CaseSensitive:=value.CaseSensitive;
   sl.Sorted:=value.Sorted;
end;

// GetValue
//
function TAutoStrings.GetValue : TStringList;
begin
   Result:=FValue;
end;

// Clone
//
function TAutoStrings.Clone : IAutoStrings;
begin
   Result:=TAutoStrings.CreateClone(FValue);
end;

// GetItem
//
function TAutoStrings.GetItem(index : Integer) : String;
begin
   Result := FValue[index];
end;

// SetItem
//
procedure TAutoStrings.SetItem(index : Integer; const value : String);
begin
   FValue[index] := value;
end;

// Count
//
function TAutoStrings.Count : Integer;
begin
   Result := FValue.Count;
end;

// IndexOfName
//
function TAutoStrings.IndexOfName(const name : String) : Integer;
begin
   Result := FValue.IndexOfName(name);
end;

// GetValues
//
function TAutoStrings.GetValues(const name : String) : String;
begin
   Result := FValue.Values[name];
end;

// SetValues
//
procedure TAutoStrings.SetValues(const name, value : String);
begin
   FValue.Values[name] := value;
end;

// ------------------
// ------------------ TNameObjectHash<T> ------------------
// ------------------

// Create
//
constructor TNameObjectHash.Create(initialCapacity : Integer = 0);
var
   n : Integer;
begin
   if initialCapacity>0 then begin
      // find nearest power of two >= initialCapacity
      n:=cNameObjectHashMinSize;
      while n<initialCapacity do
         n:=n shl 1;
      initialCapacity:=n;
      SetLength(FBuckets, n);
   end;
   FHighIndex:=initialCapacity-1;
end;

// HashName
//
class function TNameObjectHash.HashName(const aName : String) : Cardinal;
begin
   Result := xxHash32.Full(Pointer(aName), Length(aName)*SizeOf(Char), vHashSalt);
   if Result=0 then
      Result:=1;
end;

// Grow
//
procedure TNameObjectHash.Grow;
begin
   if FHighIndex<cNameObjectHashMinSize-1 then
      Resize(cNameObjectHashMinSize)
   else Resize(FHighIndex*2+2);
end;

// Pack
//
procedure TNameObjectHash.Pack;
var
   i, m : Integer;
begin
   case FCount of
      0 : Clear;
      1..cNameObjectHashMinSize shr 1 : Resize(cNameObjectHashMinSize);
   else
      i:=FHighIndex+1;
      m:=FCount*3; // = 2 * FCount*(3/2)
      while i>=m do
         i:=i shr 1;
      Resize(i);
   end;
end;

// Names
//
function TNameObjectHash.Names : TStringDynArray;
var
   i, k : Integer;
begin
   k := 0;
   SetLength(Result, FHighIndex);
   for i := 0 to FHighIndex do begin
      if FBuckets[i].HashCode <> 0 then begin
         Result[k] := FBuckets[i].Name;
         Inc(k);
      end;
   end;
   SetLength(Result, k);
end;

// Resize
//
procedure TNameObjectHash.Resize(newSize : Integer);

   procedure MoveBucket(var src, dest : TNameObjectHashBucket); inline;
   begin
      dest.HashCode:=src.HashCode;
      Pointer(dest.Name):=Pointer(src.Name);
      Pointer(src.Name):=nil;
      dest.Obj:=src.Obj;
   end;

var
   i, j, n : Integer;
   mask : Integer;
   oldBuckets : TNameObjectHashBuckets;
   oldBucket : PNameObjectHashBucket;
begin
   Assert(newSize>FCount);  // protect from infinite loop

   oldBuckets:=FBuckets;
   FBuckets:=nil;
   SetLength(FBuckets, newSize);
   FHighIndex:=newSize-1;
   FGrowth:=(FHighIndex*3) div 4;

   if FCount=0 then Exit;

   n:=0;
   mask:=newSize-1;
   oldBucket:=@oldBuckets[0];
   for i:=0 to High(oldBuckets) do begin
      if oldBucket.HashCode<>0 then begin
         Inc(n);
         j:=(oldBucket.HashCode and mask);
         while FBuckets[j].HashCode<>0 do
            j:=(j+1) and mask;
         MoveBucket(oldBucket^, FBuckets[j]);
      end;
      Inc(oldBucket);
   end;
   FCount:=n;
end;

// GetHashedIndex
//
function TNameObjectHash.GetHashedIndex(const aName : String; aHash : Cardinal) : Integer;
var
   i : Integer;
begin
   if FCount=0 then Exit(-1);

   i:=aHash and FHighIndex;

   repeat
      with FBuckets[i] do begin
         if HashCode=0 then
            Exit(-1)
         else if (HashCode=aHash) and (Name=aName) then
            Exit(i);
      end;
      i:=(i+1) and FHighIndex;
   until False;
end;

// GetIndex
//
function TNameObjectHash.GetIndex(const aName : String) : Integer;
begin
   if Count = 0 then
      Result := -1
   else Result := GetHashedIndex(aName, HashName(aName));
end;

// GetHashedObjects
//
function TNameObjectHash.GetHashedObjects(const aName : String; aHash : Cardinal) : TObject;
var
   i : Integer;
begin
   i:=GetHashedIndex(aName, aHash);
   if i<0 then
      Result:=nil
   else Result:=FBuckets[i].Obj
end;

// GetObjects
//
function TNameObjectHash.GetObjects(const aName : String) : TObject;
begin
   if Count = 0 then
      Result := nil
   else Result := GetHashedObjects(aName, HashName(aName));
end;

// SetHashedObjects
//
procedure TNameObjectHash.SetHashedObjects(const aName : String; aHash : Cardinal; obj : TObject);
begin
   AddHashedObject(aName, HashName(aName), obj, True);
end;

// SetObjects
//
procedure TNameObjectHash.SetObjects(const aName : String; obj : TObject);
begin
   AddObject(aName, obj, True);
end;

// AddObject
//
function TNameObjectHash.AddObject(const aName : String; aObj : TObject;
                                            replace : Boolean = False) : Boolean;
begin
   Result:=AddHashedObject(aName, HashName(aName), aObj, replace);
end;

// AddHashedObject
//
function TNameObjectHash.AddHashedObject(const aName : String; aHash : Cardinal; aObj : TObject; replace : Boolean = False) : Boolean;
var
   i : Integer;
begin
   if FCount>=FGrowth then Grow;

   i:=aHash and FHighIndex;

   repeat
      with FBuckets[i] do begin
         if HashCode=0 then
            Break
         else if (HashCode=aHash) and (Name=aName) then begin
            if replace then
               Obj:=aObj;
            Exit(False);
         end;
      end;
      i:=(i+1) and FHighIndex;
   until False;

   with FBuckets[i] do begin
      HashCode:=aHash;
      Name:=aName;
      Obj:=aObj;
   end;
   Inc(FCount);
   Result:=True;
end;

// Clean
//
procedure TNameObjectHash.Clean;
var
   i : Integer;
begin
   for i:=0 to FHighIndex do begin
      if FBuckets[i].HashCode<>0 then
         dwsFreeAndNil(FBuckets[i].Obj);
   end;
   Clear;
end;

// Clear
//
procedure TNameObjectHash.Clear;
begin
   SetLength(FBuckets, 0);
   FGrowth:=0;
   FCount:=0;
   FHighIndex:=-1;
end;

// GetBucket
//
function TNameObjectHash.GetBucket(index : Integer) : PNameObjectHashBucket;
begin
   Result:=@FBuckets[index];
end;

// GetBucketName
//
function TNameObjectHash.GetBucketName(index : Integer) : String;
begin
   Result:=FBuckets[index].Name;
end;

// GetBucketObject
//
function TNameObjectHash.GetBucketObject(index : Integer) : TObject;
begin
   Result:=FBuckets[index].Obj;
end;

// SetBucketObject
//
procedure TNameObjectHash.SetBucketObject(index : Integer; obj : TObject);
begin
   FBuckets[index].Obj:=obj;
end;

// ------------------
// ------------------ TSimpleNameObjectHash<T> ------------------
// ------------------

// Create
//
constructor TSimpleNameObjectHash<T>.Create(initialCapacity : Integer = 0);
begin
   FHash:=TNameObjectHash.Create(initialCapacity);
end;

// Destroy
//
destructor TSimpleNameObjectHash<T>.Destroy;
begin
   FHash.Free;
end;

// Pack
//
procedure TSimpleNameObjectHash<T>.Pack;
begin
   FHash.Pack;
end;

// GetIndex
//
function TSimpleNameObjectHash<T>.GetIndex(const aName : String) : Integer;
begin
   Result := FHash.BucketIndex[aName];
end;

// GetObjects
//
function TSimpleNameObjectHash<T>.GetObjects(const aName : String) : T;
begin
   Result:=T(FHash.Objects[aName]);
end;

// SetObjects
//
procedure TSimpleNameObjectHash<T>.SetObjects(const aName : String; obj : T);
begin
   FHash.Objects[aName] := obj;
end;

// AddObject
//
function TSimpleNameObjectHash<T>.AddObject(const aName : String; aObj : T;
                                            replace : Boolean = False) : Boolean;
begin
   Result:=FHash.AddObject(aName, aObj, replace);
end;

// Clean
//
procedure TSimpleNameObjectHash<T>.Clean;
begin
   FHash.Clean;
end;

// Clear
//
procedure TSimpleNameObjectHash<T>.Clear;
begin
   FHash.Clear;
end;

// GetBucketName
//
function TSimpleNameObjectHash<T>.GetBucketName(index : Integer) : String;
begin
   Result := FHash.BucketName[index];
end;

// GetBucketObject
//
function TSimpleNameObjectHash<T>.GetBucketObject(index : Integer) : T;
begin
   Result:=T(FHash.BucketObject[index]);
end;

// SetBucketObject
//
procedure TSimpleNameObjectHash<T>.SetBucketObject(index : Integer; obj : T);
begin
   FHash.BucketObject[index] := obj;
end;

// Count
//
function TSimpleNameObjectHash<T>.Count : Integer;
begin
   Result:=FHash.Count;
end;

// HighIndex
//
function TSimpleNameObjectHash<T>.HighIndex : Integer;
begin
   Result:=FHash.HighIndex;
end;

// ------------------
// ------------------ TRefCountedObject ------------------
// ------------------

{$ifdef DOUBLE_FREE_PROTECTOR}
constructor TRefCountedObject.Create;
begin
   inherited;
   FDoubleFreeProtector := $0102030405060708;
end;

destructor TRefCountedObject.Destroy;
begin
   Assert(FDoubleFreeProtector = $0102030405060708, ClassName);
   FDoubleFreeProtector := $0807060504030201;
   inherited;
end;
{$endif}

// Free
//
procedure TRefCountedObject.Free;
begin
   if Self <> nil then
      DecRefCount;
end;

// IncRefCount
//
function TRefCountedObject.IncRefCount : Integer;
begin
   {$ifdef USE_MONITOR_FOR_REFCOUNT}
   var p := PInteger(NativeInt(Self)+InstanceSize-hfFieldSize+hfMonitorOffset);
   Result := AtomicIncrement(p^);
   {$else}
   Result := AtomicIncrement(FRefCount);
   {$endif}
end;

// DecRefCount
//
procedure CallDestroy(obj : TObject);
begin
   obj.Destroy;
end;
function TRefCountedObject.DecRefCount : Integer;

{$ifdef USE_MONITOR_FOR_REFCOUNT}
begin
   var p := PInteger(NativeInt(Self)+InstanceSize-hfFieldSize+hfMonitorOffset);
   if p^=0 then begin
      Destroy;
      Result:=0;
   end else Result := AtomicDecrement(p^);
end;
{$else}
begin
   if FRefCount = 0 then begin
      Destroy;
      Result := 0;
   end else begin
      Result := AtomicDecrement(FRefCount);
   end;
end;
{$endif}

// GetRefCount
//
function TRefCountedObject.GetRefCount : Integer;
 {$ifdef USE_MONITOR_FOR_REFCOUNT}
var
   p : PInteger;
begin
   p:=PInteger(NativeInt(Self)+InstanceSize-hfFieldSize+hfMonitorOffset);
   Result:=p^;
end;
{$else}
begin
   Result := FRefCount;
end;
{$endif}

// SetRefCount
//
procedure TRefCountedObject.SetRefCount(n : Integer);
{$ifdef USE_MONITOR_FOR_REFCOUNT}
var
   p : PInteger;
begin
   p:=PInteger(NativeInt(Self)+InstanceSize-hfFieldSize+hfMonitorOffset);
   p^:=n;
end;
{$else}
begin
   FRefCount := n;
end;
{$endif}


// ------------------
// ------------------ TSimpleObjectObjectHash<T1, T2> ------------------
// ------------------

// Grow
//
procedure TSimpleObjectObjectHash<TKey, TValue>.Grow;
var
   i, j, n : Integer;
   oldBuckets : TObjectObjectHashBuckets;
begin
   if FCapacity=0 then
      FCapacity:=32
   else FCapacity:=FCapacity*2;
   FGrowth:=(FCapacity*3) div 4;

   oldBuckets:=FBuckets;
   FBuckets:=nil;
   SetLength(FBuckets, FCapacity);

   n:=FCapacity-1;
   for i:=0 to High(oldBuckets) do begin
      if oldBuckets[i].HashCode=0 then continue;
      j:=(oldBuckets[i].HashCode and (FCapacity-1));
      while FBuckets[j].HashCode<>0 do
         j:=(j+1) and n;
      FBuckets[j]:=oldBuckets[i];
   end;
end;

// GetItemHashCode
//
function TSimpleObjectObjectHash<TKey, TValue>.GetItemHashCode(const item1 : TObjectObjectHashBucket) : Integer;
begin
   Result:=(PNativeInt(@item1.Key)^ shr 2);
end;

// GetValue
//
function TSimpleObjectObjectHash<TKey, TValue>.GetValue(aKey : TKey) : TValue;
var
   bucket : TObjectObjectHashBucket;
begin
   bucket.Key:=aKey;
   if Match(bucket) then
      Result:=bucket.Value
   {$ifdef VER200}
   else Result:=default(TValue); // D2009 support
   {$else}
   else Result:=TValue(TObject(nil));  // workaround for D2010 compiler bug
   {$endif}
end;

// SetValue
//
procedure TSimpleObjectObjectHash<TKey, TValue>.SetValue(aKey : TKey; aValue : TValue);
var
   bucket : TObjectObjectHashBucket;
begin
   bucket.Key:=aKey;
   bucket.Value:=aValue;
   Replace(bucket);
end;

// LinearFind
//
function TSimpleObjectObjectHash<TKey, TValue>.LinearFind(const item : TObjectObjectHashBucket; var index : Integer) : Boolean;
begin
   repeat
      if FBuckets[index].HashCode=0 then
         Exit(False)
      else if item.Key=FBuckets[index].Key then
         Exit(True);
      index:=(index+1) and (FCapacity-1);
   until False;
end;

// Match
//
function TSimpleObjectObjectHash<TKey, TValue>.Match(var anItem : TObjectObjectHashBucket) : Boolean;
var
   i : Integer;
begin
   if FCount=0 then Exit(False);
   i:=(GetItemHashCode(anItem) and (FCapacity-1));
   Result:=LinearFind(anItem, i);
   if Result then
      anItem:=FBuckets[i];
end;

// Replace
//
function TSimpleObjectObjectHash<TKey, TValue>.Replace(const anItem : TObjectObjectHashBucket) : Boolean;
var
   i : Integer;
   hashCode : Integer;
begin
   if FCount>=FGrowth then Grow;

   hashCode:=GetItemHashCode(anItem);
   i:=(hashCode and (FCapacity-1));
   if LinearFind(anItem, i) then begin
      FBuckets[i]:=anItem;
      Replace := False;
   end else begin
      FBuckets[i]:=anItem;
      FBuckets[i].HashCode:=hashCode;
      Inc(FCount);
      Result:=True;
   end;
end;

// CleanValues
//
procedure TSimpleObjectObjectHash<TKey, TValue>.CleanValues;
var
   i : Integer;
begin
   for i:=0 to FCapacity-1 do begin
      if FBuckets[i].HashCode<>0 then
         FBuckets[i].Value.Free;
   end;
   Clear;
end;

// Clear
//
procedure TSimpleObjectObjectHash<TKey, TValue>.Clear;
begin
   FCount:=0;
   FCapacity:=0;
   FGrowth:=0;
   SetLength(FBuckets, 0);
end;

// ------------------
// ------------------ TThread<T> ------------------
// ------------------

// Create
//
constructor TThread<T>.Create(const anObj : T);
begin
   inherited Create;
   FLock := TdwsCriticalSection.Create;
   FValue := anObj;
end;

// Destroy
//
destructor TThread<T>.Destroy;
begin
   inherited;
   FLock.Free;
   FValue.Free;
end;

// Lock
//
function TThread<T>.Lock : T;
begin
   FLock.Enter;
   Result := FValue;
end;

// Unlock
//
procedure TThread<T>.Unlock;
begin
   FLock.Leave;
end;

// ------------------
// ------------------ TThreadCached<T> ------------------
// ------------------

// Create
//
constructor TThreadCached<T>.Create(const aNeedValue : TSimpleCallback<T>; maxAgeMSec : Integer);
begin
   FLock:=TMultiReadSingleWrite.Create;
   FOnNeedValue:=aNeedValue;
   FMaxAge:=maxAgeMSec;
end;

// Destroy
//
destructor TThreadCached<T>.Destroy;
begin
   FLock.Free;
end;

// Invalidate
//
procedure TThreadCached<T>.Invalidate;
begin
   FExpiresAt:=0;
end;

// GetValue
//
function TThreadCached<T>.GetValue : T;
var
   ts : Int64;
begin
   ts:=GetSystemMilliseconds;
   if ts>=FExpiresAt then begin
      FLock.BeginWrite;
      try
         if FOnNeedValue(FValue)=csContinue then
            FExpiresAt:=ts+FMaxAge;
         Result:=FValue;
      finally
         FLock.EndWrite;
      end;
   end else begin
      FLock.BeginRead;
      try
         Result:=FValue;
      finally
         FLock.EndRead;
      end;
   end;
end;

// SetValue
//
procedure TThreadCached<T>.SetValue(const v : T);
begin
   FLock.BeginWrite;
   try
      FExpiresAt:=GetSystemMilliseconds+FMaxAge;
      FValue:=v;
   finally
      FLock.EndWrite;
   end;
end;

// ------------------
// ------------------ TSimpleIntegerStack ------------------
// ------------------

// Create
//
constructor TSimpleIntegerStack.Create;
begin
   FChunk:=@FBaseChunk;
   FChunkIndex:=-1;
end;

// Destroy
//
destructor TSimpleIntegerStack.Destroy;
begin
   Clear;
end;

// Allocate
//
class function TSimpleIntegerStack.Allocate : TSimpleIntegerStack;
var
   p : Pointer;
   n : Integer;
begin
   n:=InstanceSize;
   GetMem(p, n);
   Move(Pointer(vTemplate)^, p^, n);
   Result:=TSimpleIntegerStack(p);
   Result.FChunk:=@Result.FBaseChunk;
end;

// Push
//
procedure TSimpleIntegerStack.Push(const item : Integer);
begin
   if FChunkIndex<TSimpleIntegerStackChunk.ChunkSize-1 then
      Inc(FChunkIndex)
   else Grow;
   FChunk.Data[FChunkIndex]:=item;
   Inc(FCount);
end;

// Pop
//
procedure TSimpleIntegerStack.Pop;
begin
   if FChunkIndex>0 then
      Dec(FChunkIndex)
   else Shrink;
   Dec(FCount);
end;

// Clear
//
procedure TSimpleIntegerStack.Clear;
var
   p : PSimpleIntegerStackChunk;
begin
   if FPooledChunk<>nil then
      FreeMem(FPooledChunk);
   FPooledChunk:=nil;
   while FChunk<>@FBaseChunk do begin
      p:=FChunk;
      FChunk:=p.Prev;
      FreeMem(p);
   end;
end;

// Grow
//
procedure TSimpleIntegerStack.Grow;
var
   p : PSimpleIntegerStackChunk;
begin
   if FPooledChunk<>nil then begin
      p:=FPooledChunk;
      FPooledChunk:=nil;
   end else GetMem(p, SizeOf(TSimpleIntegerStackChunk));
   p.Prev:=FChunk;
   FChunk:=p;
   FChunkIndex:=0;
end;

// Shrink
//
procedure TSimpleIntegerStack.Shrink;
begin
   if FChunk.Prev=nil then
      Dec(FChunkIndex)
   else begin
      FreeMem(FPooledChunk);
      FPooledChunk:=FChunk;
      FChunk:=FChunk.Prev;
      FChunkIndex:=TSimpleIntegerStackChunk.ChunkSize-1;
   end;
end;

// GetPeek
//
function TSimpleIntegerStack.GetPeek : Integer;
begin
   Result:=FChunk.Data[FChunkIndex];
end;

// SetPeek
//
procedure TSimpleIntegerStack.SetPeek(const item : Integer);
begin
   FChunk.Data[FChunkIndex]:=item;
end;

// ------------------
// ------------------ TClassCloneConstructor<T> ------------------
// ------------------

// Initialize
//
procedure TClassCloneConstructor<T>.Initialize(aTemplate : T);
begin
   FTemplate := aTemplate;
   FSize := FTemplate.InstanceSize;
end;

// Finalize
//
procedure TClassCloneConstructor<T>.Finalize;
begin
   FTemplate.Free;
   TObject(FTemplate) := nil; // D2010 bug workaround
end;

// Create
//
function TClassCloneConstructor<T>.Create : T;
begin
   {$ifdef FPC}
   System.GetMem(Pointer(Result), FSize);
   System.Move(Pointer(FTemplate)^, Pointer(Result)^, FSize);
   {$else}
   GetMemForT(Result, FSize);
   Move(TtoPointer(FTemplate)^, TtoPointer(Result)^, FSize);
   {$endif}
end;

// ------------------
// ------------------ TQuickSort ------------------
// ------------------

// Sort
//
procedure TQuickSort.Sort(minIndex, maxIndex : NativeInt);
var
   i, j, p, n : NativeInt;
begin
   n:=maxIndex-minIndex;
   case n of
      1 : begin
         if CompareMethod(minIndex, maxIndex)>0 then
            SwapMethod(minIndex, maxIndex);
      end;
      2 : begin
         i:=minIndex+1;
         if CompareMethod(minIndex,i)>0 then
            SwapMethod(minIndex, i);
         if CompareMethod(i, maxIndex)>0 then begin
            SwapMethod(i, maxIndex);
            if CompareMethod(minIndex, i)>0 then
               SwapMethod(minIndex, i);
         end;
      end;
   else
      if n<=0 then Exit;
      repeat
         i:=minIndex;
         j:=maxIndex;
         p:=((i+j) shr 1);
         repeat
            while CompareMethod(i, p)<0 do Inc(i);
            while CompareMethod(j, p)>0 do Dec(j);
            if i<=j then begin
               if i <> j then
                  SwapMethod(i, j);
               if p=i then
                  p:=j
               else if p=j then
                  p:=i;
               Inc(i);
               Dec(j);
            end;
         until i>j;
         if minIndex<j then
            Sort(minIndex, j);
         minIndex:=i;
      until i>=maxIndex;
   end;
end;

// ------------------
// ------------------ TSimpleQueue<T> ------------------
// ------------------

// Create
//
constructor TSimpleQueue<T>.Create(poolSize: Integer);
begin
   FPoolLeft:=poolSize;
end;

// Destroy
//
destructor TSimpleQueue<T>.Destroy;
var
   next : PItemT;
begin
   Clear;
   while FPool<>nil do begin
      next:=FPool.Next;
      FreeMem(FPool);
      FPool:=next;
   end;
   inherited;
end;

// Alloc
//
function TSimpleQueue<T>.Alloc: PItemT;
begin
   if FPool=nil then
      Result:=AllocMem(SizeOf(ItemT))
   else begin
      Result:=FPool;
      FPool:=Result.Next;
      Result.Next:=nil;
      Inc(FPoolLeft);
   end;
   Inc(FCount);
end;

// Release
//
procedure TSimpleQueue<T>.Release(i: PItemT);
begin
   i.Value:=Default(T);
   if FPoolLeft>0 then begin
      Dec(FPoolLeft);
      i.Prev:=nil;
      i.Next:=FPool;
      FPool:=i;
   end else FreeMem(i);
   Dec(FCount);
end;

// Push
//
procedure TSimpleQueue<T>.Push(const v: T);
var
   p : PItemT;
begin
   p:=Alloc;
   p.Value:=v;
   if FLast<>nil then begin
      p.Prev:=FLast;
      FLast.Next:=p;
   end else FFirst:=p;
   FLast:=p;
end;

// Pop
//
function TSimpleQueue<T>.Pop(var v: T) : Boolean;
var
   p : PItemT;
begin
   if FCount=0 then Exit(False);

   p:=FLast;
   FLast:=p.Prev;
   v:=p.Value;
   Release(p);
   if FLast<>nil then
      FLast.Next:=nil
   else FFirst:=FLast;
   Result:=True;
end;

// Pop
//
function TSimpleQueue<T>.Pop : T;
begin
   Assert(Count>0);
   Pop(Result{%H-});
end;

// Insert
//
procedure TSimpleQueue<T>.Insert(const v: T);
var
   p : PItemT;
begin
   p:=Alloc;
   p.Value:=v;
   if FFirst<>nil then begin
      p.Next:=FFirst;
      FFirst.Prev:=p;
   end else FLast:=p;
   FFirst:=p;
end;

// Pull
//
function TSimpleQueue<T>.Pull(var v: T) : Boolean;
var
   p : PItemT;
begin
   if FCount=0 then Exit(False);

   p:=FFirst;
   FFirst:=p.Next;
   v:=p.Value;
   Release(p);
   if FFirst<>nil then
      FFirst.Prev:=nil
   else FLast:=FFirst;
   Result:=True;
end;

// Pull
//
function TSimpleQueue<T>.Pull : T;
begin
   Assert(Count>0);
   Pull(Result{%H-});
end;

// Clear
//
procedure TSimpleQueue<T>.Clear;
var
   p, pNext : PItemT;
begin
   p:=FFirst;
   while p<>nil do begin
      pNext:=p.Next;
      Release(p);
      p:=pNext;
   end;
   FFirst:=nil;
   FLast:=nil;
end;

// ------------------
// ------------------ TSimpleStringHash ------------------
// ------------------

// SameItem
//
function TSimpleStringHash.SameItem(const item1, item2 : String) : Boolean;
begin
   Result:=UnicodeSameText(item1, item2);
end;

// GetItemHashCode
//
function TSimpleStringHash.GetItemHashCode(const item1 : String) : Cardinal;
begin
   Result := SimpleStringCaseInsensitiveHash(item1);
end;

// ------------------
// ------------------ TSimpleInt64List ------------------
// ------------------

// Sort
//
procedure TSimpleInt64List.Sort;
begin
   if Count > 1 then
      QuickSortInt64(PInt64Array(FItems), 0, Count-1)
end;

// ------------------
// ------------------ TSimpleDoubleList ------------------
// ------------------

// DoExchange
//
procedure TSimpleDoubleList.Exchange(index1, index2 : Integer);
var
   buf : Double;
begin
   buf:=FItems[index1];
   FItems[index1]:=FItems[index2];
   FItems[index2]:=buf;
end;

// Sort
//
procedure TSimpleDoubleList.Sort;
begin
   if Count > 1 then
      QuickSortDoublePrecision(PDoubleArray(FItems), 0, Count-1);
end;

// QuickSum
//
function TSimpleDoubleList.QuickSum : Double;
{$ifndef WIN32_ASM}
var
   i : Integer;
   buf : Extended;
begin
   buf := 0;
   for i := 0 to Count-1 do
      buf := buf + FItems[i];
   Result := buf;
{$else}
asm
   fldz
   mov   ecx, [eax + OFFSET FCount]
   test  ecx, ecx
   jz    @@done

   mov   edx, [eax + OFFSET FItems]

@@loop:
   fadd  qword ptr [edx]
   lea   edx, [edx+8]
   dec   ecx
   jnz   @@Loop

@@done:
   ret
{$endif}
end;

// KahanSum
//
function TSimpleDoubleList.KahanSum : Double;
var
   c, y, t : Double;
   i : Integer;
begin
   if Count = 0 then Exit(0);
   Result := FItems[0];
   c := 0;
   for i := 1 to Count-1 do begin
      y := FItems[i] - c;
      t := Result + y;
      c := (t-Result) - y;
      Result := t;
   end;
end;

// NeumaierSum
//
function TSimpleDoubleList.NeumaierSum : Double;
var
   c, t : Double;
   i : Integer;
begin
   if Count = 0 then Exit(0);
   Result := FItems[0];
   c := 0;
   for i := 1 to Count-1 do begin
      t := Result + FItems[i];
      if Abs(Result) >= Abs(FItems[i]) then
         c := c + (Result - t) + FItems[i]
      else c := c + (FItems[i] - t) + Result;
      Result := t;
   end;
   Result := Result + c;
end;

// MedianSort
//
procedure TSimpleDoubleList.MedianSort(minIndex, maxIndex : Integer);
// Median sorts aims only to sort accurately the two mid-point values
// which are (potentially) used to compute the median
var
   i, j, p, n : Integer;
begin
   n := maxIndex - minIndex;
   case n of
      1 : begin
         if FItems[minIndex] > FItems[maxIndex] then
            Exchange(minIndex, maxIndex);
      end;
      2 : begin
         i := minIndex+1;
         if FItems[minIndex] > FItems[i] then
            Exchange(minIndex, i);
         if FItems[i] > FItems[maxIndex] then begin
            Exchange(i, maxIndex);
            if FItems[minIndex] > FItems[i] then
               Exchange(minIndex, i);
         end;
      end;
   else
      repeat
         i := minIndex;
         j := maxIndex;
         p := ((i+j) shr 1);
         repeat
            while FItems[i] < FItems[p] do Inc(i);
            while FItems[j] > FItems[p] do Dec(j);
            if i <= j then begin
               Exchange(i, j);
               if p = i then
                  p := j
               else if p = j then
                  p := i;
               Inc(i);
               Dec(j);
            end;
         until i > j;
         if minIndex < j then
            if (j >= Count div 2 - 1) and (minIndex <= Count div 2) then
               MedianSort(minIndex, j);
         minIndex := i;
      until i >= maxIndex;
   end;
end;

// QuickMedian
//
function TSimpleDoubleList.QuickMedian : Double;
var
   n : Integer;
begin
   case Count of
      0 : Result := 0;
      1 : Result := FItems[0];
      2 : Result := (FItems[0] + FItems[1])*0.5;
   else
      MedianSort(0, Count-1);
      if (Count and 1)=0 then begin
         n := Count shr 1;
         Result := (FItems[n] + FItems[n-1])*0.5;
      end else begin
         Result := FItems[Count shr 1];
      end;
   end;
end;

// ------------------
// ------------------ TPooledObject ------------------
// ------------------

// Create
//
constructor TPooledObject.Create;
begin
   // nothing, just to introduce virtual construction
end;

// ------------------
// ------------------ TPool ------------------
// ------------------

// Initialize
//
procedure TPool.Initialize(aClass : TPooledObjectClass);
begin
   FRoot:=nil;
   FPoolClass:=aClass;
   FLock:=TMultiReadSingleWrite.Create;
   FCount:=0;
   FCapacity:=16*1024;
end;

// Finalize
//
procedure TPool.Finalize;
begin
   Clean(0);
   FLock.Free;
   FLock:=nil;
   FPoolClass:=nil;
   FCount:=0;
   FCapacity:=0;
end;

// Clean
//
procedure TPool.Clean(nb : Integer);
var
   obj : TPooledObject;
begin
   if FCount=0 then Exit;

   FLock.BeginWrite;
   try
      if nb=0 then nb:=FCount;
      while (FRoot<>nil) and (nb>0) do begin
         obj:=FRoot;
         FRoot:=obj.FNext;
         obj.Destroy;
         Dec(nb);
         Dec(FCount);
      end;
   finally
      FLock.EndWrite;
   end;
end;

// Acquire
//
function TPool.Acquire : TPooledObject;
begin
   Result:=nil;
   if FRoot<>nil then begin
      FLock.BeginWrite;
      try
         if FRoot<>nil then begin
            Result:=FRoot;
            FRoot:=Result.FNext;
            Dec(FCount);
         end;
      finally
         FLock.EndWrite;
      end;
   end;
   if Result=nil then
      Result:=FPoolClass.Create
   else Result.FNext:=nil;
end;

// Release
//
procedure TPool.Release(obj : TPooledObject);
begin
   FLock.BeginWrite;
   try
      if FCount<FCapacity then begin
         obj.FNext:=FRoot;
         FRoot:=obj;
         obj:=nil;
         Inc(FCount);
      end;
   finally
      FLock.EndWrite;
   end;
   if obj<>nil then
      obj.Destroy;
end;

// ------------------
// ------------------ TStringDynArrayHelper ------------------
// ------------------

function TStringDynArrayHelper.Join(const separator : String) : String;
var
   i : Integer;
   wobs : TWriteOnlyBlockStream;
begin
   case Length(Self) of
      0 : Result := '';
      1 : Result := Self[0];
      2 : Result := Self[0] + separator + Self[1];
   else
      wobs := TWriteOnlyBlockStream.AllocFromPool;
      try
         wobs.WriteString(Self[0]);
         for i := 1 to High(Self) do begin
            wobs.WriteString(separator);
            wobs.WriteString(Self[i]);
         end;
         Result := wobs.ToString;
      finally
         wobs.ReturnToPool;
      end;
   end;
end;

// ------------------
// ------------------ TSimpleStringList ------------------
// ------------------

// Add
//
procedure TSimpleStringList.Add(const s : String);
begin
   if FCount = FCapacity then begin
      Inc(FCapacity, 8);
      SetLength(FItems, FCapacity);
   end;
   FItems[FCount] := s;
   Inc(FCount);
end;

// IndexOf
//
function TSimpleStringList.IndexOf(const s : String) : Integer;
begin
   for Result := 0 to FCount-1 do
      if StrEquals(FItems[Result], s) then Exit;
   Result := -1;
end;

// Clear
//
procedure TSimpleStringList.Clear;
begin
   FCount := 0;
end;

// GetItems
//
function TSimpleStringList.GetItems(i : Integer) : String;
begin
   Result := FItems[i];
end;

// SetItems
//
procedure TSimpleStringList.SetItems(i : Integer; const s : String);
begin
   FItems[i] := s;
end;

// ------------------
// ------------------ TSimpleStringListPool ------------------
// ------------------

// Initialize
//
procedure TSimpleStringListPool.Initialize;
begin
   FCount := 0;
end;

// Finalize
//
procedure TSimpleStringListPool.Finalize;
begin
   Flush;
end;

// Acquire
//
function TSimpleStringListPool.Acquire : TSimpleStringList;
begin
   if FCount > 0 then begin
      Result := FPool[FCount-1];
      Dec(FCount);
   end else begin
      Result := TSimpleStringList.Create;
   end;
end;

// Release
//
procedure TSimpleStringListPool.Release(sl : TSimpleStringList);
begin
   if FCount < Length(FPool) then begin
      sl.Clear;
      FPool[FCount] := sl;
      Inc(FCount);
   end else begin
      sl.Free;
   end;
end;

// Flush
//
procedure TSimpleStringListPool.Flush;
begin
   while FCount > 0 do begin
      Dec(FCount);
      FPool[FCount].Free;
   end;
end;

// ScanMemory
//
function ScanMemory(p : Pointer; size : IntPtr; const pattern : TBytes) : Pointer;
var
   n : IntPtr;
   tail : UIntPtr;
   iter : PByte;
begin
   n := Length(pattern);
   if n <= 0 then Exit(nil);
   tail := UIntPtr(@PByte(p)[size - n]);
   iter := p;
   while UIntPtr(iter) <= tail do begin
      if iter^ = pattern[0] then begin
         if CompareMem(iter, pattern, n) then
            Exit(iter);
      end;
      Inc(iter);
   end;
   Result := nil;
end;

{$ifdef DELPHI_XE2_PLUS}
   {$if Defined(WIN32_ASM) or Defined(WIN64_ASM)}
      {$define TEST_POPCNT}
   {$ifend}
{$endif}

// PopCount32
//
function PopCount32_Pascal(v : Int32) : Integer;
begin
   v      := (v and $55555555) + ((v shr  1) and $55555555);
   v      := (v and $33333333) + ((v shr  2) and $33333333);
   v      := (v and $0f0f0f0f) + ((v shr  4) and $0f0f0f0f);
   v      := (v and $00ff00ff) + ((v shr  8) and $00ff00ff);
   Result := (v and $0000ffff) + ((v shr 16) and $0000ffff);
end;
{$ifdef WIN32_ASM}
function PopCount32_asm(v : Int32) : Integer;
asm
   POPCNT    eax, v
end;
{$endif}
var vPopCount32 : function(v : Int32) : Integer;
function PopCount32(v : Int32) : Integer;
begin
   Result := vPopCount32(v);
end;

// PopCount64a
//
{$ifdef WIN64_ASM}
function PopCount64_asm(p : PInt64; nbInt64s : Integer) : Integer;
asm
   xor   rax, rax
   test  rdx, rdx
   jz    @@done
@@loop:
   popcnt r8, qword ptr [rcx]
   add   rax, r8
   add   rcx, 8
   dec   rdx
   jnz   @@loop
@@done:
end;
{$endif}
function PopCount64_Pascal(p : PInt64; nbInt64s : Integer) : Integer;
begin
   Result := 0;
   while nbInt64s > 0 do begin
      Inc(Result, vPopCount32(PIntegerArray(p)[0]) + vPopCount32(PIntegerArray(p)[1]));
      Dec(nbInt64s);
      Inc(p);
   end;
end;
var vPopCount64 : function(p : PInt64; nbInt64s : Integer) : Integer;
function PopCount64(p : PInt64; nbInt64s : Integer) : Integer;
begin
   Result := vPopCount64(p, nbInt64s);
end;

// PopCount
//
function PopCount(p : PByte; n : Integer) : Integer;
var
   n64 : Integer;
begin
   n64 := n shr 3;
   if n64 > 0 then begin
      Result := vPopCount64(PInt64(p), n64);
      Inc(p, n64*SizeOf(Int64));
      n := n and 7;
   end else Result := 0;
   if n >= SizeOf(Int32) then begin
      Inc(Result, PopCount32(PInteger(p)^));
      Inc(p, SizeOf(Int32));
      Dec(n, SizeOf(Int32));
   end;
   case n of
      1 : Inc(Result, PopCount32(p^));
      2 : Inc(Result, PopCount32(PWord(p)^));
      3 : Inc(Result, PopCount32((PWord(p)^ shl 16) or p[2]));
   end;
end;

// SwapInt64
//
procedure SwapInt64(var a, b : Int64);
var
   buf : Int64;
begin
   buf := a;
   a := b;
   b := buf;
end;

// SwapSingles
//
procedure SwapSingles(var a, b : Single); inline;
var
   buf : Single;
begin
   buf := a;
   a := b;
   b := buf;
end;

// SwapDoubles
//
procedure SwapDoubles(var a, b : Double);
var
   buf : Double;
begin
   buf := a;
   a := b;
   b := buf;
end;

// SwapPointers
//
procedure SwapPointers(var a, b : Pointer); inline;
var
   buf : Pointer;
begin
   buf := a;
   a := b;
   b := buf;
end;

// ExchangeBytes
//
procedure ExchangeBytes(const a, b; size : Integer);
begin
   var p1 : PByte := @a;
   var p2 : PByte := @b;
   while size >= 8 do begin
      var buf := PUInt64(p1)^;
      PUInt64(p1)^ := PUInt64(p2)^;
      PUInt64(p2)^ := buf;
      Dec(size, 8);
      Inc(p1, 8);
      Inc(p2, 8);
   end;

   if size >= 4 then begin
      var buf := PUInt32(p1)^;
      PUInt32(p1)^ := PUInt32(p2)^;
      PUInt32(p2)^ := buf;
      Dec(size, 4);
      Inc(p1, 4);
      Inc(p2, 4);
   end;

   if size >= 2 then begin
      var buf := PWord(p1)^;
      PWord(p1)^ := PWord(p2)^;
      PWord(p2)^ := buf;
      Dec(size, 2);
      Inc(p1, 2);
      Inc(p2, 2);
   end;

   if size > 0 then begin
      var buf := p1^;
      p1^ := p2^;
      p2^ := buf;
   end;
end;

// ------------------
// ------------------ TClassInstanceTemplate<T> ------------------
// ------------------

// Initialize
//
procedure TClassInstanceTemplate<T>.Initialize;
begin
   FTemplate := T.NewInstance;
   FPool := nil;
end;

// Finalize
//
procedure TClassInstanceTemplate<T>.Finalize;
begin
   if FTemplate <> nil then begin
      TObject(FTemplate).FreeInstance;
      FTemplate := nil;
   end;
   if FPool <> nil then begin
      TObject(FPool).FreeInstance;
      FPool := nil;
   end;
end;

// Initialized
//
function TClassInstanceTemplate<T>.Initialized : Boolean;
begin
   Result := FTemplate <> nil;
end;

// CreateInstance
//
function TClassInstanceTemplate<T>.CreateInstance : T;
begin
   Result := FPool;
   if Result <> nil then begin
      if InterlockedCompareExchangePointer(FPool, nil, Pointer(Result)) <> Pointer(Result) then
         Result := nil;
   end;
   if Result = nil then
      Result := GetMemory(T.InstanceSize);
   System.Move(Pointer(FTemplate)^, Pointer(Result)^, T.InstanceSize);
end;

// ReleaseInstance
//
procedure TClassInstanceTemplate<T>.ReleaseInstance(instance : T);
begin
   if FPool <> nil then
      FreeMemory(Pointer(instance))
   else if InterlockedCompareExchangePointer(FPool, Pointer(instance), nil) <> nil then
      FreeMemory(Pointer(instance));
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   InitializeSmallIntegers;
   InitializeStringsUnifier;
   TSimpleIntegerStack.vTemplate:=TSimpleIntegerStack.Create;
   TNameObjectHash.vHashSalt := Cardinal(GetSystemMilliseconds);

   vPopCount32 := PopCount32_Pascal;
   vPopCount64 := PopCount64_Pascal;
   {$ifdef TEST_POPCNT}
   if (System.TestSSE or sePOPCNT) <> 0 then begin
      {$ifdef WIN32_ASM}
      vPopCount32 := PopCount32_asm;
      {$endif}
      {$ifdef WIN64_ASM}
      vPopCount64 := PopCount64_asm;
      {$endif}
   end;
   {$endif}

finalization

   FinalizeStringsUnifier;

   FreeAndNil(TSimpleIntegerStack.vTemplate);
   FreeAndNil(TObject(vWOBSPool));

end.

