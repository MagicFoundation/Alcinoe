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
{ The Original Code is HashMap.pas.                                                                }
{                                                                                                  }
{ The Initial Developer of the Original Code is Jean-Philippe BEMPEL aka RDM. Portions created by  }
{ Jean-Philippe BEMPEL are Copyright (C) Jean-Philippe BEMPEL (rdm_30 att yahoo dott com)          }
{ All rights reserved.                                                                             }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ The Delphi Container Library                                                                     }
{                                                                                                  }
{**************************************************************************************************}

// Last modified: $Date$
// For history see end of file

unit JclHashMap;

{$I jcl.inc}

interface

uses
  JclBase, JclAbstractContainer, JclDCL_intf, JclDCLUtil;

type
  TJclIntfIntfEntry = record
    Key: IInterface;
    Value: IInterface;
  end;

  TJclStrIntfEntry = record
    Key: string;
    Value: IInterface;
  end;

  TJclStrStrEntry = record
    Key: string;
    Value: string;
  end;

  TJclStrEntry = record
    Key: string;
    Value: TObject
  end;

  TJclEntry = record
    Key: TObject;
    Value: TObject;
  end;

  TJclIntfIntfEntryArray = array of TJclIntfIntfEntry;
  TJclStrIntfEntryArray = array of TJclStrIntfEntry;
  TJclStrStrEntryArray = array of TJclStrStrEntry;
  TJclStrEntryArray = array of TJclStrEntry;
  TJclEntryArray = array of TJclEntry;

  PJclIntfIntfBucket = ^TJclIntfIntfBucket;
  TJclIntfIntfBucket = record
    Count: Integer;
    Entries: TJclIntfIntfEntryArray;
  end;

  PJclStrIntfBucket = ^TJclStrIntfBucket;
  TJclStrIntfBucket = record
    Count: Integer;
    Entries: TJclStrIntfEntryArray;
  end;

  PJclStrStrBucket = ^TJclStrStrBucket;
  TJclStrStrBucket = record
    Count: Integer;
    Entries: TJclStrStrEntryArray;
  end;

  PJclStrBucket = ^TJclStrBucket;
  TJclStrBucket = record
    Count: Integer;
    Entries: TJclStrEntryArray;
  end;

  PJclBucket = ^TJclBucket;
  TJclBucket = record
    Count: Integer;
    Entries: TJclEntryArray;
  end;

  TJclIntfIntfBucketArray = array of TJclIntfIntfBucket;
  TJclStrIntfBucketArray = array of TJclStrIntfBucket;
  TJclStrStrBucketArray = array of TJclStrStrBucket;
  TJclStrBucketArray = array of TJclStrBucket;
  TJclBucketArray = array of TJclBucket;

  // Hash Function
  TJclHashFunction = function(Key: Cardinal): Cardinal of object;

  TJclIntfIntfHashMap = class(TJclAbstractContainer, IIntfIntfMap, IIntfCloneable)
  private
    FCapacity: Integer;
    FCount: Integer;
    FBuckets: TJclIntfIntfBucketArray;
    FHashFunction: TJclHashFunction;
    function HashMul(Key: Cardinal): Cardinal;
  protected
    procedure GrowEntries(BucketIndex: Integer); virtual;
    { IIntfIntfMap }
    procedure Clear;
    function ContainsKey(Key: IInterface): Boolean;
    function ContainsValue(Value: IInterface): Boolean;
    function Equals(AMap: IIntfIntfMap): Boolean;
    function GetValue(Key: IInterface): IInterface;
    function IsEmpty: Boolean;
    function KeySet: IIntfSet;
    procedure PutAll(AMap: IIntfIntfMap);
    procedure PutValue(Key, Value: IInterface);
    function Remove(Key: IInterface): IInterface;
    function Size: Integer;
    function Values: IIntfCollection;
    { IIntfCloneable }
    function Clone: IInterface;
  public
    constructor Create(Capacity: Integer = DCLDefaultCapacity);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write
      FHashFunction;
  end;

  TJclStrIntfHashMap = class(TJclAbstractContainer, IStrIntfMap, IIntfCloneable)
  private
    FCapacity: Integer;
    FCount: Integer;
    FBuckets: TJclStrIntfBucketArray;
    FHashFunction: TJclHashFunction;
    function HashMul(Key: Cardinal): Cardinal;
    function HashString(const Key: string): Cardinal;
  protected
    procedure GrowEntries(BucketIndex: Integer); virtual;
    { IIntfMap }
    procedure Clear;
    function ContainsKey(const Key: string): Boolean;
    function ContainsValue(Value: IInterface): Boolean;
    function Equals(AMap: IStrIntfMap): Boolean;
    function GetValue(const Key: string): IInterface;
    function IsEmpty: Boolean;
    function KeySet: IStrSet;
    procedure PutAll(AMap: IStrIntfMap);
    procedure PutValue(const Key: string; Value: IInterface);
    function Remove(const Key: string): IInterface;
    function Size: Integer;
    function Values: IIntfCollection;
    { IIntfCloneable }
    function Clone: IInterface;
  public
    constructor Create(Capacity: Integer = DCLDefaultCapacity);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write
      FHashFunction;
  end;

  TJclStrStrHashMap = class(TJclAbstractContainer, IStrStrMap, IIntfCloneable)
  private
    FCapacity: Integer;
    FCount: Integer;
    FBuckets: TJclStrStrBucketArray;
    FHashFunction: TJclHashFunction;
    function HashMul(Key: Cardinal): Cardinal;
    function HashString(const Key: string): Cardinal;
  protected
    procedure GrowEntries(BucketIndex: Integer); virtual;
    { IStrStrMap }
    procedure Clear;
    function ContainsKey(const Key: string): Boolean;
    function ContainsValue(const Value: string): Boolean;
    function Equals(AMap: IStrStrMap): Boolean;
    function GetValue(const Key: string): string;
    function IsEmpty: Boolean;
    function KeySet: IStrSet;
    procedure PutAll(AMap: IStrStrMap);
    procedure PutValue(const Key, Value: string);
    function Remove(const Key: string): string;
    function Size: Integer;
    function Values: IStrCollection;
    // Daniele Teti
    function KeyOfValue(const Value: string): string;
    { IIntfCloneable }
    function Clone: IInterface;
  public
    constructor Create(Capacity: Integer = DCLDefaultCapacity);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write
      FHashFunction;
  end;

  TJclStrHashMap = class(TJclAbstractContainer, IStrMap, ICloneable)
  private
    FCapacity: Integer;
    FCount: Integer;
    FBuckets: TJclStrBucketArray;
    FHashFunction: TJclHashFunction;
    FOwnsObjects: Boolean;
    function HashMul(Key: Cardinal): Cardinal;
    function HashString(const Key: string): Cardinal;
  protected
    procedure GrowEntries(BucketIndex: Integer); virtual;
    procedure FreeObject(var AObject: TObject);
    { IStrMap }
    procedure Clear;
    function ContainsKey(const Key: string): Boolean;
    function ContainsValue(Value: TObject): Boolean;
    function Equals(AMap: IStrMap): Boolean;
    function GetValue(const Key: string): TObject;
    function IsEmpty: Boolean;
    function KeySet: IStrSet;
    procedure PutAll(AMap: IStrMap);
    procedure PutValue(const Key: string; Value: TObject);
    function Remove(const Key: string): TObject;
    function Size: Integer;
    function Values: ICollection;
    { ICloneable }
    function Clone: TObject;
  public
    constructor Create(Capacity: Integer = DCLDefaultCapacity; AOwnsObjects:
      Boolean = True);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write
      FHashFunction;
    property OwnsObjects: Boolean read FOwnsObjects;
  end;

  TJclHashMap = class(TJclAbstractContainer, IMap, ICloneable)
  private
    FCapacity: Integer;
    FCount: Integer;
    FBuckets: TJclBucketArray;
    FHashFunction: TJclHashFunction;
    FOwnsObjects: Boolean;
    function HashMul(Key: Cardinal): Cardinal;
  protected
    procedure GrowEntries(BucketIndex: Integer); virtual;
    procedure FreeObject(var AObject: TObject);
    { ICloneable }
    function Clone: TObject;
  public
    { IMap }
    procedure Clear;
    function ContainsKey(Key: TObject): Boolean;
    function ContainsValue(Value: TObject): Boolean;
    function Equals(AMap: IMap): Boolean;
    function GetValue(Key: TObject): TObject;
    function IsEmpty: Boolean;
    function KeySet: ISet;
    procedure PutAll(AMap: IMap);
    procedure PutValue(Key, Value: TObject);
    function Remove(Key: TObject): TObject;
    function Size: Integer;
    function Values: ICollection;

    constructor Create(Capacity: Integer = DCLDefaultCapacity; AOwnsObjects:
      Boolean = True);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write FHashFunction;
    property OwnsObjects: Boolean read FOwnsObjects;
  end;

implementation

uses
  SysUtils,
  JclArrayList, JclArraySet;

//=== { TJclIntfIntfHashMap } ================================================

constructor TJclIntfIntfHashMap.Create(Capacity: Integer = DCLDefaultCapacity);
var
  I: Integer;
begin
  inherited Create;
  FCapacity := Capacity;
  SetLength(FBuckets, FCapacity);
  for I := 0 to FCapacity - 1 do
    SetLength(FBuckets[I].Entries, 1);
  FHashFunction := HashMul;
end;

destructor TJclIntfIntfHashMap.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TJclIntfIntfHashMap.Clear;
var
  I, J: Integer;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  for I := 0 to FCapacity - 1 do
  begin
    for J := 0 to FBuckets[I].Count - 1 do
    begin
      FBuckets[I].Entries[J].Key := nil;
      FBuckets[I].Entries[J].Value := nil;
    end;
    FBuckets[I].Count := 0;
  end;
  FCount := 0;
end;

function TJclIntfIntfHashMap.Clone: IInterface;
var
  I, J: Integer;
  NewEntryArray: TJclIntfIntfEntryArray;
  NewMap: TJclIntfIntfHashMap;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  NewMap := TJclIntfIntfHashMap.Create(FCapacity);
  for I := 0 to FCapacity - 1 do
  begin
    NewEntryArray := NewMap.FBuckets[I].Entries;
    SetLength(NewEntryArray, Length(FBuckets[I].Entries));
    for J := 0 to FBuckets[I].Count - 1 do
    begin
      NewEntryArray[J].Key := FBuckets[I].Entries[J].Key;
      NewEntryArray[J].Value := FBuckets[I].Entries[J].Value;
    end;
    NewMap.FBuckets[I].Count := FBuckets[I].Count;
  end;
  Result := NewMap;
end;

function TJclIntfIntfHashMap.ContainsKey(Key: IInterface): Boolean;
var
  I: Integer;
  Bucket: PJclIntfIntfBucket;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := False;
  if Key = nil then
    Exit;
  Bucket := @FBuckets[FHashFunction(Integer(Key))];
  for I := 0 to Bucket.Count - 1 do
    if Bucket.Entries[I].Key = Key then
    begin
      Result := True;
      Break;
    end;
end;

function TJclIntfIntfHashMap.ContainsValue(Value: IInterface): Boolean;
var
  I, J: Integer;
  Bucket: PJclIntfIntfBucket;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := False;
  if Value = nil then
    Exit;
  for J := 0 to FCapacity - 1 do
  begin
    Bucket := @FBuckets[J];
    for I := 0 to Bucket.Count - 1 do
      if Bucket.Entries[I].Value = Value then
      begin
        Result := True;
        Exit;
      end;
  end;
end;

function TJclIntfIntfHashMap.Equals(AMap: IIntfIntfMap): Boolean;
var
  I, J: Integer;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := False;
  if AMap = nil then
    Exit;
  if FCount <> AMap.Size then
    Exit;
  Result := True;
  for I := 0 to FCapacity - 1 do
    for J := 0 to FBuckets[I].Count - 1 do
      if AMap.ContainsKey(FBuckets[I].Entries[J].Key) then
      begin
        if AMap.GetValue(FBuckets[I].Entries[J].Key) <>
          FBuckets[I].Entries[J].Value then
        begin
          Result := False;
          Exit;
        end;
      end
      else
      begin
        Result := False;
        Exit;
      end;
end;

function TJclIntfIntfHashMap.GetValue(Key: IInterface): IInterface;
var
  I: Integer;
  Bucket: PJclIntfIntfBucket;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := nil;
  if Key = nil then
    Exit;
  Bucket := @FBuckets[FHashFunction(Integer(Key))];
  for I := 0 to Bucket.Count - 1 do
    if Bucket.Entries[I].Key = Key then
    begin
      Result := Bucket.Entries[I].Value;
      Break;
    end;
end;

procedure TJclIntfIntfHashMap.GrowEntries(BucketIndex: Integer);
var
  Capacity: Integer;
begin
  Capacity := Length(FBuckets[BucketIndex].Entries);
  if Capacity > 64 then
    Capacity := Capacity + Capacity div 4
  else
    Capacity := Capacity * 4;
  SetLength(FBuckets[BucketIndex].Entries, Capacity);
end;

function TJclIntfIntfHashMap.HashMul(Key: Cardinal): Cardinal;
const
  A = 0.6180339887; // (sqrt(5) - 1) / 2
begin
  Result := Trunc(FCapacity * (Frac(Key * A)));
end;

function TJclIntfIntfHashMap.IsEmpty: Boolean;
begin
  Result := FCount = 0;
end;

function TJclIntfIntfHashMap.KeySet: IIntfSet;
var
  I, J: Integer;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := TJclIntfArraySet.Create(FCapacity);
  for I := 0 to FCapacity - 1 do
    for J := 0 to FBuckets[I].Count - 1 do
      Result.Add(FBuckets[I].Entries[J].Key);
end;

procedure TJclIntfIntfHashMap.PutAll(AMap: IIntfIntfMap);
var
  It: IIntfIterator;
  Key: IInterface;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  if AMap = nil then
    Exit;
  It := AMap.KeySet.First;
  while It.HasNext do
  begin
    Key := It.Next;
    PutValue(Key, AMap.GetValue(Key));
  end;
end;

procedure TJclIntfIntfHashMap.PutValue(Key, Value: IInterface);
var
  Index: Integer;
  Bucket: PJclIntfIntfBucket;
  I: Integer;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  if Key = nil then
    Exit;
  if Value = nil then
    Exit;
  Index := FHashFunction(Integer(Key));
  Bucket := @FBuckets[Index];
  for I := 0 to Bucket.Count - 1 do
    if Bucket.Entries[I].Key = Key then
    begin
      Bucket.Entries[I].Value := Value;
      Exit;
    end;
  if Bucket.Count = Length(Bucket.Entries) then
    GrowEntries(Index);
  Bucket.Entries[Bucket.Count].Key := Key;
  Bucket.Entries[Bucket.Count].Value := Value;
  Inc(Bucket.Count);
  Inc(FCount);
end;

function TJclIntfIntfHashMap.Remove(Key: IInterface): IInterface;
var
  Bucket: PJclIntfIntfBucket;
  I: Integer;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := nil;
  if Key = nil then
    Exit;
  Bucket := @FBuckets[FHashFunction(Integer(Key))];
  for I := 0 to Bucket.Count - 1 do
    if Bucket.Entries[I].Key = Key then
    begin
      Result := Bucket.Entries[I].Value;
      if I < Length(Bucket.Entries) - 1 then
        System.Move(Bucket.Entries[I + 1], Bucket.Entries[I],
          (Bucket.Count - I) * SizeOf(TJclStrStrEntry));
      Dec(Bucket.Count);
      Break;
    end;
end;

function TJclIntfIntfHashMap.Size: Integer;
begin
  Result := FCount;
end;

function TJclIntfIntfHashMap.Values: IIntfCollection;
var
  I, J: Integer;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := TJclIntfArrayList.Create(FCapacity);
  for I := 0 to FCapacity - 1 do
    for J := 0 to FBuckets[I].Count - 1 do
      Result.Add(FBuckets[I].Entries[J].Value);
end;

//=== { TJclStrIntfHashMap } =================================================

constructor TJclStrIntfHashMap.Create(Capacity: Integer = DCLDefaultCapacity);
var
  I: Integer;
begin
  inherited Create;
  FCapacity := Capacity;
  SetLength(FBuckets, FCapacity);
  for I := 0 to FCapacity - 1 do
    SetLength(FBuckets[I].Entries, 1);
  FHashFunction := HashMul;
end;

destructor TJclStrIntfHashMap.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TJclStrIntfHashMap.Clear;
var
  I, J: Integer;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  for I := 0 to FCapacity - 1 do
  begin
    for J := 0 to FBuckets[I].Count - 1 do
    begin
      FBuckets[I].Entries[J].Key := '';
      FBuckets[I].Entries[J].Value := nil;
    end;
    FBuckets[I].Count := 0;
  end;
  FCount := 0;
end;

function TJclStrIntfHashMap.Clone: IInterface;
var
  I, J: Integer;
  NewEntryArray: TJclStrIntfEntryArray;
  NewMap: TJclStrIntfHashMap;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  NewMap := TJclStrIntfHashMap.Create(FCapacity);
  for I := 0 to FCapacity - 1 do
  begin
    NewEntryArray := NewMap.FBuckets[I].Entries;
    SetLength(NewEntryArray, Length(FBuckets[I].Entries));
    for J := 0 to FBuckets[I].Count - 1 do
    begin
      NewEntryArray[J].Key := FBuckets[I].Entries[J].Key;
      NewEntryArray[J].Value := FBuckets[I].Entries[J].Value;
    end;
    NewMap.FBuckets[I].Count := FBuckets[I].Count;
  end;
  Result := NewMap;
end;

function TJclStrIntfHashMap.ContainsKey(const Key: string): Boolean;
var
  I: Integer;
  Bucket: PJclStrIntfBucket;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := False;
  if Key = '' then
    Exit;
  Bucket := @FBuckets[FHashFunction(HashString(Key))];
  for I := 0 to Bucket.Count - 1 do
    if Bucket.Entries[I].Key = Key then
    begin
      Result := True;
      Break;
    end;
end;

function TJclStrIntfHashMap.ContainsValue(Value: IInterface): Boolean;
var
  I, J: Integer;
  Bucket: PJclStrIntfBucket;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := False;
  if Value = nil then
    Exit;
  for J := 0 to FCapacity - 1 do
  begin
    Bucket := @FBuckets[J];
    for I := 0 to Bucket.Count - 1 do
      if Bucket.Entries[I].Value = Value then
      begin
        Result := True;
        Exit;
      end;
  end;
end;

function TJclStrIntfHashMap.Equals(AMap: IStrIntfMap): Boolean;
var
  I, J: Integer;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := False;
  if AMap = nil then
    Exit;
  if FCount <> AMap.Size then
    Exit;
  Result := True;
  for I := 0 to FCapacity - 1 do
    for J := 0 to FBuckets[I].Count - 1 do
      if AMap.ContainsKey(FBuckets[I].Entries[J].Key) then
      begin
        if AMap.GetValue(FBuckets[I].Entries[J].Key) <>
          FBuckets[I].Entries[J].Value then
        begin
          Result := False;
          Exit;
        end;
      end
      else
      begin
        Result := False;
        Exit;
      end;
end;

function TJclStrIntfHashMap.GetValue(const Key: string): IInterface;
var
  I: Integer;
  Index: Integer;
  Bucket: PJclStrIntfBucket;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := nil;
  if Key = '' then
    Exit;
  Index := FHashFunction(HashString(Key));
  Bucket := @FBuckets[Index];
  for I := 0 to Bucket.Count - 1 do
    if Bucket.Entries[I].Key = Key then
    begin
      Result := Bucket.Entries[I].Value;
      Break;
    end;
end;

procedure TJclStrIntfHashMap.GrowEntries(BucketIndex: Integer);
var
  Capacity: Integer;
begin
  Capacity := Length(FBuckets[BucketIndex].Entries);
  if Capacity > 64 then
    Capacity := Capacity + Capacity div 4
  else
    Capacity := Capacity * 4;
  SetLength(FBuckets[BucketIndex].Entries, Capacity);
end;

function TJclStrIntfHashMap.HashMul(Key: Cardinal): Cardinal;
const
  A = 0.6180339887; // (sqrt(5) - 1) / 2
begin
  Result := Trunc(FCapacity * (Frac(Key * A)));
end;

function TJclStrIntfHashMap.HashString(const Key: string): Cardinal;
var
  I: Integer;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := 0;
  for I := 1 to Length(Key) do
    Result := Result + Cardinal(Ord(Key[I]) * (I - 1) * 256);
end;

function TJclStrIntfHashMap.IsEmpty: Boolean;
begin
  Result := FCount = 0;
end;

function TJclStrIntfHashMap.KeySet: IStrSet;
var
  I, J: Integer;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := TJclStrArraySet.Create(FCapacity);
  for I := 0 to FCapacity - 1 do
    for J := 0 to FBuckets[I].Count - 1 do
      Result.Add(FBuckets[I].Entries[J].Key);
end;

procedure TJclStrIntfHashMap.PutAll(AMap: IStrIntfMap);
var
  It: IStrIterator;
  Key: string;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  if AMap = nil then
    Exit;
  It := AMap.KeySet.First;
  while It.HasNext do
  begin
    Key := It.Next;
    PutValue(Key, AMap.GetValue(Key));
  end;
end;

procedure TJclStrIntfHashMap.PutValue(const Key: string; Value: IInterface);
var
  Index: Integer;
  Bucket: PJclStrIntfBucket;
  I: Integer;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  if Key = '' then
    Exit;
  if Value = nil then
    Exit;
  Index := FHashFunction(HashString(Key));
  Bucket := @FBuckets[Index];
  for I := 0 to Bucket.Count - 1 do
    if Bucket.Entries[I].Key = Key then
    begin
      Bucket.Entries[I].Value := Value;
      Exit;
    end;
  if Bucket.Count = Length(Bucket.Entries) then
    GrowEntries(Index);
  Bucket.Entries[Bucket.Count].Key := Key;
  Bucket.Entries[Bucket.Count].Value := Value;
  Inc(Bucket.Count);
  Inc(FCount);
end;

function TJclStrIntfHashMap.Remove(const Key: string): IInterface;
var
  Bucket: PJclStrIntfBucket;
  I: Integer;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := nil;
  if Key = '' then
    Exit;
  Bucket := @FBuckets[FHashFunction(HashString(Key))];
  for I := 0 to Bucket.Count - 1 do
    if Bucket.Entries[I].Key = Key then
    begin
      Result := Bucket.Entries[I].Value;
      if I < Length(Bucket.Entries) - 1 then
        System.Move(Bucket.Entries[I + 1], Bucket.Entries[I],
          (Bucket.Count - I) * SizeOf(TJclStrStrEntry));
      Dec(Bucket.Count);
      Break;
    end;
end;

function TJclStrIntfHashMap.Size: Integer;
begin
  Result := FCount;
end;

function TJclStrIntfHashMap.Values: IIntfCollection;
var
  I, J: Integer;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := TJclIntfArrayList.Create;
  for I := 0 to FCapacity - 1 do
    for J := 0 to FBuckets[I].Count - 1 do
      Result.Add(FBuckets[I].Entries[J].Value);
end;

//=== { TJclStrStrHashMap } ==================================================

constructor TJclStrStrHashMap.Create(Capacity: Integer = DCLDefaultCapacity);
var
  I: Integer;
begin
  inherited Create;
  FCapacity := Capacity;
  SetLength(FBuckets, FCapacity);
  for I := 0 to FCapacity - 1 do
    SetLength(FBuckets[I].Entries, 1);
  FHashFunction := HashMul;
end;

destructor TJclStrStrHashMap.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TJclStrStrHashMap.Clear;
var
  I, J: Integer;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  for I := 0 to FCapacity - 1 do
  begin
    for J := 0 to FBuckets[I].Count - 1 do
    begin
      FBuckets[I].Entries[J].Key := '';
      FBuckets[I].Entries[J].Value := '';
    end;
    FBuckets[I].Count := 0;
  end;
  FCount := 0;
end;

function TJclStrStrHashMap.Clone: IInterface;
var
  I, J: Integer;
  NewEntryArray: TJclStrStrEntryArray;
  NewMap: TJclStrStrHashMap;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  NewMap := TJclStrStrHashMap.Create(FCapacity);
  for I := 0 to FCapacity - 1 do
  begin
    NewEntryArray := NewMap.FBuckets[I].Entries;
    SetLength(NewEntryArray, Length(FBuckets[I].Entries));
    for J := 0 to FBuckets[I].Count - 1 do
    begin
      NewEntryArray[J].Key := FBuckets[I].Entries[J].Key;
      NewEntryArray[J].Value := FBuckets[I].Entries[J].Value;
    end;
    NewMap.FBuckets[I].Count := FBuckets[I].Count;
  end;
  Result := NewMap;
end;

function TJclStrStrHashMap.ContainsKey(const Key: string): Boolean;
var
  I: Integer;
  Bucket: PJclStrStrBucket;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := False;
  if Key = '' then
    Exit;
  Bucket := @FBuckets[FHashFunction(HashString(Key))];
  for I := 0 to Bucket.Count - 1 do
    if Bucket.Entries[I].Key = Key then
    begin
      Result := True;
      Break;
    end;
end;

function TJclStrStrHashMap.ContainsValue(const Value: string): Boolean;
var
  I, J: Integer;
  Bucket: PJclStrStrBucket;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := False;
  if Value = '' then
    Exit;
  for J := 0 to FCapacity - 1 do
  begin
    Bucket := @FBuckets[J];
    for I := 0 to Bucket.Count - 1 do
      if Bucket.Entries[I].Value = Value then
      begin
        Result := True;
        Exit;
      end;
  end;
end;

function TJclStrStrHashMap.Equals(AMap: IStrStrMap): Boolean;
var
  I, J: Integer;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := False;
  if AMap = nil then
    Exit;
  if FCount <> AMap.Size then
    Exit;
  Result := True;
  for I := 0 to FCapacity - 1 do
    for J := 0 to FBuckets[I].Count - 1 do
      if AMap.ContainsKey(FBuckets[I].Entries[J].Key) then
      begin
        if AMap.GetValue(FBuckets[I].Entries[J].Key) <>
          FBuckets[I].Entries[J].Value then
        begin
          Result := False;
          Exit;
        end;
      end
      else
      begin
        Result := False;
        Exit;
      end;
end;

function TJclStrStrHashMap.GetValue(const Key: string): string;
var
  I: Integer;
  Bucket: PJclStrStrBucket;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := '';
  if Key = '' then
    Exit;
  Bucket := @FBuckets[FHashFunction(HashString(Key))];
  for I := 0 to Bucket.Count - 1 do
    if Bucket.Entries[I].Key = Key then
    begin
      Result := Bucket.Entries[I].Value;
      Break;
    end;
end;

procedure TJclStrStrHashMap.GrowEntries(BucketIndex: Integer);
var
  Capacity: Integer;
begin
  Capacity := Length(FBuckets[BucketIndex].Entries);
  if Capacity > 64 then
    Capacity := Capacity + Capacity div 4
  else
    Capacity := Capacity * 4;
  SetLength(FBuckets[BucketIndex].Entries, Capacity);
end;

function TJclStrStrHashMap.HashMul(Key: Cardinal): Cardinal;
const
  A = 0.6180339887; // (sqrt(5) - 1) / 2
begin
  Result := Trunc(FCapacity * (Frac(Key * A)));
end;

function TJclStrStrHashMap.IsEmpty: Boolean;
begin
  Result := FCount = 0;
end;

function TJclStrStrHashMap.KeyOfValue(const Value: string): string;
var
  I, J: Integer;
  Bucket: PJclStrStrBucket;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  if Value = '' then
    Exit;
  for J := 0 to FCapacity - 1 do
  begin
    Bucket := @(FBuckets[J]);
    for I := 0 to Bucket.Count - 1 do
      if Bucket.Entries[I].Value = Value then
      begin
        Result := Bucket.Entries[I].Key;
        Exit;
      end;
  end;
  raise EDCLError.CreateFmt(RsEValueNotFound, [Value]);
end;

function TJclStrStrHashMap.KeySet: IStrSet;
var
  I, J: Integer;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := TJclStrArraySet.Create(FCapacity);
  for I := 0 to FCapacity - 1 do
    for J := 0 to FBuckets[I].Count - 1 do
      Result.Add(FBuckets[I].Entries[J].Key);
end;

procedure TJclStrStrHashMap.PutAll(AMap: IStrStrMap);
var
  It: IStrIterator;
  Key: string;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  if AMap = nil then
    Exit;
  It := AMap.KeySet.First;
  while It.HasNext do
  begin
    Key := It.Next;
    PutValue(Key, AMap.GetValue(Key));
  end;
end;

procedure TJclStrStrHashMap.PutValue(const Key, Value: string);
var
  Index: Integer;
  Bucket: PJclStrStrBucket;
  I: Integer;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  if Key = '' then
    Exit;
  if Value = '' then
    Exit;
  Index := FHashFunction(HashString(Key));
  Bucket := @FBuckets[Index];
  for I := 0 to Bucket.Count - 1 do
    if Bucket.Entries[I].Key = Key then
    begin
      Bucket.Entries[I].Value := Value;
      Exit;
    end;
  if Bucket.Count = Length(Bucket.Entries) then
    GrowEntries(Index);
  Bucket.Entries[Bucket.Count].Key := Key;
  Bucket.Entries[Bucket.Count].Value := Value;
  Inc(Bucket.Count);
  Inc(FCount);
end;

function TJclStrStrHashMap.Remove(const Key: string): string;
var
  Bucket: PJclStrStrBucket;
  I: Integer;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := '';
  if Key = '' then
    Exit;
  Bucket := @FBuckets[FHashFunction(HashString(Key))];
  for I := 0 to Bucket.Count - 1 do
    if Bucket.Entries[I].Key = Key then
    begin
      Result := Bucket.Entries[I].Value;
      if I < Length(Bucket.Entries) - 1 then
        System.Move(Bucket.Entries[I + 1], Bucket.Entries[I],
          (Bucket.Count - I) * SizeOf(TJclStrStrEntry));
      Dec(Bucket.Count);
      Break;
    end;
end;

function TJclStrStrHashMap.Size: Integer;
begin
  Result := FCount;
end;

function TJclStrStrHashMap.Values: IStrCollection;
var
  I, J: Integer;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := TJclStrArrayList.Create(FCapacity);
  for I := 0 to FCapacity - 1 do
    for J := 0 to FBuckets[I].Count - 1 do
      Result.Add(FBuckets[I].Entries[J].Value);
end;

function TJclStrStrHashMap.HashString(const Key: string): Cardinal;
var
  I: Integer;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := 0;
  for I := 1 to Length(Key) do
    Result := Result + Cardinal(Ord(Key[I]) * (I - 1) * 256);
end;

//=== { TJclStrHashMap } =====================================================

constructor TJclStrHashMap.Create(Capacity: Integer = DCLDefaultCapacity;
  AOwnsObjects: Boolean = True);
var
  I: Integer;
begin
  inherited Create;
  FCapacity := Capacity;
  FOwnsObjects := AOwnsObjects;
  SetLength(FBuckets, FCapacity);
  for I := 0 to FCapacity - 1 do
    SetLength(FBuckets[I].Entries, 1);
  FHashFunction := HashMul;
end;

destructor TJclStrHashMap.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TJclStrHashMap.Clear;
var
  I, J: Integer;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  for I := 0 to FCapacity - 1 do
  begin
    for J := 0 to FBuckets[I].Count - 1 do
    begin
      FBuckets[I].Entries[J].Key := '';
      FreeObject(FBuckets[I].Entries[J].Value);
    end;
    FBuckets[I].Count := 0;
  end;
  FCount := 0;
end;

function TJclStrHashMap.Clone: TObject;
var
  I, J: Integer;
  NewEntryArray: TJclStrEntryArray;
  NewMap: TJclStrHashMap;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  NewMap := TJclStrHashMap.Create(FCapacity, False);
  // Only one can have FOwnsObjects = True
  for I := 0 to FCapacity - 1 do
  begin
    NewEntryArray := NewMap.FBuckets[I].Entries;
    SetLength(NewEntryArray, Length(FBuckets[I].Entries));
    for J := 0 to FBuckets[I].Count - 1 do
    begin
      NewEntryArray[J].Key := FBuckets[I].Entries[J].Key;
      NewEntryArray[J].Value := FBuckets[I].Entries[J].Value;
    end;
    NewMap.FBuckets[I].Count := FBuckets[I].Count;
  end;
  Result := NewMap;
end;

function TJclStrHashMap.ContainsKey(const Key: string): Boolean;
var
  I: Integer;
  Bucket: PJclStrBucket;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := False;
  if Key = '' then
    Exit;
  Bucket := @FBuckets[FHashFunction(HashString(Key))];
  for I := 0 to Bucket.Count - 1 do
    if Bucket.Entries[I].Key = Key then
    begin
      Result := True;
      Break;
    end;
end;

function TJclStrHashMap.ContainsValue(Value: TObject): Boolean;
var
  I, J: Integer;
  Bucket: PJclStrBucket;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := False;
  if Value = nil then
    Exit;
  for J := 0 to FCapacity - 1 do
  begin
    Bucket := @FBuckets[J];
    for I := 0 to Bucket.Count - 1 do
      if Bucket.Entries[I].Value = Value then
      begin
        Result := True;
        Exit;
      end;
  end;
end;

function TJclStrHashMap.Equals(AMap: IStrMap): Boolean;
var
  I, J: Integer;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := False;
  if AMap = nil then
    Exit;
  if FCount <> AMap.Size then
    Exit;
  Result := True;
  for I := 0 to FCapacity - 1 do
    for J := 0 to FBuckets[I].Count - 1 do
      if AMap.ContainsKey(FBuckets[I].Entries[J].Key) then
      begin
        if AMap.GetValue(FBuckets[I].Entries[J].Key) <>
          FBuckets[I].Entries[J].Value then
        begin
          Result := False;
          Exit;
        end;
      end
      else
      begin
        Result := False;
        Exit;
      end;
end;

function TJclStrHashMap.GetValue(const Key: string): TObject;
var
  I: Integer;
  Bucket: PJclStrBucket;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := nil;
  if Key = '' then
    Exit;
  I := FHashFunction(HashString(Key));
  Bucket := @FBuckets[I];
  for I := 0 to Bucket.Count - 1 do
    if Bucket.Entries[I].Key = Key then
    begin
      Result := Bucket.Entries[I].Value;
      Break;
    end;
end;

procedure TJclStrHashMap.FreeObject(var AObject: TObject);
begin
  if FOwnsObjects then
  begin
    AObject.Free;
    AObject := nil;
  end;
end;

procedure TJclStrHashMap.GrowEntries(BucketIndex: Integer);
var
  Capacity: Integer;
begin
  Capacity := Length(FBuckets[BucketIndex].Entries);
  if Capacity > 64 then
    Capacity := Capacity + Capacity div 4
  else
    Capacity := Capacity * 4;
  SetLength(FBuckets[BucketIndex].Entries, Capacity);
end;

function TJclStrHashMap.HashMul(Key: Cardinal): Cardinal;
const
  A = 0.6180339887; // (sqrt(5) - 1) / 2
begin
  Result := Trunc(FCapacity * (Frac(Key * A)));
  //Result := LongRec(Key).Bytes[1] and $FF;
end;

function TJclStrHashMap.HashString(const Key: string): Cardinal;
var
  I: Integer;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := 0;
  for I := 1 to Length(Key) do
    Result := Result + Cardinal(Ord(Key[I]) * (I - 1) * 256);
end;

function TJclStrHashMap.IsEmpty: Boolean;
begin
  Result := FCount = 0;
end;

function TJclStrHashMap.KeySet: IStrSet;
var
  I, J: Integer;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := TJclStrArraySet.Create(FCapacity);
  for I := 0 to FCapacity - 1 do
    for J := 0 to FBuckets[I].Count - 1 do
      Result.Add(FBuckets[I].Entries[J].Key);
end;

procedure TJclStrHashMap.PutAll(AMap: IStrMap);
var
  It: IStrIterator;
  Key: string;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  if AMap = nil then
    Exit;
  It := AMap.KeySet.First;
  while It.HasNext do
  begin
    Key := It.Next;
    PutValue(Key, AMap.GetValue(Key));
  end;
end;

procedure TJclStrHashMap.PutValue(const Key: string; Value: TObject);
var
  Index: Integer;
  Bucket: PJclStrBucket;
  I: Integer;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  if Key = '' then
    Exit;
  if Value = nil then
    Exit;
  Index := FHashFunction(HashString(Key));
  Bucket := @FBuckets[Index];
  for I := 0 to Bucket.Count - 1 do
    if Bucket.Entries[I].Key = Key then
    begin
      Bucket.Entries[I].Value := Value;
      Exit;
    end;
  if Bucket.Count = Length(Bucket.Entries) then
    GrowEntries(Index);
  Bucket.Entries[Bucket.Count].Key := Key;
  Bucket.Entries[Bucket.Count].Value := Value;
  Inc(Bucket.Count);
  Inc(FCount);
end;

function TJclStrHashMap.Remove(const Key: string): TObject;
var
  Bucket: PJclStrBucket;
  I: Integer;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := nil;
  if Key = '' then
    Exit;
  Bucket := @FBuckets[FHashFunction(HashString(Key))];
  for I := 0 to Bucket.Count - 1 do
    if Bucket.Entries[I].Key = Key then
    begin
      if not FOwnsObjects then
        Result := Bucket.Entries[I].Value
      else
        Bucket.Entries[I].Value.Free;
      if I < Length(Bucket.Entries) - 1 then
        System.Move(Bucket.Entries[I + 1], Bucket.Entries[I],
          (Bucket.Count - I) * SizeOf(TJclStrEntry));
      Dec(Bucket.Count);
      Break;
    end;
end;

function TJclStrHashMap.Size: Integer;
begin
  Result := FCount;
end;

function TJclStrHashMap.Values: ICollection;
var
  I, J: Integer;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := TJclArrayList.Create(FCapacity, False); // NEVER Owns Objects !
  for I := 0 to FCapacity - 1 do
    for J := 0 to FBuckets[I].Count - 1 do
      Result.Add(FBuckets[I].Entries[J].Value);
end;

//=== { TJclHashMap } ========================================================

constructor TJclHashMap.Create(Capacity: Integer = DCLDefaultCapacity;
  AOwnsObjects: Boolean = True);
var
  I: Integer;
begin
  inherited Create;
  FCapacity := Capacity;
  FOwnsObjects := AOwnsObjects;
  SetLength(FBuckets, FCapacity);
  for I := 0 to FCapacity - 1 do
    SetLength(FBuckets[I].Entries, 64);
  FHashFunction := HashMul;
end;

destructor TJclHashMap.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TJclHashMap.Clear;
var
  I, J: Integer;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  for I := 0 to FCapacity - 1 do
  begin
    for J := 0 to FBuckets[I].Count - 1 do
    begin
      FBuckets[I].Entries[J].Key := nil; // Free key ?
      FreeObject(FBuckets[I].Entries[J].Value);
    end;
    FBuckets[I].Count := 0;
  end;
  FCount := 0;
end;

function TJclHashMap.Clone: TObject;
var
  I, J: Integer;
  NewEntryArray: TJclEntryArray;
  NewMap: TJclHashMap;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  NewMap := TJclHashMap.Create(FCapacity, FOwnsObjects);
  for I := 0 to FCapacity - 1 do
  begin
    NewEntryArray := NewMap.FBuckets[I].Entries;
    SetLength(NewEntryArray, Length(FBuckets[I].Entries));
    for J := 0 to FBuckets[I].Count - 1 do
    begin
      NewEntryArray[J].Key := FBuckets[I].Entries[J].Key;
      NewEntryArray[J].Value := FBuckets[I].Entries[J].Value;
    end;
    NewMap.FBuckets[I].Count := FBuckets[I].Count;
  end;
  Result := NewMap;
end;

function TJclHashMap.ContainsKey(Key: TObject): Boolean;
var
  I: Integer;
  Bucket: PJclBucket;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := False;
  if Key = nil then
    Exit;
  Bucket := @FBuckets[FHashFunction(Integer(Key))];
  for I := 0 to Bucket.Count - 1 do
    if Bucket.Entries[I].Key = Key then
    begin
      Result := True;
      Exit;
    end;
end;

function TJclHashMap.ContainsValue(Value: TObject): Boolean;
var
  I, J: Integer;
  Bucket: PJclBucket;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := False;
  if Value = nil then
    Exit;
  for J := 0 to FCapacity - 1 do
  begin
    Bucket := @FBuckets[J];
    for I := 0 to Bucket.Count - 1 do
      if Bucket.Entries[I].Value = Value then
      begin
        Result := True;
        Exit;
      end;
  end;
end;

function TJclHashMap.Equals(AMap: IMap): Boolean;
var
  I, J: Integer;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := False;
  if AMap = nil then
    Exit;
  if FCount <> AMap.Size then
    Exit;
  Result := True;
  for I := 0 to FCapacity - 1 do
    for J := 0 to FBuckets[I].Count - 1 do
      if AMap.ContainsKey(FBuckets[I].Entries[J].Key) then
      begin
        if AMap.GetValue(FBuckets[I].Entries[J].Key) <>
          FBuckets[I].Entries[J].Value then
        begin
          Result := False;
          Exit;
        end;
      end
      else
      begin
        Result := False;
        Exit;
      end;
end;

procedure TJclHashMap.FreeObject(var AObject: TObject);
begin
  if FOwnsObjects then
  begin
    AObject.Free;
    AObject := nil;
  end;
end;

function TJclHashMap.GetValue(Key: TObject): TObject;
var
  I: Integer;
  Bucket: PJclBucket;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := nil;
  if Key = nil then
    Exit;
  Bucket := @FBuckets[FHashFunction(Integer(Key))];
  for I := 0 to Bucket.Count - 1 do
    if Bucket.Entries[I].Key = Key then
    begin
      Result := Bucket.Entries[I].Value;
      Break;
    end;
end;

procedure TJclHashMap.GrowEntries(BucketIndex: Integer);
var
  Capacity: Integer;
  OldCapacity: Integer;
begin
  OldCapacity := Length(FBuckets[BucketIndex].Entries);
  if OldCapacity > 64 then
    Capacity := OldCapacity + OldCapacity div 4
  else
    Capacity := OldCapacity * 4;
  SetLength(FBuckets[BucketIndex].Entries, Capacity);
end;

function TJclHashMap.HashMul(Key: Cardinal): Cardinal;
const
  A = 0.6180339887; // (sqrt(5) - 1) / 2
begin
  Result := Trunc(FCapacity * (Frac(Key * A)));
  //Result := LongRec(Key).Bytes[1] and $FF;
end;

function TJclHashMap.IsEmpty: Boolean;
begin
  Result := FCount = 0;
end;

function TJclHashMap.KeySet: ISet;
var
  I, J: Integer;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := TJclArraySet.Create(FCapacity, False); // NEVER Owns Objects !
  for I := 0 to FCapacity - 1 do
    for J := 0 to FBuckets[I].Count - 1 do
      Result.Add(FBuckets[I].Entries[J].Key);
end;

procedure TJclHashMap.PutAll(AMap: IMap);
var
  It: IIterator;
  Key: TObject;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  if AMap = nil then
    Exit;
  It := AMap.KeySet.First;
  while It.HasNext do
  begin
    Key := It.Next;
    PutValue(Key, AMap.GetValue(Key));
  end;
end;

procedure TJclHashMap.PutValue(Key, Value: TObject);
var
  Index: Integer;
  Bucket: PJclBucket;
  I: Integer;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  if Key = nil then
    Exit;
  if Value = nil then
    Exit;
  Index := FHashFunction(Integer(Key));
  Bucket := @FBuckets[Index];
  for I := 0 to Bucket.Count - 1 do
    if Bucket.Entries[I].Key = Key then
    begin
      Bucket.Entries[I].Value := Value;
      Exit;
    end;
  if Bucket.Count = Length(Bucket.Entries) then
    GrowEntries(Index);
  begin
    Bucket.Entries[Bucket.Count].Key := Key;
    Bucket.Entries[Bucket.Count].Value := Value;
  end;
  Inc(Bucket.Count);
  Inc(FCount);
end;

function TJclHashMap.Remove(Key: TObject): TObject;
var
  Bucket: PJclBucket;
  I: Integer;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := nil;
  if Key = nil then
    Exit;
  Bucket := @FBuckets[FHashFunction(Integer(Key))];
  for I := 0 to Bucket.Count - 1 do
    if Bucket.Entries[I].Key = Key then
    begin
      if not FOwnsObjects then
        Result := Bucket.Entries[I].Value
      else
        Bucket.Entries[I].Value.Free;
      if I < Length(Bucket.Entries) - 1 then
        System.Move(Bucket.Entries[I + 1], Bucket.Entries[I],
          (Bucket.Count - I) * SizeOf(TJclEntry));
      Dec(Bucket.Count);
      Break;
    end;
end;

function TJclHashMap.Size: Integer;
begin
  Result := FCount;
end;

function TJclHashMap.Values: ICollection;
var
  I, J: Integer;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := TJclArrayList.Create(FCapacity, False); // NEVER Owns Objects !
  for I := 0 to FCapacity - 1 do
    for J := 0 to FBuckets[I].Count - 1 do
      Result.Add(FBuckets[I].Entries[J].Value);
end;

end.

