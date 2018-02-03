{**************************************************************************************************}
{  WARNING:  JEDI preprocessor generated unit.  Do not edit.                                       }
{**************************************************************************************************}

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
{ The Original Code is Queue.pas.                                                                  }
{                                                                                                  }
{ The Initial Developer of the Original Code is Jean-Philippe BEMPEL aka RDM. Portions created by  }
{ Jean-Philippe BEMPEL are Copyright (C) Jean-Philippe BEMPEL (rdm_30 att yahoo dott com)          }
{ All rights reserved.                                                                             }
{                                                                                                  }
{ Contributors:                                                                                    }
{   Florent Ouchet (outchy)                                                                        }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ The Delphi Container Library                                                                     }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclQueues;

{$I jcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  JclAlgorithms,
  JclBase, JclAbstractContainers, JclContainerIntf, JclSynch;


type
  TJclIntfQueue = class(TJclIntfAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclBaseContainer,
    IJclIntfContainer, IJclIntfEqualityComparer,
    IJclIntfQueue)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  private
    FElements: TDynIInterfaceArray;
    FHead: Integer;
    FTail: Integer;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclIntfQueue }
    procedure Clear;
    function Contains(const AInterface: IInterface): Boolean;
    function Dequeue: IInterface;
    function Empty: Boolean;
    function Enqueue(const AInterface: IInterface): Boolean;
    function Peek: IInterface;
    function Size: Integer;
  end;

  TJclAnsiStrQueue = class(TJclAnsiStrAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclBaseContainer,
    IJclAnsiStrContainer, IJclAnsiStrEqualityComparer, IJclStrBaseContainer,
    IJclAnsiStrQueue)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  private
    FElements: TDynAnsiStringArray;
    FHead: Integer;
    FTail: Integer;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclAnsiStrQueue }
    procedure Clear;
    function Contains(const AString: AnsiString): Boolean;
    function Dequeue: AnsiString;
    function Empty: Boolean;
    function Enqueue(const AString: AnsiString): Boolean;
    function Peek: AnsiString;
    function Size: Integer;
  end;

  TJclWideStrQueue = class(TJclWideStrAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclBaseContainer,
    IJclWideStrContainer, IJclWideStrEqualityComparer, IJclStrBaseContainer,
    IJclWideStrQueue)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  private
    FElements: TDynWideStringArray;
    FHead: Integer;
    FTail: Integer;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclWideStrQueue }
    procedure Clear;
    function Contains(const AString: WideString): Boolean;
    function Dequeue: WideString;
    function Empty: Boolean;
    function Enqueue(const AString: WideString): Boolean;
    function Peek: WideString;
    function Size: Integer;
  end;

  {$IFDEF SUPPORTS_UNICODE_STRING}
  TJclUnicodeStrQueue = class(TJclUnicodeStrAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclBaseContainer,
    IJclUnicodeStrContainer, IJclUnicodeStrEqualityComparer, IJclStrBaseContainer,
    IJclUnicodeStrQueue)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  private
    FElements: TDynUnicodeStringArray;
    FHead: Integer;
    FTail: Integer;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclUnicodeStrQueue }
    procedure Clear;
    function Contains(const AString: UnicodeString): Boolean;
    function Dequeue: UnicodeString;
    function Empty: Boolean;
    function Enqueue(const AString: UnicodeString): Boolean;
    function Peek: UnicodeString;
    function Size: Integer;
  end;
  {$ENDIF SUPPORTS_UNICODE_STRING}

  {$IFDEF CONTAINER_ANSISTR}
  TJclStrQueue = TJclAnsiStrQueue;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  TJclStrQueue = TJclWideStrQueue;
  {$ENDIF CONTAINER_WIDESTR}
  {$IFDEF CONTAINER_UNICODESTR}
  TJclStrQueue = TJclUnicodeStrQueue;
  {$ENDIF CONTAINER_UNICODESTR}

  TJclSingleQueue = class(TJclSingleAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclBaseContainer,
    IJclSingleContainer, IJclSingleEqualityComparer,
    IJclSingleQueue)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  private
    FElements: TDynSingleArray;
    FHead: Integer;
    FTail: Integer;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclSingleQueue }
    procedure Clear;
    function Contains(const AValue: Single): Boolean;
    function Dequeue: Single;
    function Empty: Boolean;
    function Enqueue(const AValue: Single): Boolean;
    function Peek: Single;
    function Size: Integer;
  end;

  TJclDoubleQueue = class(TJclDoubleAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclBaseContainer,
    IJclDoubleContainer, IJclDoubleEqualityComparer,
    IJclDoubleQueue)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  private
    FElements: TDynDoubleArray;
    FHead: Integer;
    FTail: Integer;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclDoubleQueue }
    procedure Clear;
    function Contains(const AValue: Double): Boolean;
    function Dequeue: Double;
    function Empty: Boolean;
    function Enqueue(const AValue: Double): Boolean;
    function Peek: Double;
    function Size: Integer;
  end;

  TJclExtendedQueue = class(TJclExtendedAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclBaseContainer,
    IJclExtendedContainer, IJclExtendedEqualityComparer,
    IJclExtendedQueue)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  private
    FElements: TDynExtendedArray;
    FHead: Integer;
    FTail: Integer;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclExtendedQueue }
    procedure Clear;
    function Contains(const AValue: Extended): Boolean;
    function Dequeue: Extended;
    function Empty: Boolean;
    function Enqueue(const AValue: Extended): Boolean;
    function Peek: Extended;
    function Size: Integer;
  end;

  {$IFDEF MATH_SINGLE_PRECISION}
  TJclFloatQueue = TJclSingleQueue;
  {$ENDIF MATH_SINGLE_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  TJclFloatQueue = TJclDoubleQueue;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_EXTENDED_PRECISION}
  TJclFloatQueue = TJclExtendedQueue;
  {$ENDIF MATH_EXTENDED_PRECISION}

  TJclIntegerQueue = class(TJclIntegerAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclBaseContainer,
    IJclIntegerContainer, IJclIntegerEqualityComparer,
    IJclIntegerQueue)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  private
    FElements: TDynIntegerArray;
    FHead: Integer;
    FTail: Integer;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclIntegerQueue }
    procedure Clear;
    function Contains(AValue: Integer): Boolean;
    function Dequeue: Integer;
    function Empty: Boolean;
    function Enqueue(AValue: Integer): Boolean;
    function Peek: Integer;
    function Size: Integer;
  end;

  TJclCardinalQueue = class(TJclCardinalAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclBaseContainer,
    IJclCardinalContainer, IJclCardinalEqualityComparer,
    IJclCardinalQueue)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  private
    FElements: TDynCardinalArray;
    FHead: Integer;
    FTail: Integer;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclCardinalQueue }
    procedure Clear;
    function Contains(AValue: Cardinal): Boolean;
    function Dequeue: Cardinal;
    function Empty: Boolean;
    function Enqueue(AValue: Cardinal): Boolean;
    function Peek: Cardinal;
    function Size: Integer;
  end;

  TJclInt64Queue = class(TJclInt64AbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclBaseContainer,
    IJclInt64Container, IJclInt64EqualityComparer,
    IJclInt64Queue)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  private
    FElements: TDynInt64Array;
    FHead: Integer;
    FTail: Integer;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclInt64Queue }
    procedure Clear;
    function Contains(const AValue: Int64): Boolean;
    function Dequeue: Int64;
    function Empty: Boolean;
    function Enqueue(const AValue: Int64): Boolean;
    function Peek: Int64;
    function Size: Integer;
  end;

  TJclPtrQueue = class(TJclPtrAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclBaseContainer,
    IJclPtrContainer, IJclPtrEqualityComparer,
    IJclPtrQueue)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  private
    FElements: TDynPointerArray;
    FHead: Integer;
    FTail: Integer;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclPtrQueue }
    procedure Clear;
    function Contains(APtr: Pointer): Boolean;
    function Dequeue: Pointer;
    function Empty: Boolean;
    function Enqueue(APtr: Pointer): Boolean;
    function Peek: Pointer;
    function Size: Integer;
  end;

  TJclQueue = class(TJclAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclBaseContainer,
    IJclContainer, IJclEqualityComparer, IJclObjectOwner,
    IJclQueue)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  private
    FElements: TDynObjectArray;
    FHead: Integer;
    FTail: Integer;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
  public
    constructor Create(ACapacity: Integer; AOwnsObjects: Boolean);
    destructor Destroy; override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclQueue }
    procedure Clear;
    function Contains(AObject: TObject): Boolean;
    function Dequeue: TObject;
    function Empty: Boolean;
    function Enqueue(AObject: TObject): Boolean;
    function Peek: TObject;
    function Size: Integer;
  end;


  {$IFDEF SUPPORTS_GENERICS}
  //DOM-IGNORE-BEGIN

  TJclQueue<T> = class(TJclAbstractContainer<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclBaseContainer,
    IJclContainer<T>, IJclEqualityComparer<T>, IJclItemOwner<T>,
    IJclQueue<T>)
  protected
    type
      TDynArray = array of T;
    procedure MoveArray(var List: TDynArray; FromIndex, ToIndex, Count: SizeInt);
  private
    FElements: TDynArray;
    FHead: Integer;
    FTail: Integer;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
  public
    constructor Create(ACapacity: Integer; AOwnsItems: Boolean);
    destructor Destroy; override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclQueue<T> }
    procedure Clear;
    function Contains(const AItem: T): Boolean;
    function Dequeue: T;
    function Empty: Boolean;
    function Enqueue(const AItem: T): Boolean;
    function Peek: T;
    function Size: Integer;
  end;

  // E = external helper to compare items for equality (GetHashCode is not used)
  TJclQueueE<T> = class(TJclQueue<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclBaseContainer, IJclContainer<T>, IJclQueue<T>, IJclItemOwner<T>)
  private
    FEqualityComparer: IEqualityComparer<T>;
  protected
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(const AEqualityComparer: IEqualityComparer<T>; ACapacity: Integer; AOwnsItems: Boolean);
    { IJclEqualityComparer<T> }
    function ItemsEqual(const A, B: T): Boolean; override;
    property EqualityComparer: IEqualityComparer<T> read FEqualityComparer write FEqualityComparer;
  end;

  // F = function to compare items for equality
  TJclQueueF<T> = class(TJclQueue<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclBaseContainer, IJclContainer<T>, IJclQueue<T>, IJclItemOwner<T>)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(AEqualityCompare: TEqualityCompare<T>; ACapacity: Integer; AOwnsItems: Boolean);
  end;

  // I = items can compare themselves to an other
  TJclQueueI<T: IEquatable<T>> = class(TJclQueue<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclBaseContainer, IJclContainer<T>, IJclQueue<T>, IJclItemOwner<T>)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    { IJclEqualityComparer<T> }
    function ItemsEqual(const A, B: T): Boolean; override;
  end;

  //DOM-IGNORE-END
  {$ENDIF SUPPORTS_GENERICS}

{$IFDEF BCB}
{$IFDEF WIN64}
  {$HPPEMIT '#ifdef MANAGED_INTERFACE_OPERATORS'}
  {$HPPEMIT ' #undef MANAGED_INTERFACE_OPERATORS'}
  {$HPPEMIT ' #define JclQueues_MANAGED_INTERFACE_OPERATORS'}
  {$HPPEMIT '#endif'}

  {$HPPEMIT END '#ifdef JclQueues_MANAGED_INTERFACE_OPERATORS'}
  {$HPPEMIT END ' #define MANAGED_INTERFACE_OPERATORS'}
  {$HPPEMIT END ' #undef JclQueues_MANAGED_INTERFACE_OPERATORS'}
  {$HPPEMIT END '#endif'}
{$ENDIF WIN64}
{$ENDIF BCB}

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
  System.SysUtils;
  {$ELSE ~HAS_UNITSCOPE}
  SysUtils;
  {$ENDIF ~HAS_UNITSCOPE}

//=== { TJclIntfQueue } =======================================================

constructor TJclIntfQueue.Create(ACapacity: Integer);
begin
  inherited Create();
  FHead := 0;
  FTail := 0;
  SetCapacity(ACapacity);
end;

destructor TJclIntfQueue.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclIntfQueue.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclIntfQueue;
  I: Integer;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclIntfQueue then
  begin
    ADest := TJclIntfQueue(Dest);
    ADest.Clear;
    ADest.SetCapacity(Size + 1);
    I := FHead;
    while I <> FTail do
    begin
      ADest.Enqueue(FElements[I]);
      Inc(I);
      if I = FCapacity then
        I := 0;
    end;
  end;
end;

procedure TJclIntfQueue.Clear;
var
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
     I := FHead;
     while I <> FTail do
     begin
       FreeObject(FElements[I]);
       Inc(I);
       if I = FCapacity then
         I := 0;
     end;
     FHead := 0;
     FTail := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfQueue.Contains(const AInterface: IInterface): Boolean;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    I := FHead;
    while I <> FTail do
    begin
      if ItemsEqual(FElements[I], AInterface) then
      begin
        Result := True;
        Break;
      end;
      Inc(I);
      if I = FCapacity then
        I := 0;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfQueue.Dequeue: IInterface;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if FTail <> FHead then
    begin
      Result := FElements[FHead];
      FElements[FHead] := nil;
      Inc(FHead);
      if FHead = FCapacity then
        FHead := 0;
      AutoPack;
    end
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfQueue.Empty: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := FTail = FHead;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfQueue.Enqueue(const AInterface: IInterface): Boolean;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if (FTail = (FHead - 1)) or (FTail = (FHead + FCapacity - 1)) then
      AutoGrow;
    Result := (FTail <> (FHead - 1)) and (FTail <> (FHead + FCapacity - 1));
    if Result then
    begin
      FElements[FTail] := AInterface;
      Inc(FTail);
      if FTail = FCapacity then
        FTail := 0;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfQueue.Pack;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    SetCapacity(Size + 1);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfQueue.Peek: IInterface;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if FTail <> FHead then
      Result := FElements[FHead]
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfQueue.SetCapacity(Value: Integer);
var
  NewHead: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if Value < 1 then
      raise EJclIllegalQueueCapacityError.Create;
    if Value <= Size then
      raise EJclOutOfBoundsError.Create;

    if FHead > FTail then // looped
    begin
      NewHead := FHead + Value - FCapacity;
      if Value > FCapacity then
        // growing
        SetLength(FElements, Value);
      MoveArray(FElements, FHead, NewHead, FCapacity - FHead);
      if FCapacity > Value then
        // packing
        SetLength(FElements, Value);
      FHead := NewHead;
    end
    else
    begin
      // unlooped
      if Value < FCapacity then
      begin
        MoveArray(FElements, FHead, 0, FTail - FHead);
        Dec(FTail, FHead);
        FHead := 0;
      end;
      SetLength(FElements, Value);
    end;
    inherited SetCapacity(Value);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfQueue.Size: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    if FHead > FTail then
      Result := FCapacity - FHead + FTail  // looped
    else
      Result := FTail - FHead;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfQueue.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfQueue.Create(Size + 1);
  AssignPropertiesTo(Result);
end;

//=== { TJclAnsiStrQueue } =======================================================

constructor TJclAnsiStrQueue.Create(ACapacity: Integer);
begin
  inherited Create();
  FHead := 0;
  FTail := 0;
  SetCapacity(ACapacity);
end;

destructor TJclAnsiStrQueue.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclAnsiStrQueue.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclAnsiStrQueue;
  I: Integer;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclAnsiStrQueue then
  begin
    ADest := TJclAnsiStrQueue(Dest);
    ADest.Clear;
    ADest.SetCapacity(Size + 1);
    I := FHead;
    while I <> FTail do
    begin
      ADest.Enqueue(FElements[I]);
      Inc(I);
      if I = FCapacity then
        I := 0;
    end;
  end;
end;

procedure TJclAnsiStrQueue.Clear;
var
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
     I := FHead;
     while I <> FTail do
     begin
       FreeString(FElements[I]);
       Inc(I);
       if I = FCapacity then
         I := 0;
     end;
     FHead := 0;
     FTail := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrQueue.Contains(const AString: AnsiString): Boolean;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    I := FHead;
    while I <> FTail do
    begin
      if ItemsEqual(FElements[I], AString) then
      begin
        Result := True;
        Break;
      end;
      Inc(I);
      if I = FCapacity then
        I := 0;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrQueue.Dequeue: AnsiString;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := '';
    if FTail <> FHead then
    begin
      Result := FElements[FHead];
      FElements[FHead] := '';
      Inc(FHead);
      if FHead = FCapacity then
        FHead := 0;
      AutoPack;
    end
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrQueue.Empty: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := FTail = FHead;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrQueue.Enqueue(const AString: AnsiString): Boolean;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if (FTail = (FHead - 1)) or (FTail = (FHead + FCapacity - 1)) then
      AutoGrow;
    Result := (FTail <> (FHead - 1)) and (FTail <> (FHead + FCapacity - 1));
    if Result then
    begin
      FElements[FTail] := AString;
      Inc(FTail);
      if FTail = FCapacity then
        FTail := 0;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclAnsiStrQueue.Pack;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    SetCapacity(Size + 1);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrQueue.Peek: AnsiString;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := '';
    if FTail <> FHead then
      Result := FElements[FHead]
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclAnsiStrQueue.SetCapacity(Value: Integer);
var
  NewHead: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if Value < 1 then
      raise EJclIllegalQueueCapacityError.Create;
    if Value <= Size then
      raise EJclOutOfBoundsError.Create;

    if FHead > FTail then // looped
    begin
      NewHead := FHead + Value - FCapacity;
      if Value > FCapacity then
        // growing
        SetLength(FElements, Value);
      MoveArray(FElements, FHead, NewHead, FCapacity - FHead);
      if FCapacity > Value then
        // packing
        SetLength(FElements, Value);
      FHead := NewHead;
    end
    else
    begin
      // unlooped
      if Value < FCapacity then
      begin
        MoveArray(FElements, FHead, 0, FTail - FHead);
        Dec(FTail, FHead);
        FHead := 0;
      end;
      SetLength(FElements, Value);
    end;
    inherited SetCapacity(Value);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrQueue.Size: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    if FHead > FTail then
      Result := FCapacity - FHead + FTail  // looped
    else
      Result := FTail - FHead;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrQueue.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclAnsiStrQueue.Create(Size + 1);
  AssignPropertiesTo(Result);
end;

//=== { TJclWideStrQueue } =======================================================

constructor TJclWideStrQueue.Create(ACapacity: Integer);
begin
  inherited Create();
  FHead := 0;
  FTail := 0;
  SetCapacity(ACapacity);
end;

destructor TJclWideStrQueue.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclWideStrQueue.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclWideStrQueue;
  I: Integer;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclWideStrQueue then
  begin
    ADest := TJclWideStrQueue(Dest);
    ADest.Clear;
    ADest.SetCapacity(Size + 1);
    I := FHead;
    while I <> FTail do
    begin
      ADest.Enqueue(FElements[I]);
      Inc(I);
      if I = FCapacity then
        I := 0;
    end;
  end;
end;

procedure TJclWideStrQueue.Clear;
var
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
     I := FHead;
     while I <> FTail do
     begin
       FreeString(FElements[I]);
       Inc(I);
       if I = FCapacity then
         I := 0;
     end;
     FHead := 0;
     FTail := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrQueue.Contains(const AString: WideString): Boolean;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    I := FHead;
    while I <> FTail do
    begin
      if ItemsEqual(FElements[I], AString) then
      begin
        Result := True;
        Break;
      end;
      Inc(I);
      if I = FCapacity then
        I := 0;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrQueue.Dequeue: WideString;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := '';
    if FTail <> FHead then
    begin
      Result := FElements[FHead];
      FElements[FHead] := '';
      Inc(FHead);
      if FHead = FCapacity then
        FHead := 0;
      AutoPack;
    end
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrQueue.Empty: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := FTail = FHead;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrQueue.Enqueue(const AString: WideString): Boolean;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if (FTail = (FHead - 1)) or (FTail = (FHead + FCapacity - 1)) then
      AutoGrow;
    Result := (FTail <> (FHead - 1)) and (FTail <> (FHead + FCapacity - 1));
    if Result then
    begin
      FElements[FTail] := AString;
      Inc(FTail);
      if FTail = FCapacity then
        FTail := 0;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclWideStrQueue.Pack;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    SetCapacity(Size + 1);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrQueue.Peek: WideString;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := '';
    if FTail <> FHead then
      Result := FElements[FHead]
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclWideStrQueue.SetCapacity(Value: Integer);
var
  NewHead: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if Value < 1 then
      raise EJclIllegalQueueCapacityError.Create;
    if Value <= Size then
      raise EJclOutOfBoundsError.Create;

    if FHead > FTail then // looped
    begin
      NewHead := FHead + Value - FCapacity;
      if Value > FCapacity then
        // growing
        SetLength(FElements, Value);
      MoveArray(FElements, FHead, NewHead, FCapacity - FHead);
      if FCapacity > Value then
        // packing
        SetLength(FElements, Value);
      FHead := NewHead;
    end
    else
    begin
      // unlooped
      if Value < FCapacity then
      begin
        MoveArray(FElements, FHead, 0, FTail - FHead);
        Dec(FTail, FHead);
        FHead := 0;
      end;
      SetLength(FElements, Value);
    end;
    inherited SetCapacity(Value);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrQueue.Size: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    if FHead > FTail then
      Result := FCapacity - FHead + FTail  // looped
    else
      Result := FTail - FHead;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrQueue.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclWideStrQueue.Create(Size + 1);
  AssignPropertiesTo(Result);
end;

{$IFDEF SUPPORTS_UNICODE_STRING}
//=== { TJclUnicodeStrQueue } =======================================================

constructor TJclUnicodeStrQueue.Create(ACapacity: Integer);
begin
  inherited Create();
  FHead := 0;
  FTail := 0;
  SetCapacity(ACapacity);
end;

destructor TJclUnicodeStrQueue.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclUnicodeStrQueue.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclUnicodeStrQueue;
  I: Integer;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclUnicodeStrQueue then
  begin
    ADest := TJclUnicodeStrQueue(Dest);
    ADest.Clear;
    ADest.SetCapacity(Size + 1);
    I := FHead;
    while I <> FTail do
    begin
      ADest.Enqueue(FElements[I]);
      Inc(I);
      if I = FCapacity then
        I := 0;
    end;
  end;
end;

procedure TJclUnicodeStrQueue.Clear;
var
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
     I := FHead;
     while I <> FTail do
     begin
       FreeString(FElements[I]);
       Inc(I);
       if I = FCapacity then
         I := 0;
     end;
     FHead := 0;
     FTail := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrQueue.Contains(const AString: UnicodeString): Boolean;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    I := FHead;
    while I <> FTail do
    begin
      if ItemsEqual(FElements[I], AString) then
      begin
        Result := True;
        Break;
      end;
      Inc(I);
      if I = FCapacity then
        I := 0;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrQueue.Dequeue: UnicodeString;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := '';
    if FTail <> FHead then
    begin
      Result := FElements[FHead];
      FElements[FHead] := '';
      Inc(FHead);
      if FHead = FCapacity then
        FHead := 0;
      AutoPack;
    end
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrQueue.Empty: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := FTail = FHead;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrQueue.Enqueue(const AString: UnicodeString): Boolean;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if (FTail = (FHead - 1)) or (FTail = (FHead + FCapacity - 1)) then
      AutoGrow;
    Result := (FTail <> (FHead - 1)) and (FTail <> (FHead + FCapacity - 1));
    if Result then
    begin
      FElements[FTail] := AString;
      Inc(FTail);
      if FTail = FCapacity then
        FTail := 0;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclUnicodeStrQueue.Pack;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    SetCapacity(Size + 1);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrQueue.Peek: UnicodeString;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := '';
    if FTail <> FHead then
      Result := FElements[FHead]
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclUnicodeStrQueue.SetCapacity(Value: Integer);
var
  NewHead: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if Value < 1 then
      raise EJclIllegalQueueCapacityError.Create;
    if Value <= Size then
      raise EJclOutOfBoundsError.Create;

    if FHead > FTail then // looped
    begin
      NewHead := FHead + Value - FCapacity;
      if Value > FCapacity then
        // growing
        SetLength(FElements, Value);
      MoveArray(FElements, FHead, NewHead, FCapacity - FHead);
      if FCapacity > Value then
        // packing
        SetLength(FElements, Value);
      FHead := NewHead;
    end
    else
    begin
      // unlooped
      if Value < FCapacity then
      begin
        MoveArray(FElements, FHead, 0, FTail - FHead);
        Dec(FTail, FHead);
        FHead := 0;
      end;
      SetLength(FElements, Value);
    end;
    inherited SetCapacity(Value);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrQueue.Size: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    if FHead > FTail then
      Result := FCapacity - FHead + FTail  // looped
    else
      Result := FTail - FHead;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrQueue.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclUnicodeStrQueue.Create(Size + 1);
  AssignPropertiesTo(Result);
end;

{$ENDIF SUPPORTS_UNICODE_STRING}

//=== { TJclSingleQueue } =======================================================

constructor TJclSingleQueue.Create(ACapacity: Integer);
begin
  inherited Create();
  FHead := 0;
  FTail := 0;
  SetCapacity(ACapacity);
end;

destructor TJclSingleQueue.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclSingleQueue.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclSingleQueue;
  I: Integer;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclSingleQueue then
  begin
    ADest := TJclSingleQueue(Dest);
    ADest.Clear;
    ADest.SetCapacity(Size + 1);
    I := FHead;
    while I <> FTail do
    begin
      ADest.Enqueue(FElements[I]);
      Inc(I);
      if I = FCapacity then
        I := 0;
    end;
  end;
end;

procedure TJclSingleQueue.Clear;
var
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
     I := FHead;
     while I <> FTail do
     begin
       FreeSingle(FElements[I]);
       Inc(I);
       if I = FCapacity then
         I := 0;
     end;
     FHead := 0;
     FTail := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleQueue.Contains(const AValue: Single): Boolean;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    I := FHead;
    while I <> FTail do
    begin
      if ItemsEqual(FElements[I], AValue) then
      begin
        Result := True;
        Break;
      end;
      Inc(I);
      if I = FCapacity then
        I := 0;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleQueue.Dequeue: Single;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := 0.0;
    if FTail <> FHead then
    begin
      Result := FElements[FHead];
      FElements[FHead] := 0.0;
      Inc(FHead);
      if FHead = FCapacity then
        FHead := 0;
      AutoPack;
    end
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleQueue.Empty: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := FTail = FHead;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleQueue.Enqueue(const AValue: Single): Boolean;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if (FTail = (FHead - 1)) or (FTail = (FHead + FCapacity - 1)) then
      AutoGrow;
    Result := (FTail <> (FHead - 1)) and (FTail <> (FHead + FCapacity - 1));
    if Result then
    begin
      FElements[FTail] := AValue;
      Inc(FTail);
      if FTail = FCapacity then
        FTail := 0;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclSingleQueue.Pack;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    SetCapacity(Size + 1);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleQueue.Peek: Single;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := 0.0;
    if FTail <> FHead then
      Result := FElements[FHead]
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclSingleQueue.SetCapacity(Value: Integer);
var
  NewHead: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if Value < 1 then
      raise EJclIllegalQueueCapacityError.Create;
    if Value <= Size then
      raise EJclOutOfBoundsError.Create;

    if FHead > FTail then // looped
    begin
      NewHead := FHead + Value - FCapacity;
      if Value > FCapacity then
        // growing
        SetLength(FElements, Value);
      MoveArray(FElements, FHead, NewHead, FCapacity - FHead);
      if FCapacity > Value then
        // packing
        SetLength(FElements, Value);
      FHead := NewHead;
    end
    else
    begin
      // unlooped
      if Value < FCapacity then
      begin
        MoveArray(FElements, FHead, 0, FTail - FHead);
        Dec(FTail, FHead);
        FHead := 0;
      end;
      SetLength(FElements, Value);
    end;
    inherited SetCapacity(Value);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleQueue.Size: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    if FHead > FTail then
      Result := FCapacity - FHead + FTail  // looped
    else
      Result := FTail - FHead;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleQueue.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclSingleQueue.Create(Size + 1);
  AssignPropertiesTo(Result);
end;

//=== { TJclDoubleQueue } =======================================================

constructor TJclDoubleQueue.Create(ACapacity: Integer);
begin
  inherited Create();
  FHead := 0;
  FTail := 0;
  SetCapacity(ACapacity);
end;

destructor TJclDoubleQueue.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclDoubleQueue.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclDoubleQueue;
  I: Integer;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclDoubleQueue then
  begin
    ADest := TJclDoubleQueue(Dest);
    ADest.Clear;
    ADest.SetCapacity(Size + 1);
    I := FHead;
    while I <> FTail do
    begin
      ADest.Enqueue(FElements[I]);
      Inc(I);
      if I = FCapacity then
        I := 0;
    end;
  end;
end;

procedure TJclDoubleQueue.Clear;
var
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
     I := FHead;
     while I <> FTail do
     begin
       FreeDouble(FElements[I]);
       Inc(I);
       if I = FCapacity then
         I := 0;
     end;
     FHead := 0;
     FTail := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleQueue.Contains(const AValue: Double): Boolean;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    I := FHead;
    while I <> FTail do
    begin
      if ItemsEqual(FElements[I], AValue) then
      begin
        Result := True;
        Break;
      end;
      Inc(I);
      if I = FCapacity then
        I := 0;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleQueue.Dequeue: Double;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := 0.0;
    if FTail <> FHead then
    begin
      Result := FElements[FHead];
      FElements[FHead] := 0.0;
      Inc(FHead);
      if FHead = FCapacity then
        FHead := 0;
      AutoPack;
    end
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleQueue.Empty: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := FTail = FHead;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleQueue.Enqueue(const AValue: Double): Boolean;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if (FTail = (FHead - 1)) or (FTail = (FHead + FCapacity - 1)) then
      AutoGrow;
    Result := (FTail <> (FHead - 1)) and (FTail <> (FHead + FCapacity - 1));
    if Result then
    begin
      FElements[FTail] := AValue;
      Inc(FTail);
      if FTail = FCapacity then
        FTail := 0;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclDoubleQueue.Pack;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    SetCapacity(Size + 1);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleQueue.Peek: Double;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := 0.0;
    if FTail <> FHead then
      Result := FElements[FHead]
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclDoubleQueue.SetCapacity(Value: Integer);
var
  NewHead: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if Value < 1 then
      raise EJclIllegalQueueCapacityError.Create;
    if Value <= Size then
      raise EJclOutOfBoundsError.Create;

    if FHead > FTail then // looped
    begin
      NewHead := FHead + Value - FCapacity;
      if Value > FCapacity then
        // growing
        SetLength(FElements, Value);
      MoveArray(FElements, FHead, NewHead, FCapacity - FHead);
      if FCapacity > Value then
        // packing
        SetLength(FElements, Value);
      FHead := NewHead;
    end
    else
    begin
      // unlooped
      if Value < FCapacity then
      begin
        MoveArray(FElements, FHead, 0, FTail - FHead);
        Dec(FTail, FHead);
        FHead := 0;
      end;
      SetLength(FElements, Value);
    end;
    inherited SetCapacity(Value);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleQueue.Size: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    if FHead > FTail then
      Result := FCapacity - FHead + FTail  // looped
    else
      Result := FTail - FHead;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleQueue.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclDoubleQueue.Create(Size + 1);
  AssignPropertiesTo(Result);
end;

//=== { TJclExtendedQueue } =======================================================

constructor TJclExtendedQueue.Create(ACapacity: Integer);
begin
  inherited Create();
  FHead := 0;
  FTail := 0;
  SetCapacity(ACapacity);
end;

destructor TJclExtendedQueue.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclExtendedQueue.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclExtendedQueue;
  I: Integer;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclExtendedQueue then
  begin
    ADest := TJclExtendedQueue(Dest);
    ADest.Clear;
    ADest.SetCapacity(Size + 1);
    I := FHead;
    while I <> FTail do
    begin
      ADest.Enqueue(FElements[I]);
      Inc(I);
      if I = FCapacity then
        I := 0;
    end;
  end;
end;

procedure TJclExtendedQueue.Clear;
var
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
     I := FHead;
     while I <> FTail do
     begin
       FreeExtended(FElements[I]);
       Inc(I);
       if I = FCapacity then
         I := 0;
     end;
     FHead := 0;
     FTail := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedQueue.Contains(const AValue: Extended): Boolean;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    I := FHead;
    while I <> FTail do
    begin
      if ItemsEqual(FElements[I], AValue) then
      begin
        Result := True;
        Break;
      end;
      Inc(I);
      if I = FCapacity then
        I := 0;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedQueue.Dequeue: Extended;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := 0.0;
    if FTail <> FHead then
    begin
      Result := FElements[FHead];
      FElements[FHead] := 0.0;
      Inc(FHead);
      if FHead = FCapacity then
        FHead := 0;
      AutoPack;
    end
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedQueue.Empty: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := FTail = FHead;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedQueue.Enqueue(const AValue: Extended): Boolean;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if (FTail = (FHead - 1)) or (FTail = (FHead + FCapacity - 1)) then
      AutoGrow;
    Result := (FTail <> (FHead - 1)) and (FTail <> (FHead + FCapacity - 1));
    if Result then
    begin
      FElements[FTail] := AValue;
      Inc(FTail);
      if FTail = FCapacity then
        FTail := 0;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclExtendedQueue.Pack;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    SetCapacity(Size + 1);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedQueue.Peek: Extended;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := 0.0;
    if FTail <> FHead then
      Result := FElements[FHead]
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclExtendedQueue.SetCapacity(Value: Integer);
var
  NewHead: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if Value < 1 then
      raise EJclIllegalQueueCapacityError.Create;
    if Value <= Size then
      raise EJclOutOfBoundsError.Create;

    if FHead > FTail then // looped
    begin
      NewHead := FHead + Value - FCapacity;
      if Value > FCapacity then
        // growing
        SetLength(FElements, Value);
      MoveArray(FElements, FHead, NewHead, FCapacity - FHead);
      if FCapacity > Value then
        // packing
        SetLength(FElements, Value);
      FHead := NewHead;
    end
    else
    begin
      // unlooped
      if Value < FCapacity then
      begin
        MoveArray(FElements, FHead, 0, FTail - FHead);
        Dec(FTail, FHead);
        FHead := 0;
      end;
      SetLength(FElements, Value);
    end;
    inherited SetCapacity(Value);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedQueue.Size: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    if FHead > FTail then
      Result := FCapacity - FHead + FTail  // looped
    else
      Result := FTail - FHead;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedQueue.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclExtendedQueue.Create(Size + 1);
  AssignPropertiesTo(Result);
end;

//=== { TJclIntegerQueue } =======================================================

constructor TJclIntegerQueue.Create(ACapacity: Integer);
begin
  inherited Create();
  FHead := 0;
  FTail := 0;
  SetCapacity(ACapacity);
end;

destructor TJclIntegerQueue.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclIntegerQueue.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclIntegerQueue;
  I: Integer;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclIntegerQueue then
  begin
    ADest := TJclIntegerQueue(Dest);
    ADest.Clear;
    ADest.SetCapacity(Size + 1);
    I := FHead;
    while I <> FTail do
    begin
      ADest.Enqueue(FElements[I]);
      Inc(I);
      if I = FCapacity then
        I := 0;
    end;
  end;
end;

procedure TJclIntegerQueue.Clear;
var
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
     I := FHead;
     while I <> FTail do
     begin
       FreeInteger(FElements[I]);
       Inc(I);
       if I = FCapacity then
         I := 0;
     end;
     FHead := 0;
     FTail := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerQueue.Contains(AValue: Integer): Boolean;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    I := FHead;
    while I <> FTail do
    begin
      if ItemsEqual(FElements[I], AValue) then
      begin
        Result := True;
        Break;
      end;
      Inc(I);
      if I = FCapacity then
        I := 0;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerQueue.Dequeue: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    if FTail <> FHead then
    begin
      Result := FElements[FHead];
      FElements[FHead] := 0;
      Inc(FHead);
      if FHead = FCapacity then
        FHead := 0;
      AutoPack;
    end
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerQueue.Empty: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := FTail = FHead;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerQueue.Enqueue(AValue: Integer): Boolean;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if (FTail = (FHead - 1)) or (FTail = (FHead + FCapacity - 1)) then
      AutoGrow;
    Result := (FTail <> (FHead - 1)) and (FTail <> (FHead + FCapacity - 1));
    if Result then
    begin
      FElements[FTail] := AValue;
      Inc(FTail);
      if FTail = FCapacity then
        FTail := 0;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntegerQueue.Pack;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    SetCapacity(Size + 1);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerQueue.Peek: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    if FTail <> FHead then
      Result := FElements[FHead]
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntegerQueue.SetCapacity(Value: Integer);
var
  NewHead: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if Value < 1 then
      raise EJclIllegalQueueCapacityError.Create;
    if Value <= Size then
      raise EJclOutOfBoundsError.Create;

    if FHead > FTail then // looped
    begin
      NewHead := FHead + Value - FCapacity;
      if Value > FCapacity then
        // growing
        SetLength(FElements, Value);
      MoveArray(FElements, FHead, NewHead, FCapacity - FHead);
      if FCapacity > Value then
        // packing
        SetLength(FElements, Value);
      FHead := NewHead;
    end
    else
    begin
      // unlooped
      if Value < FCapacity then
      begin
        MoveArray(FElements, FHead, 0, FTail - FHead);
        Dec(FTail, FHead);
        FHead := 0;
      end;
      SetLength(FElements, Value);
    end;
    inherited SetCapacity(Value);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerQueue.Size: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    if FHead > FTail then
      Result := FCapacity - FHead + FTail  // looped
    else
      Result := FTail - FHead;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerQueue.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntegerQueue.Create(Size + 1);
  AssignPropertiesTo(Result);
end;

//=== { TJclCardinalQueue } =======================================================

constructor TJclCardinalQueue.Create(ACapacity: Integer);
begin
  inherited Create();
  FHead := 0;
  FTail := 0;
  SetCapacity(ACapacity);
end;

destructor TJclCardinalQueue.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclCardinalQueue.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclCardinalQueue;
  I: Integer;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclCardinalQueue then
  begin
    ADest := TJclCardinalQueue(Dest);
    ADest.Clear;
    ADest.SetCapacity(Size + 1);
    I := FHead;
    while I <> FTail do
    begin
      ADest.Enqueue(FElements[I]);
      Inc(I);
      if I = FCapacity then
        I := 0;
    end;
  end;
end;

procedure TJclCardinalQueue.Clear;
var
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
     I := FHead;
     while I <> FTail do
     begin
       FreeCardinal(FElements[I]);
       Inc(I);
       if I = FCapacity then
         I := 0;
     end;
     FHead := 0;
     FTail := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalQueue.Contains(AValue: Cardinal): Boolean;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    I := FHead;
    while I <> FTail do
    begin
      if ItemsEqual(FElements[I], AValue) then
      begin
        Result := True;
        Break;
      end;
      Inc(I);
      if I = FCapacity then
        I := 0;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalQueue.Dequeue: Cardinal;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    if FTail <> FHead then
    begin
      Result := FElements[FHead];
      FElements[FHead] := 0;
      Inc(FHead);
      if FHead = FCapacity then
        FHead := 0;
      AutoPack;
    end
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalQueue.Empty: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := FTail = FHead;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalQueue.Enqueue(AValue: Cardinal): Boolean;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if (FTail = (FHead - 1)) or (FTail = (FHead + FCapacity - 1)) then
      AutoGrow;
    Result := (FTail <> (FHead - 1)) and (FTail <> (FHead + FCapacity - 1));
    if Result then
    begin
      FElements[FTail] := AValue;
      Inc(FTail);
      if FTail = FCapacity then
        FTail := 0;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclCardinalQueue.Pack;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    SetCapacity(Size + 1);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalQueue.Peek: Cardinal;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    if FTail <> FHead then
      Result := FElements[FHead]
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclCardinalQueue.SetCapacity(Value: Integer);
var
  NewHead: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if Value < 1 then
      raise EJclIllegalQueueCapacityError.Create;
    if Value <= Size then
      raise EJclOutOfBoundsError.Create;

    if FHead > FTail then // looped
    begin
      NewHead := FHead + Value - FCapacity;
      if Value > FCapacity then
        // growing
        SetLength(FElements, Value);
      MoveArray(FElements, FHead, NewHead, FCapacity - FHead);
      if FCapacity > Value then
        // packing
        SetLength(FElements, Value);
      FHead := NewHead;
    end
    else
    begin
      // unlooped
      if Value < FCapacity then
      begin
        MoveArray(FElements, FHead, 0, FTail - FHead);
        Dec(FTail, FHead);
        FHead := 0;
      end;
      SetLength(FElements, Value);
    end;
    inherited SetCapacity(Value);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalQueue.Size: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    if FHead > FTail then
      Result := FCapacity - FHead + FTail  // looped
    else
      Result := FTail - FHead;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalQueue.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclCardinalQueue.Create(Size + 1);
  AssignPropertiesTo(Result);
end;

//=== { TJclInt64Queue } =======================================================

constructor TJclInt64Queue.Create(ACapacity: Integer);
begin
  inherited Create();
  FHead := 0;
  FTail := 0;
  SetCapacity(ACapacity);
end;

destructor TJclInt64Queue.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclInt64Queue.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclInt64Queue;
  I: Integer;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclInt64Queue then
  begin
    ADest := TJclInt64Queue(Dest);
    ADest.Clear;
    ADest.SetCapacity(Size + 1);
    I := FHead;
    while I <> FTail do
    begin
      ADest.Enqueue(FElements[I]);
      Inc(I);
      if I = FCapacity then
        I := 0;
    end;
  end;
end;

procedure TJclInt64Queue.Clear;
var
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
     I := FHead;
     while I <> FTail do
     begin
       FreeInt64(FElements[I]);
       Inc(I);
       if I = FCapacity then
         I := 0;
     end;
     FHead := 0;
     FTail := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64Queue.Contains(const AValue: Int64): Boolean;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    I := FHead;
    while I <> FTail do
    begin
      if ItemsEqual(FElements[I], AValue) then
      begin
        Result := True;
        Break;
      end;
      Inc(I);
      if I = FCapacity then
        I := 0;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64Queue.Dequeue: Int64;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    if FTail <> FHead then
    begin
      Result := FElements[FHead];
      FElements[FHead] := 0;
      Inc(FHead);
      if FHead = FCapacity then
        FHead := 0;
      AutoPack;
    end
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64Queue.Empty: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := FTail = FHead;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64Queue.Enqueue(const AValue: Int64): Boolean;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if (FTail = (FHead - 1)) or (FTail = (FHead + FCapacity - 1)) then
      AutoGrow;
    Result := (FTail <> (FHead - 1)) and (FTail <> (FHead + FCapacity - 1));
    if Result then
    begin
      FElements[FTail] := AValue;
      Inc(FTail);
      if FTail = FCapacity then
        FTail := 0;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclInt64Queue.Pack;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    SetCapacity(Size + 1);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64Queue.Peek: Int64;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    if FTail <> FHead then
      Result := FElements[FHead]
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclInt64Queue.SetCapacity(Value: Integer);
var
  NewHead: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if Value < 1 then
      raise EJclIllegalQueueCapacityError.Create;
    if Value <= Size then
      raise EJclOutOfBoundsError.Create;

    if FHead > FTail then // looped
    begin
      NewHead := FHead + Value - FCapacity;
      if Value > FCapacity then
        // growing
        SetLength(FElements, Value);
      MoveArray(FElements, FHead, NewHead, FCapacity - FHead);
      if FCapacity > Value then
        // packing
        SetLength(FElements, Value);
      FHead := NewHead;
    end
    else
    begin
      // unlooped
      if Value < FCapacity then
      begin
        MoveArray(FElements, FHead, 0, FTail - FHead);
        Dec(FTail, FHead);
        FHead := 0;
      end;
      SetLength(FElements, Value);
    end;
    inherited SetCapacity(Value);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64Queue.Size: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    if FHead > FTail then
      Result := FCapacity - FHead + FTail  // looped
    else
      Result := FTail - FHead;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64Queue.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclInt64Queue.Create(Size + 1);
  AssignPropertiesTo(Result);
end;

//=== { TJclPtrQueue } =======================================================

constructor TJclPtrQueue.Create(ACapacity: Integer);
begin
  inherited Create();
  FHead := 0;
  FTail := 0;
  SetCapacity(ACapacity);
end;

destructor TJclPtrQueue.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclPtrQueue.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclPtrQueue;
  I: Integer;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclPtrQueue then
  begin
    ADest := TJclPtrQueue(Dest);
    ADest.Clear;
    ADest.SetCapacity(Size + 1);
    I := FHead;
    while I <> FTail do
    begin
      ADest.Enqueue(FElements[I]);
      Inc(I);
      if I = FCapacity then
        I := 0;
    end;
  end;
end;

procedure TJclPtrQueue.Clear;
var
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
     I := FHead;
     while I <> FTail do
     begin
       FreePointer(FElements[I]);
       Inc(I);
       if I = FCapacity then
         I := 0;
     end;
     FHead := 0;
     FTail := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrQueue.Contains(APtr: Pointer): Boolean;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    I := FHead;
    while I <> FTail do
    begin
      if ItemsEqual(FElements[I], APtr) then
      begin
        Result := True;
        Break;
      end;
      Inc(I);
      if I = FCapacity then
        I := 0;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrQueue.Dequeue: Pointer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if FTail <> FHead then
    begin
      Result := FElements[FHead];
      FElements[FHead] := nil;
      Inc(FHead);
      if FHead = FCapacity then
        FHead := 0;
      AutoPack;
    end
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrQueue.Empty: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := FTail = FHead;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrQueue.Enqueue(APtr: Pointer): Boolean;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if (FTail = (FHead - 1)) or (FTail = (FHead + FCapacity - 1)) then
      AutoGrow;
    Result := (FTail <> (FHead - 1)) and (FTail <> (FHead + FCapacity - 1));
    if Result then
    begin
      FElements[FTail] := APtr;
      Inc(FTail);
      if FTail = FCapacity then
        FTail := 0;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclPtrQueue.Pack;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    SetCapacity(Size + 1);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrQueue.Peek: Pointer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if FTail <> FHead then
      Result := FElements[FHead]
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclPtrQueue.SetCapacity(Value: Integer);
var
  NewHead: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if Value < 1 then
      raise EJclIllegalQueueCapacityError.Create;
    if Value <= Size then
      raise EJclOutOfBoundsError.Create;

    if FHead > FTail then // looped
    begin
      NewHead := FHead + Value - FCapacity;
      if Value > FCapacity then
        // growing
        SetLength(FElements, Value);
      MoveArray(FElements, FHead, NewHead, FCapacity - FHead);
      if FCapacity > Value then
        // packing
        SetLength(FElements, Value);
      FHead := NewHead;
    end
    else
    begin
      // unlooped
      if Value < FCapacity then
      begin
        MoveArray(FElements, FHead, 0, FTail - FHead);
        Dec(FTail, FHead);
        FHead := 0;
      end;
      SetLength(FElements, Value);
    end;
    inherited SetCapacity(Value);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrQueue.Size: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    if FHead > FTail then
      Result := FCapacity - FHead + FTail  // looped
    else
      Result := FTail - FHead;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrQueue.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclPtrQueue.Create(Size + 1);
  AssignPropertiesTo(Result);
end;

//=== { TJclQueue } =======================================================

constructor TJclQueue.Create(ACapacity: Integer; AOwnsObjects: Boolean);
begin
  inherited Create(AOwnsObjects);
  FHead := 0;
  FTail := 0;
  SetCapacity(ACapacity);
end;

destructor TJclQueue.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclQueue.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclQueue;
  I: Integer;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclQueue then
  begin
    ADest := TJclQueue(Dest);
    ADest.Clear;
    ADest.SetCapacity(Size + 1);
    I := FHead;
    while I <> FTail do
    begin
      ADest.Enqueue(FElements[I]);
      Inc(I);
      if I = FCapacity then
        I := 0;
    end;
  end;
end;

procedure TJclQueue.Clear;
var
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
     I := FHead;
     while I <> FTail do
     begin
       FreeObject(FElements[I]);
       Inc(I);
       if I = FCapacity then
         I := 0;
     end;
     FHead := 0;
     FTail := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclQueue.Contains(AObject: TObject): Boolean;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    I := FHead;
    while I <> FTail do
    begin
      if ItemsEqual(FElements[I], AObject) then
      begin
        Result := True;
        Break;
      end;
      Inc(I);
      if I = FCapacity then
        I := 0;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclQueue.Dequeue: TObject;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if FTail <> FHead then
    begin
      Result := FElements[FHead];
      FElements[FHead] := nil;
      Inc(FHead);
      if FHead = FCapacity then
        FHead := 0;
      AutoPack;
    end
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclQueue.Empty: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := FTail = FHead;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclQueue.Enqueue(AObject: TObject): Boolean;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if (FTail = (FHead - 1)) or (FTail = (FHead + FCapacity - 1)) then
      AutoGrow;
    Result := (FTail <> (FHead - 1)) and (FTail <> (FHead + FCapacity - 1));
    if Result then
    begin
      FElements[FTail] := AObject;
      Inc(FTail);
      if FTail = FCapacity then
        FTail := 0;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclQueue.Pack;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    SetCapacity(Size + 1);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclQueue.Peek: TObject;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if FTail <> FHead then
      Result := FElements[FHead]
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclQueue.SetCapacity(Value: Integer);
var
  NewHead: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if Value < 1 then
      raise EJclIllegalQueueCapacityError.Create;
    if Value <= Size then
      raise EJclOutOfBoundsError.Create;

    if FHead > FTail then // looped
    begin
      NewHead := FHead + Value - FCapacity;
      if Value > FCapacity then
        // growing
        SetLength(FElements, Value);
      MoveArray(FElements, FHead, NewHead, FCapacity - FHead);
      if FCapacity > Value then
        // packing
        SetLength(FElements, Value);
      FHead := NewHead;
    end
    else
    begin
      // unlooped
      if Value < FCapacity then
      begin
        MoveArray(FElements, FHead, 0, FTail - FHead);
        Dec(FTail, FHead);
        FHead := 0;
      end;
      SetLength(FElements, Value);
    end;
    inherited SetCapacity(Value);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclQueue.Size: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    if FHead > FTail then
      Result := FCapacity - FHead + FTail  // looped
    else
      Result := FTail - FHead;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclQueue.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclQueue.Create(Size + 1, False);
  AssignPropertiesTo(Result);
end;


{$IFDEF SUPPORTS_GENERICS}
//DOM-IGNORE-BEGIN

//=== { TJclQueue<T> } =======================================================

constructor TJclQueue<T>.Create(ACapacity: Integer; AOwnsItems: Boolean);
begin
  inherited Create(AOwnsItems);
  FHead := 0;
  FTail := 0;
  SetCapacity(ACapacity);
end;

destructor TJclQueue<T>.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclQueue<T>.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclQueue<T>;
  I: Integer;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclQueue<T> then
  begin
    ADest := TJclQueue<T>(Dest);
    ADest.Clear;
    ADest.SetCapacity(Size + 1);
    I := FHead;
    while I <> FTail do
    begin
      ADest.Enqueue(FElements[I]);
      Inc(I);
      if I = FCapacity then
        I := 0;
    end;
  end;
end;

procedure TJclQueue<T>.Clear;
var
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
     I := FHead;
     while I <> FTail do
     begin
       FreeItem(FElements[I]);
       Inc(I);
       if I = FCapacity then
         I := 0;
     end;
     FHead := 0;
     FTail := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclQueue<T>.Contains(const AItem: T): Boolean;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    I := FHead;
    while I <> FTail do
    begin
      if ItemsEqual(FElements[I], AItem) then
      begin
        Result := True;
        Break;
      end;
      Inc(I);
      if I = FCapacity then
        I := 0;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclQueue<T>.Dequeue: T;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := Default(T);
    if FTail <> FHead then
    begin
      Result := FElements[FHead];
      FElements[FHead] := Default(T);
      Inc(FHead);
      if FHead = FCapacity then
        FHead := 0;
      AutoPack;
    end
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclQueue<T>.Empty: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := FTail = FHead;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclQueue<T>.Enqueue(const AItem: T): Boolean;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if (FTail = (FHead - 1)) or (FTail = (FHead + FCapacity - 1)) then
      AutoGrow;
    Result := (FTail <> (FHead - 1)) and (FTail <> (FHead + FCapacity - 1));
    if Result then
    begin
      FElements[FTail] := AItem;
      Inc(FTail);
      if FTail = FCapacity then
        FTail := 0;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclQueue<T>.Pack;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    SetCapacity(Size + 1);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclQueue<T>.Peek: T;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := Default(T);
    if FTail <> FHead then
      Result := FElements[FHead]
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclQueue<T>.SetCapacity(Value: Integer);
var
  NewHead: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if Value < 1 then
      raise EJclIllegalQueueCapacityError.Create;
    if Value <= Size then
      raise EJclOutOfBoundsError.Create;

    if FHead > FTail then // looped
    begin
      NewHead := FHead + Value - FCapacity;
      if Value > FCapacity then
        // growing
        SetLength(FElements, Value);
      MoveArray(FElements, FHead, NewHead, FCapacity - FHead);
      if FCapacity > Value then
        // packing
        SetLength(FElements, Value);
      FHead := NewHead;
    end
    else
    begin
      // unlooped
      if Value < FCapacity then
      begin
        MoveArray(FElements, FHead, 0, FTail - FHead);
        Dec(FTail, FHead);
        FHead := 0;
      end;
      SetLength(FElements, Value);
    end;
    inherited SetCapacity(Value);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclQueue<T>.Size: Integer;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    if FHead > FTail then
      Result := FCapacity - FHead + FTail  // looped
    else
      Result := FTail - FHead;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclQueue<T>.MoveArray(var List: TDynArray; FromIndex, ToIndex, Count: SizeInt);
var
  I: SizeInt;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      List[ToIndex + I] := List[FromIndex + I];

    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
        List[FromIndex + I] := Default(T)
    else
      // independant
      for I := 0 to Count - 1 do
        List[FromIndex + I] := Default(T);
  end
  else
  begin
    for I := 0 to Count - 1 do
      List[ToIndex + I] := List[FromIndex + I];

    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
        List[FromIndex + I] := Default(T)
    else
      // independant
      for I := 0 to Count - 1 do
        List[FromIndex + I] := Default(T);
  end; 
end;

//=== { TJclQueueE<T> } ======================================================

constructor TJclQueueE<T>.Create(const AEqualityComparer: IEqualityComparer<T>;
  ACapacity: Integer; AOwnsItems: Boolean);
begin
  inherited Create(ACapacity, AOwnsItems);
  FEqualityComparer := AEqualityComparer;
end;

procedure TJclQueueE<T>.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclQueueE<T> then
    TJclQueueE<T>(Dest).FEqualityComparer := FEqualityComparer;
end;

function TJclQueueE<T>.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclQueueE<T>.Create(EqualityComparer, Size + 1, False);
  AssignPropertiesTo(Result);
end;

function TJclQueueE<T>.ItemsEqual(const A, B: T): Boolean;
begin
  if EqualityComparer <> nil then
    Result := EqualityComparer.Equals(A, B)
  else
    Result := inherited ItemsEqual(A, B);
end;

//=== { TJclQueueF<T> } ======================================================

constructor TJclQueueF<T>.Create(AEqualityCompare: TEqualityCompare<T>;
  ACapacity: Integer; AOwnsItems: Boolean);
begin
  inherited Create(ACapacity, AOwnsItems);
  SetEqualityCompare(AEqualityCompare);
end;

function TJclQueueF<T>.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclQueueF<T>.Create(EqualityCompare, Size + 1, False);
  AssignPropertiesTo(Result);
end;

//=== { TJclQueueI<T> } ======================================================

function TJclQueueI<T>.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclQueueI<T>.Create(Size + 1, False);
  AssignPropertiesTo(Result);
end;

function TJclQueueI<T>.ItemsEqual(const A, B: T): Boolean;
begin
  if Assigned(FEqualityCompare) then
    Result := FEqualityCompare(A, B)
  else
  if Assigned(FCompare) then
    Result := FCompare(A, B) = 0
  else
    Result := A.Equals(B);
end;

//DOM-IGNORE-END
{$ENDIF SUPPORTS_GENERICS}

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
