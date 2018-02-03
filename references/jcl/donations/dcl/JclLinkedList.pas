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
{ The Original Code is LinkedList.pas.                                                             }
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

unit JclLinkedList;

{$I jcl.inc}

interface

uses
  Classes,
  JclBase, JclAbstractContainer, JclDCL_intf;

type
  PJclIntfLinkedListItem = ^TJclIntfLinkedListItem;
  TJclIntfLinkedListItem = record
    Obj: IInterface;
    Next: PJclIntfLinkedListItem;
  end;

  PJclStrLinkedListItem = ^TJclStrLinkedListItem;
  TJclStrLinkedListItem = record
    Str: string;
    Next: PJclStrLinkedListItem;
  end;

  PJclLinkedListItem = ^TJclLinkedListItem;
  TJclLinkedListItem = record
    Obj: TObject;
    Next: PJclLinkedListItem;
  end;

  TJclIntfLinkedList = class(TJclAbstractContainer, IIntfCollection, IIntfList, IIntfCloneable)
  private
    FStart: PJclIntfLinkedListItem;
    FEnd: PJclIntfLinkedListItem;
    FSize: Integer;
  protected
    procedure AddFirst(AInterface: IInterface);
    { IIntfCollection }
    function Add(AInterface: IInterface): Boolean; overload;
    function AddAll(ACollection: IIntfCollection): Boolean; overload;
    procedure Clear;
    function Contains(AInterface: IInterface): Boolean;
    function ContainsAll(ACollection: IIntfCollection): Boolean;
    function Equals(ACollection: IIntfCollection): Boolean;
    function First: IIntfIterator;
    function IsEmpty: Boolean;
    function Last: IIntfIterator;
    function Remove(AInterface: IInterface): Boolean; overload;
    function RemoveAll(ACollection: IIntfCollection): Boolean;
    function RetainAll(ACollection: IIntfCollection): Boolean;
    function Size: Integer;
    { IIntfList }
    procedure Insert(Index: Integer; AInterface: IInterface); overload;
    function InsertAll(Index: Integer; ACollection: IIntfCollection): Boolean; overload;
    function GetObject(Index: Integer): IInterface;
    function IndexOf(AInterface: IInterface): Integer;
    function LastIndexOf(AInterface: IInterface): Integer;
    function Remove(Index: Integer): IInterface; overload;
    procedure SetObject(Index: Integer; AInterface: IInterface);
    function SubList(First, Count: Integer): IIntfList;
    { IIntfCloneable }
    function Clone: IInterface;
  public
    constructor Create(ACollection: IIntfCollection = nil);
    destructor Destroy; override;
  end;

  TJclStrLinkedList = class(TJclAbstractContainer, IStrCollection, IStrList, ICloneable)
  private
    FStart: PJclStrLinkedListItem;
    FEnd: PJclStrLinkedListItem;
    FSize: Integer;
  protected
    procedure AddFirst(const AString: string);
    { IIntfCollection }
    function Add(const AString: string): Boolean; overload;
    function AddAll(ACollection: IStrCollection): Boolean; overload;
    procedure Clear;
    function Contains(const AString: string): Boolean;
    function ContainsAll(ACollection: IStrCollection): Boolean;
    function Equals(ACollection: IStrCollection): Boolean;
    function First: IStrIterator;
    function IsEmpty: Boolean;
    function Last: IStrIterator;
    function Remove(const AString: string): Boolean; overload;
    function RemoveAll(ACollection: IStrCollection): Boolean;
    function RetainAll(ACollection: IStrCollection): Boolean;
    function Size: Integer;
    //Daniele Teti 27/12/2004
    procedure LoadFromStrings(Strings: TStrings);
    procedure SaveToStrings(Strings: TStrings);
    procedure AppendToStrings(Strings: TStrings);
    procedure AppendFromStrings(Strings: TStrings);
    function GetAsStrings: TStrings;
    function GetAsDelimited(Separator: string = AnsiLineBreak): string;
    procedure AppendDelimited(AString: string; Separator: string = AnsiLineBreak);
    procedure LoadDelimited(AString: string; Separator: string = AnsiLineBreak);
    { IIntfList }
    procedure Insert(Index: Integer; const AString: string); overload;
    function InsertAll(Index: Integer; ACollection: IStrCollection): Boolean; overload;
    function GetString(Index: Integer): string;
    function IndexOf(const AString: string): Integer;
    function LastIndexOf(const AString: string): Integer;
    function Remove(Index: Integer): string; overload;
    procedure SetString(Index: Integer; const AString: string);
    function SubList(First, Count: Integer): IStrList;
    { ICloneable }
    function Clone: TObject;
  public
    constructor Create(ACollection: IStrCollection = nil);
    destructor Destroy; override;
  end;

  TJclLinkedList = class(TJclAbstractContainer, ICollection, IList, ICloneable)
  private
    FStart: PJclLinkedListItem;
    FEnd: PJclLinkedListItem;
    FSize: Integer;
    FOwnsObjects: Boolean;
  protected
    procedure AddFirst(AObject: TObject);
    procedure FreeObject(var AObject: TObject);
    { ICollection }
    function Add(AObject: TObject): Boolean; overload;
    function AddAll(ACollection: ICollection): Boolean; overload;
    procedure Clear;
    function Contains(AObject: TObject): Boolean;
    function ContainsAll(ACollection: ICollection): Boolean;
    function Equals(ACollection: ICollection): Boolean;
    function First: IIterator;
    function IsEmpty: Boolean;
    function Last: IIterator;
    function Remove(AObject: TObject): Boolean; overload;
    function RemoveAll(ACollection: ICollection): Boolean;
    function RetainAll(ACollection: ICollection): Boolean;
    function Size: Integer;
    { IList }
    procedure Insert(Index: Integer; AObject: TObject); overload;
    function InsertAll(Index: Integer; ACollection: ICollection): Boolean; overload;
    function GetObject(Index: Integer): TObject;
    function IndexOf(AObject: TObject): Integer;
    function LastIndexOf(AObject: TObject): Integer;
    function Remove(Index: Integer): TObject; overload;
    procedure SetObject(Index: Integer; AObject: TObject);
    function SubList(First, Count: Integer): IList;
    { ICloneable }
    function Clone: TObject;
  public
    constructor Create(ACollection: ICollection = nil; AOwnsObjects: Boolean = True);
    destructor Destroy; override;
    property OwnsObjects: Boolean read FOwnsObjects;
  end;

implementation

uses
  SysUtils,
  JclDCLUtil;

type
  TIntfItr = class(TJclAbstractContainer, IIntfIterator)
  private
    FCursor: PJclIntfLinkedListItem;
    FOwnList: TJclIntfLinkedList;
    FLastRet: PJclIntfLinkedListItem;
    FSize: Integer;
  protected
    { IIterator}
    procedure Add(AInterface: IInterface);
    function GetObject: IInterface;
    function HasNext: Boolean;
    function HasPrevious: Boolean;
    function Next: IInterface;
    function NextIndex: Integer;
    function Previous: IInterface;
    function PreviousIndex: Integer;
    procedure Remove;
    procedure SetObject(AInterface: IInterface);
  public
    constructor Create(OwnList: TJclIntfLinkedList; Start: PJclIntfLinkedListItem);
    destructor Destroy; override;
  end;

  TStrItr = class(TJclAbstractContainer, IStrIterator)
  private
    FCursor: PJclStrLinkedListItem;
    FOwnList: TJclStrLinkedList;
    FLastRet: PJclStrLinkedListItem;
    FSize: Integer;
  protected
    { IStrIterator}
    procedure Add(const AString: string);
    function GetString: string;
    function HasNext: Boolean;
    function HasPrevious: Boolean;
    function Next: string;
    function NextIndex: Integer;
    function Previous: string;
    function PreviousIndex: Integer;
    procedure Remove;
    procedure SetString(const AString: string);
  public
    constructor Create(OwnList: TJclStrLinkedList; Start: PJclStrLinkedListItem);
    destructor Destroy; override;
  end;

  TItr = class(TJclAbstractContainer, IIterator)
  private
    FCursor: PJclLinkedListItem;
    FOwnList: TJclLinkedList;
    FLastRet: PJclLinkedListItem;
    FSize: Integer;
  public
    { IIterator}
    procedure Add(AObject: TObject);
    function GetObject: TObject;
    function HasNext: Boolean;
    function HasPrevious: Boolean;
    function Next: TObject;
    function NextIndex: Integer;
    function Previous: TObject;
    function PreviousIndex: Integer;
    procedure Remove;
    procedure SetObject(AObject: TObject);
  public
    constructor Create(OwnList: TJclLinkedList; Start: PJclLinkedListItem);
    destructor Destroy; override;
  end;

//=== { TIntfItr } ===========================================================

constructor TIntfItr.Create(OwnList: TJclIntfLinkedList; Start: PJclIntfLinkedListItem);
begin
  inherited Create;
  FCursor := Start;
  FOwnList := OwnList;
  FOwnList._AddRef; // Add a ref because FOwnList is not an interface !
  FLastRet := nil;
  FSize := FOwnList.Size;
end;

destructor TIntfItr.Destroy;
begin
  FOwnList._Release;
  inherited Destroy;
end;

procedure TIntfItr.Add(AInterface: IInterface);
var
  NewItem: PJclIntfLinkedListItem;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  if AInterface = nil then
    Exit;
  New(NewItem);
  NewItem.Obj := AInterface;
  if FCursor = nil then
  begin
    FCursor := NewItem;
    NewItem.Next := nil;
  end
  else
  begin
    NewItem.Next := FCursor.Next;
    FCursor.Next := NewItem;
  end;
  Inc(FOwnList.FSize);
  Inc(FSize);
end;

function TIntfItr.GetObject: IInterface;
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := FCursor.Obj;
end;

function TIntfItr.HasNext: Boolean;
begin
  Result := FCursor <> nil;
end;

function TIntfItr.HasPrevious: Boolean;
begin
  // Unidirectional
  raise EDCLOperationNotSupportedError.Create(RsEOperationNotSupported);
end;

function TIntfItr.Next: IInterface;
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := FCursor.Obj;
  FLastRet := FCursor;
  FCursor := FCursor.Next;
end;

function TIntfItr.NextIndex: Integer;
begin
  // No index
  raise EDCLOperationNotSupportedError.Create(RsEOperationNotSupported);
end;

function TIntfItr.Previous: IInterface;
begin
  // Unidirectional
  raise EDCLOperationNotSupportedError.Create(RsEOperationNotSupported);
end;

function TIntfItr.PreviousIndex: Integer;
begin
  // No Index;
  raise EDCLOperationNotSupportedError.Create(RsEOperationNotSupported);
end;

procedure TIntfItr.Remove;
var
  Current: PJclIntfLinkedListItem;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  if FCursor = nil then
    Exit;
  Current := FCursor;
  FCursor := FCursor.Next;
  if FLastRet = nil then
    FOwnList.FStart := FCursor
  else
    FLastRet.Next := FCursor;
  Current.Next := nil;
  Current.Obj := nil;
  Dispose(Current);
  Dec(FOwnList.FSize);
  Dec(FSize);
end;

procedure TIntfItr.SetObject(AInterface: IInterface);
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  FCursor.Obj := AInterface;
end;

//=== { TStrItr } ============================================================

constructor TStrItr.Create(OwnList: TJclStrLinkedList; Start: PJclStrLinkedListItem);
begin
  inherited Create;
  FCursor := Start;
  FOwnList := OwnList;
  FOwnList._AddRef; // Add a ref because FOwnList is not an interface !
  FLastRet := nil;
  FSize := FOwnList.Size;
end;

destructor TStrItr.Destroy;
begin
  FOwnList._Release;
  inherited Destroy;
end;

procedure TStrItr.Add(const AString: string);
var
  NewItem: PJclStrLinkedListItem;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  if AString = '' then
    Exit;
  New(NewItem);
  NewItem.Str := AString;
  if FCursor = nil then
  begin
    FCursor := NewItem;
    NewItem.Next := nil;
  end
  else
  begin
    NewItem.Next := FCursor.Next;
    FCursor.Next := NewItem;
  end;
  Inc(FOwnList.FSize);
  Inc(FSize);
end;

function TStrItr.GetString: string;
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := FCursor.Str;
end;

function TStrItr.HasNext: Boolean;
begin
  Result := FCursor <> nil;
end;

function TStrItr.HasPrevious: Boolean;
begin
  // Unidirectional
  raise EDCLOperationNotSupportedError.Create(RsEOperationNotSupported);
end;

function TStrItr.Next: string;
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := FCursor.Str;
  FLastRet := FCursor;
  FCursor := FCursor.Next;
end;

function TStrItr.NextIndex: Integer;
begin
  // No index
  raise EDCLOperationNotSupportedError.Create(RsEOperationNotSupported);
end;

function TStrItr.Previous: string;
begin
  // Unidirectional
  raise EDCLOperationNotSupportedError.Create(RsEOperationNotSupported);
end;

function TStrItr.PreviousIndex: Integer;
begin
  // No index
  raise EDCLOperationNotSupportedError.Create(RsEOperationNotSupported);
end;

procedure TStrItr.Remove;
var
  Current: PJclStrLinkedListItem;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  if FCursor = nil then
    Exit;
  Current := FCursor;
  FCursor := FCursor.Next;
  if FLastRet = nil then
    FOwnList.FStart := FCursor
  else
    FLastRet.Next := FCursor;
  Current.Next := nil;
  Current.Str := '';
  Dispose(Current);
  Dec(FOwnList.FSize);
  Dec(FSize);
end;

procedure TStrItr.SetString(const AString: string);
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  FCursor.Str := AString;
end;

//=== { TItr } ===============================================================

constructor TItr.Create(OwnList: TJclLinkedList; Start: PJclLinkedListItem);
begin
  inherited Create;
  FCursor := Start;
  FOwnList := OwnList;
  FOwnList._AddRef; // Add a ref because FOwnList is not an interface !
  FLastRet := nil;
  FSize := FOwnList.Size;
end;

destructor TItr.Destroy;
begin
  FOwnList._Release;
  inherited Destroy;
end;

procedure TItr.Add(AObject: TObject);
var
  NewItem: PJclLinkedListItem;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  if AObject = nil then
    Exit;
  New(NewItem);
  NewItem.Obj := AObject;
  if FCursor = nil then
  begin
    FCursor := NewItem;
    NewItem.Next := nil;
  end
  else
  begin
    NewItem.Next := FCursor.Next;
    FCursor.Next := NewItem;
  end;
  Inc(FOwnList.FSize);
  Inc(FSize);
end;

function TItr.GetObject: TObject;
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := FCursor.Obj;
end;

function TItr.HasNext: Boolean;
begin
  Result := FCursor <> nil;
end;

function TItr.HasPrevious: Boolean;
begin
  // Unidirectional
  raise EDCLOperationNotSupportedError.Create(RsEOperationNotSupported);
end;

function TItr.Next: TObject;
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := FCursor.Obj;
  FLastRet := FCursor;
  FCursor := FCursor.Next;
end;

function TItr.NextIndex: Integer;
begin
  // No Index
  raise EDCLOperationNotSupportedError.Create(RsEOperationNotSupported);
end;

function TItr.Previous: TObject;
begin
  // Unidirectional
  raise EDCLOperationNotSupportedError.Create(RsEOperationNotSupported);
end;

function TItr.PreviousIndex: Integer;
begin
  // No Index
  raise EDCLOperationNotSupportedError.Create(RsEOperationNotSupported);
end;

procedure TItr.Remove;
var
  Current: PJclLinkedListItem;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  if FCursor = nil then
    Exit;
  Current := FCursor;
  FCursor := FCursor.Next;
  if FLastRet = nil then
    FOwnList.FStart := FCursor
  else
    FLastRet.Next := FCursor;
  Current.Next := nil;
  if FOwnList.FOwnsObjects then
    Current.Obj.Free;
  Current.Obj := nil;
  Dispose(Current);
  Dec(FOwnList.FSize);
  Dec(FSize);
end;

procedure TItr.SetObject(AObject: TObject);
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  FCursor.Obj := AObject;
end;

//=== { TJclIntfLinkedList } =================================================

constructor TJclIntfLinkedList.Create(ACollection: IIntfCollection = nil);
var
  It: IIntfIterator;
begin
  inherited Create;
  FStart := nil;
  FEnd := nil;
  FSize := 0;
  if ACollection <> nil then
  begin
    It := ACollection.First;
    while It.HasNext do
      Add(It.Next);
  end;
end;

destructor TJclIntfLinkedList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TJclIntfLinkedList.Insert(Index: Integer; AInterface: IInterface);
var
  I: Integer;
  Current: PJclIntfLinkedListItem;
  NewItem: PJclIntfLinkedListItem;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  if (Index < 0) or (Index > FSize) then
    raise EDCLOutOfBoundsError.Create(RsEOutOfBounds);
  if AInterface = nil then
    Exit;
  if FStart = nil then
  begin
    AddFirst(AInterface);
    Exit;
  end;
  New(NewItem);
  NewItem.Obj := AInterface;
  if Index = 0 then
  begin
    NewItem.Next := FStart;
    FStart := NewItem;
    Inc(FSize);
  end
  else
  begin
    Current := FStart;
    I := 0;
    while (Current <> nil) and (I <> Index) do
      Current := Current.Next;
    NewItem.Next := Current.Next;
    Current.Next := NewItem;
    Inc(FSize);
  end;
end;

function TJclIntfLinkedList.Add(AInterface: IInterface): Boolean;
var
  NewItem: PJclIntfLinkedListItem;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := False;
  if AInterface = nil then
    Exit;
  Result := True;
  if FStart = nil then
  begin
    AddFirst(AInterface);
    Exit;
  end;
  New(NewItem);
  NewItem.Obj := AInterface;
  NewItem.Next := nil;
  FEnd.Next := NewItem;
  FEnd := NewItem;
  Inc(FSize);
end;

function TJclIntfLinkedList.AddAll(ACollection: IIntfCollection): Boolean;
var
  It: IIntfIterator;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := False;
  if ACollection = nil then
    Exit;
  It := ACollection.First;
  while It.HasNext do
    Result := Add(It.Next) or Result;
end;

function TJclIntfLinkedList.InsertAll(Index: Integer; ACollection: IIntfCollection): Boolean;
var
  I: Integer;
  It: IIntfIterator;
  Current: PJclIntfLinkedListItem;
  NewItem: PJclIntfLinkedListItem;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := False;
  if (Index < 0) or (Index > FSize) then
    raise EDCLOutOfBoundsError.Create(RsEOutOfBounds);
  if ACollection = nil then
    Exit;
  It := ACollection.First;
  // (rom) is this a bug? Only one element added.
  if (FStart = nil) and It.HasNext then
  begin
    AddFirst(It.Next);
    Exit;
  end;
  Current := FStart;
  I := 0;
  while (Current <> nil) and (I <> Index) do
    Current := Current.Next;
  while It.HasNext do
  begin
    New(NewItem);
    NewItem.Obj := It.Next;
    if Index = 0 then
    begin
      NewItem.Next := FStart;
      FStart := NewItem;
      Inc(FSize);
    end
    else
    begin
      NewItem.Next := Current.Next;
      Current.Next := NewItem;
      Inc(FSize);
    end;
    Inc(Index);
  end;
  Result := True;
end;

procedure TJclIntfLinkedList.AddFirst(AInterface: IInterface);
begin
  New(FStart);
  FStart.Obj := AInterface;
  FStart.Next := nil;
  FEnd := FStart;
  Inc(FSize);
end;

procedure TJclIntfLinkedList.Clear;
var
  I: Integer;
  Old, Current: PJclIntfLinkedListItem;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Current := FStart;
  for I := 0 to FSize - 1 do
  begin
    Current.Obj := nil;
    Old := Current;
    Current := Current.Next;
    Dispose(Old);
  end;
  FSize := 0;

  //Daniele Teti 27/12/2004
  FStart := nil;
  FEnd := nil;
end;

function TJclIntfLinkedList.Clone: IInterface;
var
  NewList: IIntfList;
begin
  NewList := TJclIntfLinkedList.Create;
  NewList.AddAll(Self);
  Result := NewList;
end;

function TJclIntfLinkedList.Contains(AInterface: IInterface): Boolean;
var
  I: Integer;
  Current: PJclIntfLinkedListItem;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := False;
  if AInterface = nil then
    Exit;
  Current := FStart;
  for I := 0 to FSize - 1 do
  begin
    if Current.Obj = AInterface then
    begin
      Result := True;
      Exit;
    end;
    Current := Current.Next;
  end;
end;

function TJclIntfLinkedList.ContainsAll(ACollection: IIntfCollection): Boolean;
var
  It: IIntfIterator;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := True;
  if ACollection = nil then
    Exit;
  It := ACollection.First;
  while It.HasNext do
    if not Contains(It.Next) then
    begin
      Result := False;
      Break;
    end;
end;

function TJclIntfLinkedList.Equals(ACollection: IIntfCollection): Boolean;
var
  It, ItSelf: IIntfIterator;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := False;
  if ACollection = nil then
    Exit;
  if FSize <> ACollection.Size then
    Exit;
  It := ACollection.First;
  ItSelf := First;
  while ItSelf.HasNext do
    if ItSelf.Next <> It.Next then
      Exit;
  Result := True;
end;

function TJclIntfLinkedList.GetObject(Index: Integer): IInterface;
var
  I: Integer;
  Current: PJclIntfLinkedListItem;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := nil;
  if FStart = nil then
    Exit;
  Current := FStart;
  for I := 0 to Index - 1 do
    Current := Current.Next;
  Result := Current.Obj;
end;

function TJclIntfLinkedList.IndexOf(AInterface: IInterface): Integer;
var
  I: Integer;
  Current: PJclIntfLinkedListItem;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := -1;
  if AInterface = nil then
    Exit;
  if FStart = nil then
    Exit;
  Current := FStart;
  for I := 0 to FSize - 1 do
  begin
    if Current.Obj = AInterface then
    begin
      Result := I;
      Break;
    end;
    Current := Current.Next;
  end;
end;

function TJclIntfLinkedList.First: IIntfIterator;
begin
  Result := TIntfItr.Create(Self, FStart);
end;

function TJclIntfLinkedList.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclIntfLinkedList.Last: IIntfIterator;
begin
  Result := TIntfItr.Create(Self, FStart);
end;

function TJclIntfLinkedList.LastIndexOf(AInterface: IInterface): Integer;
var
  I: Integer;
  Current: PJclIntfLinkedListItem;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := -1;
  if AInterface = nil then
    Exit;
  if FStart = nil then
    Exit;
  Current := FStart;
  for I := 0 to FSize - 1 do
  begin
    if Current.Obj = AInterface then
      Result := I;
    Current := Current.Next;
  end;
end;

function TJclIntfLinkedList.Remove(AInterface: IInterface): Boolean;
var
  I: Integer;
  Old, Current: PJclIntfLinkedListItem;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := False;
  if AInterface = nil then
    Exit;
  if FStart = nil then
    Exit;
  Old := nil;
  Current := FStart;
  for I := 0 to FSize - 1 do
  begin
    if Current.Obj = AInterface then
    begin
      Current.Obj := nil;
      if Old <> nil then
      begin
        Old.Next := Current.Next;
        if Old.Next = nil then
          FEnd := Old;
      end
      else
        FStart := Current.Next;
      Dispose(Current);
      Dec(FSize);
      Result := True;
      Exit;
    end;
    Old := Current;
    Current := Current.Next;
  end;
end;

function TJclIntfLinkedList.Remove(Index: Integer): IInterface;
var
  I: Integer;
  Old, Current: PJclIntfLinkedListItem;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := nil;
  if FStart = nil then
    Exit;
  Old := nil;
  Current := FStart;
  for I := 0 to Index - 1 do
  begin
    Old := Current;
    Current := Current.Next;
  end;
  Current.Obj := nil;
  if Old <> nil then
  begin
    Old.Next := Current.Next;
    if Old.Next = nil then
      FEnd := Old;
  end
  else
    FStart := Current.Next;
  Dispose(Current);
  Dec(FSize);
end;

function TJclIntfLinkedList.RemoveAll(ACollection: IIntfCollection): Boolean;
var
  It: IIntfIterator;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := True;
  if ACollection = nil then
    Exit;
  It := ACollection.First;
  while It.HasNext do
    Result := Remove(It.Next) and Result;
end;

function TJclIntfLinkedList.RetainAll(ACollection: IIntfCollection): Boolean;
var
  It: IIntfIterator;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := False;
  if ACollection = nil then
    Exit;
  It := First;
  while It.HasNext do
    if not ACollection.Contains(It.Next) then
      It.Remove;
end;

procedure TJclIntfLinkedList.SetObject(Index: Integer; AInterface: IInterface);
var
  I: Integer;
  Current: PJclIntfLinkedListItem;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  if FStart = nil then
    Exit;
  Current := FStart;
  for I := 0 to Index - 1 do
    Current := Current.Next;
  Current.Obj := AInterface;
end;

function TJclIntfLinkedList.Size: Integer;
begin
  Result := FSize;
end;

function TJclIntfLinkedList.SubList(First, Count: Integer): IIntfList;
var
  I: Integer;
  It: IIntfIterator;
  Last: Integer;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Last := First + Count - 1;
  if Last > FSize then
    Last := FSize - 1;
  Result := TJclIntfLinkedList.Create;
  I := 0;
  It := Self.First;
  while (I < First) and It.HasNext do
  begin
    It.Next;
    Inc(I);
  end;
  //I := 0;
  while (I <= Last) and It.HasNext do
  begin
    Result.Add(It.Next);
    Inc(I);
  end;
end;

//=== { TJclStrLinkedList } ==================================================

constructor TJclStrLinkedList.Create(ACollection: IStrCollection = nil);
var
  It: IStrIterator;
begin
  inherited Create;
  FStart := nil;
  FEnd := nil;
  FSize := 0;
  if ACollection <> nil then
  begin
    It := ACollection.First;
    while It.HasNext do
      Add(It.Next);
  end;
end;

destructor TJclStrLinkedList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TJclStrLinkedList.Insert(Index: Integer; const AString: string);
var
  I: Integer;
  Current: PJclStrLinkedListItem;
  NewItem: PJclStrLinkedListItem;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  if (Index < 0) or (Index > FSize) then
    raise EDCLOutOfBoundsError.Create(RsEOutOfBounds);
  if AString = '' then
    Exit;
  if FStart = nil then
  begin
    AddFirst(AString);
    Exit;
  end;
  New(NewItem);
  NewItem.Str := AString;
  if Index = 0 then
  begin
    NewItem.Next := FStart;
    FStart := NewItem;
    Inc(FSize);
  end
  else
  begin
    Current := FStart;
    I := 0;
    while (Current <> nil) and (I <> Index) do
      Current := Current.Next;
    NewItem.Next := Current.Next;
    Current.Next := NewItem;
    Inc(FSize);
  end;
end;

function TJclStrLinkedList.Add(const AString: string): Boolean;
var
  NewItem: PJclStrLinkedListItem;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := False;
  if AString = '' then
    Exit;
  Result := True;
  if FStart = nil then
  begin
    AddFirst(AString);
    Exit;
  end;
  New(NewItem);
  NewItem.Str := AString;
  NewItem.Next := nil;
  FEnd.Next := NewItem;
  FEnd := NewItem;
  Inc(FSize);
end;

function TJclStrLinkedList.AddAll(ACollection: IStrCollection): Boolean;
var
  It: IStrIterator;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := False;
  if ACollection = nil then
    Exit;
  It := ACollection.First;
  while It.HasNext do
    Result := Add(It.Next) or Result;
end;

function TJclStrLinkedList.InsertAll(Index: Integer; ACollection: IStrCollection): Boolean;
var
  I: Integer;
  It: IStrIterator;
  Current: PJclStrLinkedListItem;
  NewItem: PJclStrLinkedListItem;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := False;
  if ACollection = nil then
    Exit;
  It := ACollection.First;
  // (rom) is this a bug? Only one element added.
  if (FStart = nil) and It.HasNext then
  begin
    AddFirst(It.Next);
    Exit;
  end;
  Current := FStart;
  I := 0;
  while (Current <> nil) and (I <> Index) do
    Current := Current.Next;
  while It.HasNext do
  begin
    New(NewItem);
    NewItem.Str := It.Next;
    if Index = 0 then
    begin
      NewItem.Next := FStart;
      FStart := NewItem;
      Inc(FSize);
    end
    else
    begin
      NewItem.Next := Current.Next;
      Current.Next := NewItem;
      Inc(FSize);
    end;
    Inc(Index);
  end;
  Result := True;
end;

procedure TJclStrLinkedList.AddFirst(const AString: string);
begin
  New(FStart);
  FStart.Str := AString;
  FStart.Next := nil;
  FEnd := FStart;
  Inc(FSize);
end;

procedure TJclStrLinkedList.Clear;
var
  I: Integer;
  Old, Current: PJclStrLinkedListItem;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Current := FStart;
  for I := 0 to FSize - 1 do
  begin
    Current.Str := '';
    Old := Current;
    Current := Current.Next;
    Dispose(Old);
  end;
  FSize := 0;

  //Daniele Teti 27/12/2004
  FStart := nil;
  FEnd := nil;
end;

function TJclStrLinkedList.Clone: TObject;
var
  NewList: TJclStrLinkedList;
begin
  NewList := TJclStrLinkedList.Create;
  NewList.AddAll(Self);
  Result := NewList;
end;

function TJclStrLinkedList.Contains(const AString: string): Boolean;
var
  I: Integer;
  Current: PJclStrLinkedListItem;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := False;
  if AString = '' then
    Exit;
  Current := FStart;
  for I := 0 to FSize - 1 do
  begin
    if Current.Str = AString then
    begin
      Result := True;
      Exit;
    end;
    Current := Current.Next;
  end;
end;

function TJclStrLinkedList.ContainsAll(ACollection: IStrCollection): Boolean;
var
  It: IStrIterator;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := True;
  if ACollection = nil then
    Exit;
  It := ACollection.First;
  while It.HasNext do
    if not Contains(It.Next) then
    begin
      Result := False;
      Break;
    end;
end;

function TJclStrLinkedList.Equals(ACollection: IStrCollection): Boolean;
var
  It, ItSelf: IStrIterator;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := False;
  if ACollection = nil then
    Exit;
  if FSize <> ACollection.Size then
    Exit;
  It := ACollection.First;
  ItSelf := First;
  while ItSelf.HasNext do
    if ItSelf.Next <> It.Next then
      Exit;
  Result := True;
end;

function TJclStrLinkedList.First: IStrIterator;
begin
  Result := TStrItr.Create(Self, FStart);
end;

function TJclStrLinkedList.GetString(Index: Integer): string;
var
  I: Integer;
  Current: PJclStrLinkedListItem;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := '';
  if FStart = nil then
    Exit;
  Current := FStart;
  for I := 0 to Index - 1 do
    Current := Current.Next;
  Result := Current.Str;
end;

function TJclStrLinkedList.IndexOf(const AString: string): Integer;
var
  I: Integer;
  Current: PJclStrLinkedListItem;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := -1;
  if AString = '' then
    Exit;
  if FStart = nil then
    Exit;
  Current := FStart;
  for I := 0 to FSize - 1 do
  begin
    if Current.Str = AString then
    begin
      Result := I;
      Break;
    end;
    Current := Current.Next;
  end;
end;

function TJclStrLinkedList.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclStrLinkedList.Last: IStrIterator;
begin
  Result := TStrItr.Create(Self, FStart);
end;

function TJclStrLinkedList.LastIndexOf(const AString: string): Integer;
var
  I: Integer;
  Current: PJclStrLinkedListItem;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := -1;
  if AString = '' then
    Exit;
  if FStart = nil then
    Exit;
  Current := FStart;
  for I := 0 to FSize - 1 do
  begin
    if Current.Str = AString then
      Result := I;
    Current := Current.Next;
  end;
end;

function TJclStrLinkedList.Remove(Index: Integer): string;
var
  I: Integer;
  Old, Current: PJclStrLinkedListItem;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := '';
  if FStart = nil then
    Exit;
  Old := nil;
  Current := FStart;
  for I := 0 to Index - 1 do
  begin
    Old := Current;
    Current := Current.Next;
  end;
  Current.Str := '';
  if Old <> nil then
  begin
    Old.Next := Current.Next;
    if Old.Next = nil then
      FEnd := Old;
  end
  else
    FStart := Current.Next;
  Dispose(Current);
  Dec(FSize);
end;

function TJclStrLinkedList.Remove(const AString: string): Boolean;
var
  I: Integer;
  Old, Current: PJclStrLinkedListItem;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := False;
  if AString = '' then
    Exit;
  if FStart = nil then
    Exit;
  Old := nil;
  Current := FStart;
  for I := 0 to FSize - 1 do
  begin
    if Current.Str = AString then
    begin
      Current.Str := '';
      if Old <> nil then
      begin
        Old.Next := Current.Next;
        if Old.Next = nil then
          FEnd := Old;
      end
      else
        FStart := Current.Next;
      Dispose(Current);
      Dec(FSize);
      Result := True;
      Exit;
    end;
    Old := Current;
    Current := Current.Next;
  end;
end;

function TJclStrLinkedList.RemoveAll(ACollection: IStrCollection): Boolean;
var
  It: IStrIterator;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := True;
  if ACollection = nil then
    Exit;
  It := ACollection.First;
  while It.HasNext do
    Result := Remove(It.Next) and Result;
end;

function TJclStrLinkedList.RetainAll(ACollection: IStrCollection): Boolean;
var
  It: IStrIterator;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := False;
  if ACollection = nil then
    Exit;
  It := First;
  while It.HasNext do
    if not ACollection.Contains(It.Next) then
      It.Remove;
end;

procedure TJclStrLinkedList.SetString(Index: Integer; const AString: string);
var
  I: Integer;
  Current: PJclStrLinkedListItem;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  if FStart = nil then
    Exit;
  Current := FStart;
  for I := 0 to Index - 1 do
    Current := Current.Next;
  Current.Str := AString;
end;

function TJclStrLinkedList.Size: Integer;
begin
  Result := FSize;
end;

function TJclStrLinkedList.SubList(First, Count: Integer): IStrList;
var
  I: Integer;
  It: IStrIterator;
  Last: Integer;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Last := First + Count - 1;
  if Last > FSize then
    Last := FSize - 1;
  Result := TJclStrLinkedList.Create;
  I := 0;
  It := Self.First;
  while (I < First) and It.HasNext do
  begin
    It.Next;
    Inc(I);
  end;
  //I := 0;
  while (I <= Last) and It.HasNext do
  begin
    Result.Add(It.Next);
    Inc(I);
  end;
end;

//=== { TJclLinkedList } =====================================================

constructor TJclLinkedList.Create(ACollection: ICollection = nil; AOwnsObjects: Boolean = True);
var
  It: IIterator;
begin
  inherited Create;
  FStart := nil;
  FEnd := nil;
  FSize := 0;
  FOwnsObjects := AOwnsObjects;
  if ACollection <> nil then
  begin
    It := ACollection.First;
    while It.HasNext do
      Add(It.Next);
  end;
end;

destructor TJclLinkedList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TJclLinkedList.Insert(Index: Integer; AObject: TObject);
var
  I: Integer;
  Current: PJclLinkedListItem;
  NewItem: PJclLinkedListItem;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  if (Index < 0) or (Index > FSize) then
    raise EDCLOutOfBoundsError.Create(RsEOutOfBounds);
  if AObject = nil then
    Exit;
  if FStart = nil then
  begin
    AddFirst(AObject);
    Exit;
  end;
  New(NewItem);
  NewItem.Obj := AObject;
  if Index = 0 then
  begin
    NewItem.Next := FStart;
    FStart := NewItem;
    Inc(FSize);
  end
  else
  begin
    Current := FStart;
    for I := 0 to Index - 2 do
      Current := Current.Next;
    NewItem.Next := Current.Next;
    Current.Next := NewItem;
    Inc(FSize);
  end;
end;

function TJclLinkedList.Add(AObject: TObject): Boolean;
var
  NewItem: PJclLinkedListItem;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := False;
  if AObject = nil then
    Exit;
  Result := True;
  if FStart = nil then
  begin
    AddFirst(AObject);
    Exit;
  end;
  New(NewItem);
  NewItem.Obj := AObject;
  NewItem.Next := nil;
  FEnd.Next := NewItem;
  FEnd := NewItem;
  Inc(FSize);
end;

function TJclLinkedList.AddAll(ACollection: ICollection): Boolean;
var
  It: IIterator;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := False;
  if ACollection = nil then
    Exit;
  It := ACollection.First;
  while It.HasNext do
    Result := Add(It.Next) or Result;
  Result := True;
end;

function TJclLinkedList.InsertAll(Index: Integer; ACollection: ICollection): Boolean;
var
  I: Integer;
  It: IIterator;
  Current: PJclLinkedListItem;
  NewItem: PJclLinkedListItem;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := False;
  if (Index < 0) or (Index > FSize) then
    raise EDCLOutOfBoundsError.Create(RsEOutOfBounds);
  if ACollection = nil then
    Exit;
  It := ACollection.First;
  // (rom) is this a bug? Only one element added.
  if (FStart = nil) and It.HasNext then
  begin
    AddFirst(It.Next);
    Exit;
  end;
  Current := FStart;
  I := 0;
  while (Current <> nil) and (I <> Index) do
    Current := Current.Next;
  while It.HasNext do
  begin
    New(NewItem);
    NewItem.Obj := It.Next;
    if Index = 0 then
    begin
      NewItem.Next := FStart;
      FStart := NewItem;
      Inc(FSize);
    end
    else
    begin
      NewItem.Next := Current.Next;
      Current.Next := NewItem;
      Inc(FSize);
    end;
    Inc(Index);
  end;
  Result := True;
end;

procedure TJclLinkedList.AddFirst(AObject: TObject);
begin
  New(FStart);
  FStart.Obj := AObject;
  FStart.Next := nil;
  FEnd := FStart;
  Inc(FSize);
end;

procedure TJclLinkedList.Clear;
var
  I: Integer;
  Old, Current: PJclLinkedListItem;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Current := FStart;
  for I := 0 to FSize - 1 do
  begin
    Current.Obj := nil;
    Old := Current;
    Current := Current.Next;
    Dispose(Old);
  end;
  FSize := 0;

  //Daniele Teti 27/12/2004
  FStart := nil;
  FEnd := nil;
end;

function TJclLinkedList.Clone: TObject;
var
  NewList: TJclLinkedList;
begin
  NewList := TJclLinkedList.Create;
  NewList.AddAll(Self);
  Result := NewList;
end;

function TJclLinkedList.Contains(AObject: TObject): Boolean;
var
  I: Integer;
  Current: PJclLinkedListItem;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := False;
  if AObject = nil then
    Exit;
  Current := FStart;
  for I := 0 to FSize - 1 do
  begin
    if Current.Obj = AObject then
    begin
      Result := True;
      Break;
    end;
    Current := Current.Next;
  end;
end;

function TJclLinkedList.ContainsAll(ACollection: ICollection): Boolean;
var
  It: IIterator;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := True;
  if ACollection = nil then
    Exit;
  It := ACollection.First;
  while It.HasNext do
    if not Contains(It.Next) then
    begin
      Result := False;
      Break;
    end;
end;

function TJclLinkedList.Equals(ACollection: ICollection): Boolean;
var
  It, ItSelf: IIterator;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := False;
  if ACollection = nil then
    Exit;
  if FSize <> ACollection.Size then
    Exit;
  It := ACollection.First;
  ItSelf := First;
  while ItSelf.HasNext do
    if ItSelf.Next <> It.Next then
      Exit;
  Result := True;
end;

procedure TJclLinkedList.FreeObject(var AObject: TObject);
begin
  if FOwnsObjects then
  begin
    AObject.Free;
    AObject := nil;
  end;
end;

function TJclLinkedList.GetObject(Index: Integer): TObject;
var
  I: Integer;
  Current: PJclLinkedListItem;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := nil;
  if FStart = nil then
    Exit;
  Current := FStart;
  for I := 0 to Index - 1 do
    Current := Current.Next;
  Result := Current.Obj;
end;

function TJclLinkedList.IndexOf(AObject: TObject): Integer;
var
  I: Integer;
  Current: PJclLinkedListItem;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := -1;
  if AObject = nil then
    Exit;
  if FStart = nil then
    Exit;
  Current := FStart;
  for I := 0 to FSize - 1 do
  begin
    if Current.Obj = AObject then
    begin
      Result := I;
      Break;
    end;
    Current := Current.Next;
  end;
end;

function TJclLinkedList.First: IIterator;
begin
  Result := TItr.Create(Self, FStart);
end;

function TJclLinkedList.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclLinkedList.Last: IIterator;
begin
  Result := TItr.Create(Self, FStart);
end;

function TJclLinkedList.LastIndexOf(AObject: TObject): Integer;
var
  I: Integer;
  Current: PJclLinkedListItem;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := -1;
  if AObject = nil then
    Exit;
  if FStart = nil then
    Exit;
  Current := FStart;
  for I := 0 to FSize - 1 do
  begin
    if Current.Obj = AObject then
      Result := I;
    Current := Current.Next;
  end;
end;

function TJclLinkedList.Remove(AObject: TObject): Boolean;
var
  I: Integer;
  Old, Current: PJclLinkedListItem;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := False;
  if AObject = nil then
    Exit;
  if FStart = nil then
    Exit;
  Old := nil;
  Current := FStart;
  for I := 0 to FSize - 1 do
  begin
    if Current.Obj = AObject then
    begin
      FreeObject(Current.Obj);
      if Old <> nil then
      begin
        Old.Next := Current.Next;
        if Old.Next = nil then
          FEnd := Old;
      end
      else
        FStart := Current.Next;
      Dispose(Current);
      Dec(FSize);
      Result := True;
      Exit;
    end;
    Old := Current;
    Current := Current.Next;
  end;
end;

function TJclLinkedList.Remove(Index: Integer): TObject;
var
  I: Integer;
  Old, Current: PJclLinkedListItem;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := nil;
  if FStart = nil then
    Exit;
  Old := nil;
  Current := FStart;
  for I := 0 to Index - 1 do
  begin
    Old := Current;
    Current := Current.Next;
  end;
  FreeObject(Current.Obj);
  if Old <> nil then
  begin
    Old.Next := Current.Next;
    if Old.Next = nil then
      FEnd := Old;
  end
  else
    FStart := Current.Next;
  Dispose(Current);
  Dec(FSize);
end;

function TJclLinkedList.RemoveAll(ACollection: ICollection): Boolean;
var
  It: IIterator;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := True;
  if ACollection = nil then
    Exit;
  It := ACollection.First;
  while It.HasNext do
    Result := Remove(It.Next) and Result;
end;

function TJclLinkedList.RetainAll(ACollection: ICollection): Boolean;
var
  It: IIterator;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := False;
  if ACollection = nil then
    Exit;
  It := First;
  while It.HasNext do
    if not ACollection.Contains(It.Next) then
      It.Remove;
end;

procedure TJclLinkedList.SetObject(Index: Integer; AObject: TObject);
var
  I: Integer;
  Current: PJclLinkedListItem;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  if FStart = nil then
    Exit;
  Current := FStart;
  for I := 0 to Index - 1 do
    Current := Current.Next;
  Current.Obj := AObject;
end;

function TJclLinkedList.Size: Integer;
begin
  Result := FSize;
end;

function TJclLinkedList.SubList(First, Count: Integer): IList;
var
  I: Integer;
  It: IIterator;
  Last: Integer;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Last := First + Count - 1;
  if Last > FSize then
    Last := FSize - 1;
  Result := TJclLinkedList.Create;
  I := 0;
  It := Self.First;
  while (I < First) and It.HasNext do
  begin
    It.Next;
    Inc(I);
  end;
  while (I <= Last) and It.HasNext do
  begin
    Result.Add(It.Next);
    Inc(I);
  end;
end;

function TJclStrLinkedList.GetAsStrings: TStrings;
begin
  Result := TStringList.Create;
  try
    AppendToStrings(Result);
  except
    Result.Free;
    raise;
  end;
end;

procedure TJclStrLinkedList.LoadFromStrings(Strings: TStrings);
begin
  Clear;
  AppendFromStrings(Strings);
end;

procedure TJclStrLinkedList.AppendToStrings(Strings: TStrings);
var
  It: IStrIterator;
begin
  It := First;
  Strings.BeginUpdate;
  try
    while It.HasNext do
      Strings.Add(It.Next);
  finally
    Strings.EndUpdate;
  end;
end;

procedure TJclStrLinkedList.SaveToStrings(Strings: TStrings);
begin
  Strings.Clear;
  AppendToStrings(Strings);
end;

procedure TJclStrLinkedList.AppendFromStrings(Strings: TStrings);
var
  I: Integer;
begin
  for I := 0 to Strings.Count - 1 do
    Add(Strings[I]);
end;

function TJclStrLinkedList.GetAsDelimited(Separator: string): string;
var
  It: IStrIterator;
begin
  It := First;
  Result := '';
  if It.HasNext then
    Result := It.Next;
  while It.HasNext do
    Result := Result + Separator + It.Next;
end;

procedure TJclStrLinkedList.LoadDelimited(AString, Separator: string);
begin
  Clear;
  AppendDelimited(AString, Separator);
end;

procedure TJclStrLinkedList.AppendDelimited(AString, Separator: string);
begin
  DCLAppendDelimited(Self, AString, Separator);
end;

end.

