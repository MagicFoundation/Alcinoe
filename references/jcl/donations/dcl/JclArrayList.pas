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
{ The Original Code is ArrayList.pas.                                                              }
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

unit JclArrayList;

{$I jcl.inc}

interface

uses
  Classes,
  JclBase, JclAbstractContainer, JclDCL_intf, JclDCLUtil;

type
  TJclIntfArrayList = class(TJclAbstractContainer, IIntfCollection, IIntfList,
    IIntfArray, IIntfCloneable)
  private
    FElementData: TDynIInterfaceArray;
    FSize: Integer;
    FCapacity: Integer;
    procedure SetCapacity(ACapacity: Integer);
  protected
    procedure Grow; virtual;
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
    constructor Create(ACapacity: Integer = DCLDefaultCapacity); overload;
    constructor Create(ACollection: IIntfCollection); overload;
    destructor Destroy; override;
    property Capacity: Integer read FCapacity write SetCapacity;
  end;

  TJclStrArrayList = class(TJclAbstractContainer, IStrCollection, IStrList,
    IStrArray, ICloneable)
  private
    FCapacity: Integer;
    FElementData: TDynStringArray;
    FSize: Integer;
    procedure SetCapacity(ACapacity: Integer);
  protected
    procedure Grow; virtual;
    { IStrCollection }
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
    { IStrList }
    procedure Insert(Index: Integer; const AString: string); overload;
    function InsertAll(Index: Integer; ACollection: IStrCollection): Boolean; overload;
    function GetString(Index: Integer): string;
    function IndexOf(const AString: string): Integer;
    function LastIndexOf(const AString: string): Integer;
    function Remove(Index: Integer): string; overload;
    procedure SetString(Index: Integer; const AString: string);
    function SubList(First, Count: Integer): IStrList;
  public
    { ICloneable }
    function Clone: TObject;

    constructor Create(ACapacity: Integer = DCLDefaultCapacity); overload;
    constructor Create(ACollection: IStrCollection); overload;
    destructor Destroy; override;
    property Capacity: Integer read FCapacity write SetCapacity;
  end;

  TJclArrayList = class(TJclAbstractContainer, ICollection, IList, IArray, ICloneable)
  private
    FCapacity: Integer;
    FElementData: TDynObjectArray;
    FOwnsObjects: Boolean;
    FSize: Integer;
    procedure SetCapacity(ACapacity: Integer);
  protected
    procedure Grow; virtual;
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
    constructor Create(ACapacity: Integer = DCLDefaultCapacity; AOwnsObjects: Boolean = True); overload;
    constructor Create(ACollection: ICollection; AOwnsObjects: Boolean = True); overload;
    destructor Destroy; override;
    property Capacity: Integer read FCapacity write SetCapacity;
    property OwnsObjects: Boolean read FOwnsObjects;
  end;

implementation

uses
  SysUtils;

//=== { TIntfItr } ===========================================================

type
  TIntfItr = class(TJclAbstractContainer, IIntfIterator)
  private
    FCursor: Integer;
    FOwnList: TJclIntfArrayList;
    //FLastRet: Integer;
    FSize: Integer;
  protected
    { IIntfIterator}
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
    constructor Create(AOwnList: TJclIntfArrayList);
    destructor Destroy; override;
  end;

constructor TIntfItr.Create(AOwnList: TJclIntfArrayList);
begin
  inherited Create;
  FCursor := 0;
  FOwnList := AOwnList;
  FOwnList._AddRef; // Add a ref because FOwnList is not an interface !
  //FLastRet := -1;
  FSize := FOwnList.Size;
end;

destructor TIntfItr.Destroy;
begin
  FOwnList._Release;
  inherited Destroy;
end;

procedure TIntfItr.Add(AInterface: IInterface);
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  // inlined FOwnList.Add
  if FOwnList.FSize = FOwnList.Capacity then
    FOwnList.Grow;
  if FOwnList.FSize <> FCursor then
    System.Move(FOwnList.FElementData[FCursor], FOwnList.FElementData[FCursor + 1],
      (FOwnList.FSize - FCursor) * SizeOf(IInterface));
  // (rom) otherwise interface reference counting may crash
  FillChar(FOwnList.FElementData[FCursor], SizeOf(IInterface), 0);
  FOwnList.FElementData[FCursor] := AInterface;
  Inc(FOwnList.FSize);

  Inc(FSize);
  Inc(FCursor);
  //FLastRet := -1;
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
  Result := FOwnList.FElementData[FCursor];
end;

function TIntfItr.HasNext: Boolean;
begin
  Result := FCursor < FSize;
end;

function TIntfItr.HasPrevious: Boolean;
begin
  Result := FCursor > 0;
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
  Result := FOwnList.FElementData[FCursor];
  //FLastRet := FCursor;
  Inc(FCursor);
end;

function TIntfItr.NextIndex: Integer;
begin
  Result := FCursor;
end;

function TIntfItr.Previous: IInterface;
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Dec(FCursor);
  //FLastRet := FCursor;
  Result := FOwnList.FElementData[FCursor];
end;

function TIntfItr.PreviousIndex: Integer;
begin
  Result := FCursor - 1;
end;

procedure TIntfItr.Remove;
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  with FOwnList do
  begin
    FElementData[FCursor] := nil; // Force Release
    if FSize <> FCursor then
      System.Move(FElementData[FCursor + 1], FElementData[FCursor],
        (FSize - FCursor) * SizeOf(IInterface));
  end;
  Dec(FOwnList.FSize);
  Dec(FSize);
end;

procedure TIntfItr.SetObject(AInterface: IInterface);
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
  {
  if FLastRet = -1 then
    raise EDCLIllegalState.Create(SIllegalState);
  }
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  FOwnList.FElementData[FCursor] := AInterface;
end;

//=== { TStrItr } ============================================================

type
  TStrItr = class(TJclAbstractContainer, IStrIterator)
  private
    FCursor: Integer;
    FOwnList: TJclStrArrayList;
    //FLastRet: Integer;
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
    constructor Create(AOwnList: TJclStrArrayList);
    destructor Destroy; override;
  end;

constructor TStrItr.Create(AOwnList: TJclStrArrayList);
begin
  inherited Create;
  FCursor := 0;
  FOwnList := AOwnList;
  FOwnList._AddRef; // Add a ref because FOwnList is not an interface !
  //FLastRet := -1;
  FSize := FOwnList.Size;
end;

destructor TStrItr.Destroy;
begin
  FOwnList._Release;
  inherited Destroy;
end;

procedure TStrItr.Add(const AString: string);
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  // inlined FOwnList.Add
  if FOwnList.FSize = FOwnList.Capacity then
    FOwnList.Grow;
  if FOwnList.FSize <> FCursor then
    System.Move(FOwnList.FElementData[FCursor], FOwnList.FElementData[FCursor + 1],
      (FOwnList.FSize - FCursor) * SizeOf(string));
  // (rom) otherwise string reference counting may crash
  FillChar(FOwnList.FElementData[FCursor], SizeOf(string), 0);
  FOwnList.FElementData[FCursor] := AString;
  Inc(FOwnList.FSize);

  Inc(FSize);
  Inc(FCursor);
  //FLastRet := -1;
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
  Result := FOwnList.FElementData[FCursor];
end;

function TStrItr.HasNext: Boolean;
begin
  Result := FCursor < FSize;
end;

function TStrItr.HasPrevious: Boolean;
begin
  Result := FCursor > 0;
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
  Result := FOwnList.FElementData[FCursor];
  //FLastRet := FCursor;
  Inc(FCursor);
end;

function TStrItr.NextIndex: Integer;
begin
  Result := FCursor;
end;

function TStrItr.Previous: string;
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Dec(FCursor);
  //FLastRet := FCursor;
  Result := FOwnList.FElementData[FCursor];
end;

function TStrItr.PreviousIndex: Integer;
begin
  Result := FCursor - 1;
end;

procedure TStrItr.Remove;
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  with FOwnList do
  begin
    FElementData[FCursor] := ''; // Force Release
    if FSize <> FCursor then
      System.Move(FElementData[FCursor + 1], FElementData[FCursor],
        (FSize - FCursor) * SizeOf(string));
  end;
  Dec(FOwnList.FSize);
  Dec(FSize);
end;

procedure TStrItr.SetString(const AString: string);
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
  {
  if FLastRet = -1 then
    raise EDCLIllegalState.Create(SIllegalState);
  }
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  FOwnList.FElementData[FCursor] := AString;
end;

//=== { TItr } ===============================================================

type
  TItr = class(TJclAbstractContainer, IIterator)
  private
    FCursor: Integer;
    FOwnList: TJclArrayList;
    //FLastRet: Integer;
    FSize: Integer;
  protected
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
    constructor Create(AOwnList: TJclArrayList);
    destructor Destroy; override;
  end;

constructor TItr.Create(AOwnList: TJclArrayList);
begin
  inherited Create;
  FCursor := 0;
  FOwnList := AOwnList;
  FOwnList._AddRef; // Add a ref because FOwnList is not an interface !
  //FLastRet := -1;
  FSize := FOwnList.Size;
end;

destructor TItr.Destroy;
begin
  FOwnList._Release;
  inherited Destroy;
end;

procedure TItr.Add(AObject: TObject);
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  // inlined FOwnList.Add
  if FOwnList.FSize = FOwnList.Capacity then
    FOwnList.Grow;
  if FOwnList.FSize <> FCursor then
    System.Move(FOwnList.FElementData[FCursor], FOwnList.FElementData[FCursor + 1],
      (FOwnList.FSize - FCursor) * SizeOf(TObject));
  FOwnList.FElementData[FCursor] := AObject;
  Inc(FOwnList.FSize);

  Inc(FSize);
  Inc(FCursor);
  //FLastRet := -1;
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
  Result := FOwnList.FElementData[FCursor];
end;

function TItr.HasNext: Boolean;
begin
  Result := FCursor <> FSize;
end;

function TItr.HasPrevious: Boolean;
begin
  Result := FCursor > 0;
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
  Result := FOwnList.FElementData[FCursor];
  //FLastRet := FCursor;
  Inc(FCursor);
end;

function TItr.NextIndex: Integer;
begin
  Result := FCursor;
end;

function TItr.Previous: TObject;
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Dec(FCursor);
  //FLastRet := FCursor;
  Result := FOwnList.FElementData[FCursor];
end;

function TItr.PreviousIndex: Integer;
begin
  Result := FCursor - 1;
end;

procedure TItr.Remove;
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  with FOwnList do
  begin
    FreeObject(FElementData[FCursor]);
    if FSize <> FCursor then
      System.Move(FElementData[FCursor + 1], FElementData[FCursor],
        (FSize - FCursor) * SizeOf(TObject));
  end;
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
  FOwnList.FElementData[FCursor] := AObject;
end;

//=== { TJclIntfArrayList } ==================================================

constructor TJclIntfArrayList.Create(ACapacity: Integer = DCLDefaultCapacity);
begin
  inherited Create;
  FSize := 0;
  FCapacity := ACapacity;
  SetLength(FElementData, ACapacity);
end;

constructor TJclIntfArrayList.Create(ACollection: IIntfCollection);
begin
  // (rom) disabled because the following Create already calls inherited
  // inherited Create;
  if ACollection = nil then
    raise EDCLIllegalArgumentError.Create(RsENoCollection);
  Create(ACollection.Size);
  AddAll(ACollection);
end;

destructor TJclIntfArrayList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TJclIntfArrayList.Insert(Index: Integer; AInterface: IInterface);
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  if (Index < 0) or (Index > FSize) then
    raise EDCLOutOfBoundsError.Create(RsEOutOfBounds);
  if FSize = Capacity then
    Grow;
  if FSize <> Index then
    System.Move(FElementData[Index], FElementData[Index + 1],
      (FSize - Index) * SizeOf(IInterface));
  // (rom) otherwise interface reference counting may crash
  FillChar(FElementData[Index], SizeOf(IInterface), 0);
  FElementData[Index] := AInterface;
  Inc(FSize);
end;

function TJclIntfArrayList.InsertAll(Index: Integer; ACollection: IIntfCollection): Boolean;
var
  It: IIntfIterator;
  Size: Integer;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := False;
  if (Index < 0) or (Index >= FSize) then
    raise EDCLOutOfBoundsError.Create(RsEOutOfBounds);
  if ACollection = nil then
    Exit;
  Size := ACollection.Size;
  if FSize + Size >= Capacity then
    Capacity := FSize + Size;
  if Size <> 0 then
    System.Move(FElementData[Index], FElementData[Index + Size],
      Size * SizeOf(IInterface));
  // (rom) otherwise interface reference counting may crash
  FillChar(FElementData[Index], Size * SizeOf(IInterface), 0);
  It := ACollection.First;
  Result := It.HasNext;
  while It.HasNext do
  begin
    FElementData[Index] := It.Next;
    Inc(Index);
  end;
end;

function TJclIntfArrayList.Add(AInterface: IInterface): Boolean;
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  if FSize = Capacity then
    Grow;
  FillChar(FElementData[FSize], SizeOf(IInterface), 0);
  FElementData[FSize] := AInterface;
  Inc(FSize);
  Result := True;
end;

function TJclIntfArrayList.AddAll(ACollection: IIntfCollection): Boolean;
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
  begin
    // (rom) inlining Add() gives about 5 percent performance increase
    if FSize = Capacity then
      Grow;
    FillChar(FElementData[FSize], SizeOf(IInterface), 0);
    FElementData[FSize] := It.Next;
    Inc(FSize);
  end;
  Result := True;
end;

procedure TJclIntfArrayList.Clear;
var
  I: Integer;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  for I := 0 to FSize - 1 do
    FElementData[I] := nil;
  FSize := 0;
end;

function TJclIntfArrayList.Clone: IInterface;
var
  NewList: IIntfList;
begin
  NewList := TJclIntfArrayList.Create(Capacity);
  NewList.AddAll(Self);
  Result := NewList;
end;

function TJclIntfArrayList.Contains(AInterface: IInterface): Boolean;
var
  I: Integer;
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
  for I := 0 to FSize - 1 do
    if FElementData[I] = AInterface then
    begin
      Result := True;
      Break;
    end;
end;

function TJclIntfArrayList.ContainsAll(ACollection: IIntfCollection): Boolean;
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
  while Result and It.HasNext do
    Result := Contains(It.Next) and Result;
end;

function TJclIntfArrayList.Equals(ACollection: IIntfCollection): Boolean;
var
  I: Integer;
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
  if FSize <> ACollection.Size then
    Exit;
  It := ACollection.First;
  for I := 0 to FSize - 1 do
    if FElementData[I] <> It.Next then
      Exit;
  Result := True;
end;

function TJclIntfArrayList.GetObject(Index: Integer): IInterface;
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  if (Index < 0) or (Index >= FSize) then
    Result := nil
  else
    Result := FElementData[Index];
end;

procedure TJclIntfArrayList.SetCapacity(ACapacity: Integer);
begin
  if ACapacity >= FSize then
  begin
    SetLength(FElementData, ACapacity);
    FCapacity := ACapacity;
  end
  else
    raise EDCLOutOfBoundsError.Create(RsEOutOfBounds);
end;

procedure TJclIntfArrayList.Grow;
begin
  Capacity := Capacity + Capacity div 4;
end;

function TJclIntfArrayList.IndexOf(AInterface: IInterface): Integer;
var
  I: Integer;
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
  for I := 0 to FSize - 1 do
    if FElementData[I] = AInterface then
    begin
      Result := I;
      Break;
    end;
end;

function TJclIntfArrayList.First: IIntfIterator;
begin
  Result := TIntfItr.Create(Self);
end;

function TJclIntfArrayList.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclIntfArrayList.Last: IIntfIterator;
var
  NewIterator: TIntfItr;
begin
  NewIterator := TIntfItr.Create(Self);
  NewIterator.FCursor := NewIterator.FOwnList.FSize;
  NewIterator.FSize := NewIterator.FOwnList.FSize;
  Result := NewIterator;
end;

function TJclIntfArrayList.LastIndexOf(AInterface: IInterface): Integer;
var
  I: Integer;
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
  for I := FSize - 1 downto 0 do
    if FElementData[I] = AInterface then
    begin
      Result := I;
      Break;
    end;
end;

function TJclIntfArrayList.Remove(AInterface: IInterface): Boolean;
var
  I: Integer;
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
  for I := FSize - 1 downto 0 do
    if FElementData[I] = AInterface then // Removes all AInterface
    begin
      FElementData[I] := nil; // Force Release
      if FSize <> I then
        System.Move(FElementData[I + 1], FElementData[I],
          (FSize - I) * SizeOf(IInterface));
      Dec(FSize);
      Result := True;
    end;
end;

function TJclIntfArrayList.Remove(Index: Integer): IInterface;
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  if (Index < 0) or (Index >= FSize) then
    raise EDCLOutOfBoundsError.Create(RsEOutOfBounds);
  Result := FElementData[Index];
  FElementData[Index] := nil;
  if FSize <> Index then
    System.Move(FElementData[Index + 1], FElementData[Index],
      (FSize - Index) * SizeOf(IInterface));
  Dec(FSize);
end;

function TJclIntfArrayList.RemoveAll(ACollection: IIntfCollection): Boolean;
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

function TJclIntfArrayList.RetainAll(ACollection: IIntfCollection): Boolean;
var
  I: Integer;
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
  for I := FSize - 1 downto 0 do
    if not ACollection.Contains(FElementData[I]) then
      Remove(I);
end;

procedure TJclIntfArrayList.SetObject(Index: Integer; AInterface: IInterface);
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  if (Index < 0) or (Index >= FSize) then
    raise EDCLOutOfBoundsError.Create(RsEOutOfBounds);
  FElementData[Index] := AInterface;
end;

function TJclIntfArrayList.Size: Integer;
begin
  Result := FSize;
end;

function TJclIntfArrayList.SubList(First, Count: Integer): IIntfList;
var
  I: Integer;
  Last: Integer;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Last := First + Count - 1;
  if Last >= FSize then
    Last := FSize - 1;
  Result := TJclIntfArrayList.Create(Count);
  for I := First to Last do
    Result.Add(FElementData[I]);
end;

//=== { TJclStrArrayList } ===================================================

constructor TJclStrArrayList.Create(ACapacity: Integer = DCLDefaultCapacity);
begin
  inherited Create;
  FSize := 0;
  FCapacity := ACapacity;
  SetLength(FElementData, ACapacity);
end;

constructor TJclStrArrayList.Create(ACollection: IStrCollection);
begin
  // (rom) disabled because the following Create already calls inherited
  // inherited Create;
  if ACollection = nil then
    raise EDCLIllegalArgumentError.Create(RsENoCollection);
  Create(ACollection.Size);
  AddAll(ACollection);
end;

destructor TJclStrArrayList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TJclStrArrayList.Insert(Index: Integer; const AString: string);
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  if (Index < 0) or (Index > FSize) then
    raise EDCLOutOfBoundsError.Create(RsEOutOfBounds);
  if FSize = Capacity then
    Grow;
  if FSize <> Index then
    System.Move(FElementData[Index], FElementData[Index + 1],
      (FSize - Index) * SizeOf(string));
  // (rom) otherwise string reference counting would crash
  FillChar(FElementData[Index], SizeOf(string), 0);
  FElementData[Index] := AString;
  Inc(FSize);
end;

function TJclStrArrayList.InsertAll(Index: Integer; ACollection: IStrCollection): Boolean;
var
  It: IStrIterator;
  Size: Integer;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := False;
  if (Index < 0) or (Index >= FSize) then
    raise EDCLOutOfBoundsError.Create(RsEOutOfBounds);
  if ACollection = nil then
    Exit;
  Size := ACollection.Size;
  if FSize + Size >= Capacity then
    Capacity := FSize + Size;
  if Size <> 0 then
    System.Move(FElementData[Index], FElementData[Index + Size],
      Size * SizeOf(string));
  // (rom) otherwise string reference counting would crash
  FillChar(FElementData[Index], Size * SizeOf(string), 0);
  It := ACollection.First;
  Result := It.HasNext;
  while It.HasNext do
  begin
    FElementData[Index] := It.Next;
    Inc(Index);
  end;
end;

function TJclStrArrayList.Add(const AString: string): Boolean;
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  if FSize = Capacity then
    Grow;
  FillChar(FElementData[FSize], SizeOf(string), 0);
  FElementData[FSize] := AString;
  Inc(FSize);
  Result := True;
end;

function TJclStrArrayList.AddAll(ACollection: IStrCollection): Boolean;
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
  begin
    // (rom) inlining Add() gives about 5 percent performance increase
    // without THREADSAFE and about 30 percent with THREADSAFE
    if FSize = Capacity then
      Grow;
    FillChar(FElementData[FSize], SizeOf(string), 0);
    FElementData[FSize] := It.Next;
    Inc(FSize);
  end;
  Result := True;
end;

procedure TJclStrArrayList.Clear;
var
  I: Integer;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  for I := 0 to FSize - 1 do
    FElementData[I] := '';
  FSize := 0;
end;

function TJclStrArrayList.Clone: TObject;
var
  NewList: TJclStrArrayList;
begin
  NewList := TJclStrArrayList.Create(Capacity);
  NewList.AddAll(Self);
  Result := NewList;
end;

function TJclStrArrayList.Contains(const AString: string): Boolean;
var
  I: Integer;
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
  for I := 0 to FSize - 1 do
    if FElementData[I] = AString then
    begin
      Result := True;
      Break;
    end;
end;

function TJclStrArrayList.ContainsAll(ACollection: IStrCollection): Boolean;
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
  while Result and It.HasNext do
    Result := Contains(It.Next) and Result;
end;

function TJclStrArrayList.Equals(ACollection: IStrCollection): Boolean;
var
  I: Integer;
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
  if FSize <> ACollection.Size then
    Exit;
  It := ACollection.First;
  for I := 0 to FSize - 1 do
    if FElementData[I] <> It.Next then
      Exit;
  Result := True;
end;

function TJclStrArrayList.First: IStrIterator;
begin
  Result := TStrItr.Create(Self);
end;

function TJclStrArrayList.GetString(Index: Integer): string;
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  if (Index < 0) or (Index >= FSize) then
    Result := ''
  else
    Result := FElementData[Index];
end;

procedure TJclStrArrayList.SetCapacity(ACapacity: Integer);
begin
  if ACapacity >= FSize then
  begin
    SetLength(FElementData, ACapacity);
    FCapacity := ACapacity;
  end
  else
    raise EDCLOutOfBoundsError.Create(RsEOutOfBounds);
end;

procedure TJclStrArrayList.Grow;
begin
  Capacity := Capacity + Capacity div 4;
end;

function TJclStrArrayList.IndexOf(const AString: string): Integer;
var
  I: Integer;
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
  for I := 0 to FSize - 1 do
    if FElementData[I] = AString then
    begin
      Result := I;
      Break;
    end;
end;

function TJclStrArrayList.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclStrArrayList.Last: IStrIterator;
var
  NewIterator: TStrItr;
begin
  NewIterator := TStrItr.Create(Self);
  NewIterator.FCursor := NewIterator.FOwnList.FSize;
  NewIterator.FSize := NewIterator.FOwnList.FSize;
  Result := NewIterator;
end;

function TJclStrArrayList.LastIndexOf(const AString: string): Integer;
var
  I: Integer;
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
  for I := FSize - 1 downto 0 do
    if FElementData[I] = AString then
    begin
      Result := I;
      Break;
    end;
end;

function TJclStrArrayList.Remove(const AString: string): Boolean;
var
  I: Integer;
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
  for I := FSize - 1 downto 0 do
    if FElementData[I] = AString then // Removes all AString
    begin
      FElementData[I] := ''; // Force Release
      if FSize <> I then
        System.Move(FElementData[I + 1], FElementData[I],
          (FSize - I) * SizeOf(IInterface));
      Dec(FSize);
      Result := True;
    end;
end;

function TJclStrArrayList.Remove(Index: Integer): string;
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  if (Index < 0) or (Index >= FSize) then
    raise EDCLOutOfBoundsError.Create(RsEOutOfBounds);
  Result := FElementData[Index];
  FElementData[Index] := '';
  if FSize <> Index then
    System.Move(FElementData[Index + 1], FElementData[Index],
      (FSize - Index) * SizeOf(IInterface));
  Dec(FSize);
end;

function TJclStrArrayList.RemoveAll(ACollection: IStrCollection): Boolean;
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

function TJclStrArrayList.RetainAll(ACollection: IStrCollection): Boolean;
var
  I: Integer;
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
  for I := FSize - 1 downto 0 do
    if not ACollection.Contains(FElementData[I]) then
      Remove(I);
end;

procedure TJclStrArrayList.SetString(Index: Integer; const AString: string);
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  if (Index < 0) or (Index >= FSize) then
    raise EDCLOutOfBoundsError.Create(RsEOutOfBounds);
  FElementData[Index] := AString
end;

function TJclStrArrayList.Size: Integer;
begin
  Result := FSize;
end;

function TJclStrArrayList.SubList(First, Count: Integer): IStrList;
var
  I: Integer;
  Last: Integer;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Last := First + Count - 1;
  if Last >= FSize then
    Last := FSize - 1;
  Result := TJclStrArrayList.Create(Count);
  for I := First to Last do
    Result.Add(FElementData[I]);
end;

//=== { TJclArrayList } ======================================================

constructor TJclArrayList.Create(ACapacity: Integer = DCLDefaultCapacity;
  AOwnsObjects: Boolean = True);
begin
  inherited Create;
  FSize := 0;
  FCapacity := ACapacity;
  FOwnsObjects := AOwnsObjects;
  SetLength(FElementData, ACapacity);
end;

constructor TJclArrayList.Create(ACollection: ICollection; AOwnsObjects: Boolean = True);
begin
  // (rom) disabled because the following Create already calls inherited
  // inherited Create;
  if ACollection = nil then
    raise EDCLIllegalArgumentError.Create(RsENoCollection);
  Create(ACollection.Size, AOwnsObjects);
  AddAll(ACollection);
end;

destructor TJclArrayList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TJclArrayList.Insert(Index: Integer; AObject: TObject);
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  if (Index < 0) or (Index > FSize) then
    raise EDCLOutOfBoundsError.Create(RsEOutOfBounds);
  if FSize = Capacity then
    Grow;
  if FSize <> Index then
    System.Move(FElementData[Index], FElementData[Index + 1],
      (FSize - Index) * SizeOf(TObject));
  FElementData[Index] := AObject;
  Inc(FSize);
end;

function TJclArrayList.InsertAll(Index: Integer; ACollection: ICollection): Boolean;
var
  It: IIterator;
  Size: Integer;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := False;
  if (Index < 0) or (Index >= FSize) then
    raise EDCLOutOfBoundsError.Create(RsEOutOfBounds);
  if ACollection = nil then
    Exit;
  Size := ACollection.Size;
  if FSize + Size >= Capacity then
    Capacity := FSize + Size;
  if Size <> 0 then
    System.Move(FElementData[Index], FElementData[Index + Size],
      Size * SizeOf(IInterface));
  It := ACollection.First;
  Result := It.HasNext;
  while It.HasNext do
  begin
    FElementData[Index] := It.Next;
    Inc(Index);
  end;
end;

function TJclArrayList.Add(AObject: TObject): Boolean;
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  if FSize = Capacity then
    Grow;
  FElementData[FSize] := AObject;
  Inc(FSize);
  Result := True;
end;

function TJclArrayList.AddAll(ACollection: ICollection): Boolean;
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
  begin
    // (rom) inlining Add() gives about 5 percent performance increase
    if FSize = Capacity then
      Grow;
    FElementData[FSize] := It.Next;
    Inc(FSize);
  end;
  Result := True;
end;

procedure TJclArrayList.Clear;
var
  I: Integer;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  for I := 0 to FSize - 1 do
    FreeObject(FElementData[I]);
  FSize := 0;
end;

function TJclArrayList.Clone: TObject;
var
  NewList: TJclArrayList;
begin
  NewList := TJclArrayList.Create(Capacity, False); // Only one can have FOwnsObject = True
  NewList.AddAll(Self);
  Result := NewList;
end;

function TJclArrayList.Contains(AObject: TObject): Boolean;
var
  I: Integer;
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
  for I := 0 to FSize - 1 do
    if FElementData[I] = AObject then
    begin
      Result := True;
      Break;
    end;
end;

function TJclArrayList.ContainsAll(ACollection: ICollection): Boolean;
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
  while Result and It.HasNext do
    Result := Contains(It.Next) and Result;
end;

function TJclArrayList.Equals(ACollection: ICollection): Boolean;
var
  I: Integer;
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
  if FSize <> ACollection.Size then
    Exit;
  It := ACollection.First;
  for I := 0 to FSize - 1 do
    if FElementData[I] <> It.Next then
      Exit;
  Result := True;
end;

procedure TJclArrayList.FreeObject(var AObject: TObject);
begin
  if FOwnsObjects then
  begin
    AObject.Free;
    AObject := nil;
  end;
end;

function TJclArrayList.GetObject(Index: Integer): TObject;
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  if (Index < 0) or (Index >= FSize) then
    Result := nil
  else
    Result := FElementData[Index];
end;

procedure TJclArrayList.SetCapacity(ACapacity: Integer);
begin
  if ACapacity >= FSize then
  begin
    SetLength(FElementData, ACapacity);
    FCapacity := ACapacity;
  end
  else
    raise EDCLOutOfBoundsError.Create(RsEOutOfBounds);
end;

procedure TJclArrayList.Grow;
begin
  Capacity := Capacity + Capacity div 4;
end;

function TJclArrayList.IndexOf(AObject: TObject): Integer;
var
  I: Integer;
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
  for I := 0 to FSize - 1 do
    if FElementData[I] = AObject then
    begin
      Result := I;
      Break;
    end;
end;

function TJclArrayList.First: IIterator;
begin
  Result := TItr.Create(Self);
end;

function TJclArrayList.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclArrayList.Last: IIterator;
var
  NewIterator: TItr;
begin
  NewIterator := TItr.Create(Self);
  NewIterator.FCursor := NewIterator.FOwnList.FSize;
  NewIterator.FSize := NewIterator.FOwnList.FSize;
  Result := NewIterator;
end;

function TJclArrayList.LastIndexOf(AObject: TObject): Integer;
var
  I: Integer;
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
  for I := FSize - 1 downto 0 do
    if FElementData[I] = AObject then
    begin
      Result := I;
      Break;
    end;
end;

function TJclArrayList.Remove(AObject: TObject): Boolean;
var
  I: Integer;
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
  for I := FSize - 1 downto 0 do
    if FElementData[I] = AObject then // Removes all AObject
    begin
      FreeObject(FElementData[I]);
      if FSize <> I then
        System.Move(FElementData[I + 1], FElementData[I],
          (FSize - I) * SizeOf(TObject));
      Dec(FSize);
      Result := True;
    end;
end;

function TJclArrayList.Remove(Index: Integer): TObject;
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  if (Index < 0) or (Index >= FSize) then
    raise EDCLOutOfBoundsError.Create(RsEOutOfBounds);
  Result := nil;
  FreeObject(FElementData[Index]);
  if FSize <> Index then
    System.Move(FElementData[Index + 1], FElementData[Index],
      (FSize - Index) * SizeOf(TObject));
  Dec(FSize);
end;

function TJclArrayList.RemoveAll(ACollection: ICollection): Boolean;
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

function TJclArrayList.RetainAll(ACollection: ICollection): Boolean;
var
  I: Integer;
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
  for I := FSize - 1 to 0 do
    if not ACollection.Contains(FElementData[I]) then
      Remove(I);
end;

procedure TJclArrayList.SetObject(Index: Integer; AObject: TObject);
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  if (Index < 0) or (Index >= FSize) then
    raise EDCLOutOfBoundsError.Create(RsEOutOfBounds);
  FElementData[Index] := AObject;
end;

function TJclArrayList.Size: Integer;
begin
  Result := FSize;
end;

function TJclArrayList.SubList(First, Count: Integer): IList;
var
  I: Integer;
  Last: Integer;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Last := First + Count - 1;
  if Last >= FSize then
    Last := FSize - 1;
  Result := TJclArrayList.Create(Count, FOwnsObjects);
  for I := First to Last do
    Result.Add(FElementData[I]);
end;

function TJclStrArrayList.GetAsStrings: TStrings;
begin
  Result := TStringList.Create;
  try
    AppendToStrings(Result);
  except
    Result.Free;
    raise;
  end;
end;

procedure TJclStrArrayList.LoadFromStrings(Strings: TStrings);
begin
  Clear;
  AppendFromStrings(Strings);
end;

procedure TJclStrArrayList.AppendToStrings(Strings: TStrings);
var
  It: IStrIterator;
begin
  It := First;
  while It.HasNext do
    Strings.Add(It.Next);
end;

procedure TJclStrArrayList.SaveToStrings(Strings: TStrings);
begin
  Strings.Clear;
  AppendToStrings(Strings);
end;

procedure TJclStrArrayList.AppendFromStrings(Strings: TStrings);
var
  I: Integer;
begin
  for I := 0 to Strings.Count - 1 do
    Add(Strings[I]);
end;

function TJclStrArrayList.GetAsDelimited(Separator: string): string;
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

procedure TJclStrArrayList.AppendDelimited(AString, Separator: string);
begin
  DCLAppendDelimited(Self, AString, Separator);
end;

procedure TJclStrArrayList.LoadDelimited(AString, Separator: string);
begin
  Clear;
  AppendDelimited(AString, Separator);
end;

end.

