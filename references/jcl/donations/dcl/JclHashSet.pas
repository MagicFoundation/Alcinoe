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
{ The Original Code is HashSet.pas.                                                                }
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

unit JclHashSet;

{$I jcl.inc}

interface

uses
  Classes,
  JclBase, JclAbstractContainer, JclDCL_intf, JclDCLUtil, JclHashMap;

type
  TJclIntfHashSet = class(TJclAbstractContainer, IIntfCollection, IIntfSet, IIntfCloneable)
  private
    FMap: IIntfIntfMap;
  protected
    { IIntfCollection }
    function Add(AInterface: IInterface): Boolean;
    function AddAll(ACollection: IIntfCollection): Boolean;
    procedure Clear;
    function Contains(AInterface: IInterface): Boolean;
    function ContainsAll(ACollection: IIntfCollection): Boolean;
    function Equals(ACollection: IIntfCollection): Boolean;
    function First: IIntfIterator;
    function IsEmpty: Boolean;
    function Last: IIntfIterator;
    function Remove(AInterface: IInterface): Boolean;
    function RemoveAll(ACollection: IIntfCollection): Boolean;
    function RetainAll(ACollection: IIntfCollection): Boolean;
    function Size: Integer;
    { IIntfSet }
    procedure Intersect(ACollection: IIntfCollection);
    procedure Subtract(ACollection: IIntfCollection);
    procedure Union(ACollection: IIntfCollection);
    { IIntfCloneable }
    function Clone: IInterface;
  public
    constructor Create(Capacity: Integer = DCLDefaultCapacity);
    destructor Destroy; override;
  end;

  TJclStrHashSet = class(TJclAbstractContainer, IStrCollection, IStrSet, ICloneable)
  private
    FMap: IStrMap;
  protected
    { IStrCollection }
    function Add(const AString: string): Boolean;
    function AddAll(ACollection: IStrCollection): Boolean;
    procedure Clear;
    function Contains(const AString: string): Boolean;
    function ContainsAll(ACollection: IStrCollection): Boolean;
    function Equals(ACollection: IStrCollection): Boolean;
    function First: IStrIterator;
    function IsEmpty: Boolean;
    function Last: IStrIterator;
    function Remove(const AString: string): Boolean;
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
    { IIntfSet }
    procedure Intersect(ACollection: IStrCollection);
    procedure Subtract(ACollection: IStrCollection);
    procedure Union(ACollection: IStrCollection);
    { IIntfCloneable }
    function Clone: TObject;
  public
    constructor Create(Capacity: Integer = DCLDefaultCapacity);
    destructor Destroy; override;
  end;

  TJclHashSet = class(TJclAbstractContainer, ICollection, ISet, ICloneable)
  private
    FMap: IMap;
  protected
    { ICollection }
    function Add(AObject: TObject): Boolean;
    function AddAll(ACollection: ICollection): Boolean;
    procedure Clear;
    function Contains(AObject: TObject): Boolean;
    function ContainsAll(ACollection: ICollection): Boolean;
    function Equals(ACollection: ICollection): Boolean;
    function First: IIterator;
    function IsEmpty: Boolean;
    function Last: IIterator;
    function Remove(AObject: TObject): Boolean;
    function RemoveAll(ACollection: ICollection): Boolean;
    function RetainAll(ACollection: ICollection): Boolean;
    function Size: Integer;
    { ISet }
    procedure Intersect(ACollection: ICollection);
    procedure Subtract(ACollection: ICollection);
    procedure Union(ACollection: ICollection);
    { ICloneable }
    function Clone: TObject;
  public
    constructor Create(Capacity: Integer = DCLDefaultCapacity; AOwnsObject: Boolean = False);
    destructor Destroy; override;
  end;

implementation

const
  // (rom) this needs an explanation
  RefUnique: TObject = @RefUnique;

var
  IRefUnique: IInterface = nil;

//=== { TJclIntfHashSet } ====================================================

constructor TJclIntfHashSet.Create(Capacity: Integer = DCLDefaultCapacity);
begin
  inherited Create;
  FMap := TJclIntfIntfHashMap.Create(Capacity);
  if IRefUnique = nil then
    IRefUnique := TInterfacedObject.Create;
end;

destructor TJclIntfHashSet.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TJclIntfHashSet.Add(AInterface: IInterface): Boolean;
begin
  Result := not FMap.ContainsKey(AInterface);
  if Result then
    FMap.PutValue(AInterface, IRefUnique);
end;

function TJclIntfHashSet.AddAll(ACollection: IIntfCollection): Boolean;
var
  It: IIntfIterator;
begin
  Result := ACollection <> nil;
  if Result then
  begin
    It := ACollection.First;
    while It.HasNext do
      Result := Add(It.Next) and Result;
  end;
end;

procedure TJclIntfHashSet.Clear;
begin
  FMap.Clear;
end;

function TJclIntfHashSet.Clone: IInterface;
var
  NewSet: TJclIntfHashSet;
begin
  NewSet := TJclIntfHashSet.Create;
  NewSet.FMap := IIntfIntfMap(IIntfCloneable(FMap).Clone);
  Result := NewSet;
end;

function TJclIntfHashSet.Contains(AInterface: IInterface): Boolean;
begin
  Result := FMap.ContainsKey(AInterface);
end;

function TJclIntfHashSet.ContainsAll(ACollection: IIntfCollection): Boolean;
var
  It: IIntfIterator;
begin
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

function TJclIntfHashSet.Equals(ACollection: IIntfCollection): Boolean;
var
  It: IIntfIterator;
  ItMap: IIntfIterator;
begin
  Result := False;
  if ACollection = nil then
    Exit;
  if FMap.Size <> ACollection.Size then
    Exit;
  It := ACollection.First;
  ItMap := FMap.Values.First;
  while ItMap.HasNext do
    if ItMap.Next <> It.Next then
      Exit;
  Result := True;
end;

function TJclIntfHashSet.First: IIntfIterator;
begin
  Result := FMap.KeySet.First;
end;

procedure TJclIntfHashSet.Intersect(ACollection: IIntfCollection);
begin
  RetainAll(ACollection);
end;

function TJclIntfHashSet.IsEmpty: Boolean;
begin
  Result := FMap.IsEmpty;
end;

function TJclIntfHashSet.Last: IIntfIterator;
begin
  Result := FMap.KeySet.Last;
end;

function TJclIntfHashSet.Remove(AInterface: IInterface): Boolean;
begin
  Result := FMap.Remove(AInterface) = IInterface(IRefUnique);
end;

function TJclIntfHashSet.RemoveAll(ACollection: IIntfCollection): Boolean;
var
  It: IIntfIterator;
begin
  Result := True;
  if ACollection = nil then
    Exit;
  It := ACollection.First;
  while It.HasNext do
    Result := Remove(It.Next) and Result;
end;

function TJclIntfHashSet.RetainAll(ACollection: IIntfCollection): Boolean;
var
  ItMap: IIntfIterator;
begin
  Result := False;
  if ACollection = nil then
    Exit;
  ItMap := FMap.Values.First;
  while ItMap.HasNext do
    if not ACollection.Contains(ItMap.Next) then
      ItMap.Remove;
end;

function TJclIntfHashSet.Size: Integer;
begin
  Result := FMap.Size;
end;

procedure TJclIntfHashSet.Subtract(ACollection: IIntfCollection);
begin
  RemoveAll(ACollection);
end;

procedure TJclIntfHashSet.Union(ACollection: IIntfCollection);
begin
  AddAll(ACollection);
end;

//=== { TJclStrHashSet } =====================================================

constructor TJclStrHashSet.Create(Capacity: Integer = DCLDefaultCapacity);
begin
  inherited Create;
  FMap := TJclStrHashMap.Create(Capacity, False);
end;

destructor TJclStrHashSet.Destroy;
begin
  Clear;
  // (rom) no Free of FMap?
  inherited Destroy;
end;

function TJclStrHashSet.Add(const AString: string): Boolean;
begin
  Result := not FMap.ContainsKey(AString);
  if Result then
    FMap.PutValue(AString, RefUnique);
end;

function TJclStrHashSet.AddAll(ACollection: IStrCollection): Boolean;
var
  It: IStrIterator;
begin
  Result := False;
  if ACollection = nil then
    Exit;
  It := ACollection.First;
  while It.HasNext do
    Result := Add(It.Next) or Result;
end;

procedure TJclStrHashSet.Clear;
begin
  FMap.Clear;
end;

function TJclStrHashSet.Clone: TObject;
var
  NewSet: TJclStrHashSet;
begin
  NewSet := TJclStrHashSet.Create;
  NewSet.FMap := TJclStrHashMap(ICloneable(FMap).Clone);
  Result := NewSet;
end;

function TJclStrHashSet.Contains(const AString: string): Boolean;
begin
  Result := FMap.ContainsKey(AString);
end;

function TJclStrHashSet.ContainsAll(ACollection: IStrCollection): Boolean;
var
  It: IStrIterator;
begin
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

function TJclStrHashSet.Equals(ACollection: IStrCollection): Boolean;
var
  It: IStrIterator;
  ItMap: IStrIterator;
begin
  Result := False;
  if ACollection = nil then
    Exit;
  if FMap.Size <> ACollection.Size then
    Exit;
  It := ACollection.First;
  ItMap := FMap.KeySet.First;
  while ItMap.HasNext do
    if ItMap.Next <> It.Next then
      Exit;
  Result := True;
end;

function TJclStrHashSet.First: IStrIterator;
begin
  Result := FMap.KeySet.First;
end;

procedure TJclStrHashSet.Intersect(ACollection: IStrCollection);
begin
  RetainAll(ACollection);
end;

function TJclStrHashSet.IsEmpty: Boolean;
begin
  Result := FMap.IsEmpty;
end;

function TJclStrHashSet.Last: IStrIterator;
begin
  Result := FMap.KeySet.Last;
end;

function TJclStrHashSet.Remove(const AString: string): Boolean;
begin
  Result := FMap.Remove(AString) = RefUnique;
end;

function TJclStrHashSet.RemoveAll(ACollection: IStrCollection): Boolean;
var
  It: IStrIterator;
begin
  Result := True;
  if ACollection = nil then
    Exit;
  It := ACollection.First;
  while It.HasNext do
    //Result := Remove(It.Next) and Result;

    //Daniele Teti 28/12/2004
    if not Remove(It.Next) then
      Result := False;
end;

function TJclStrHashSet.RetainAll(ACollection: IStrCollection): Boolean;
var
  It: IStrIterator;
begin
  Result := False;
  if ACollection = nil then
    Exit;
  It := ACollection.First;
  while It.HasNext do
    if not FMap.ContainsKey(It.Next) then
      FMap.Remove(It.Next);
end;

function TJclStrHashSet.Size: Integer;
begin
  Result := FMap.Size;
end;

procedure TJclStrHashSet.Subtract(ACollection: IStrCollection);
begin
  RemoveAll(ACollection);
end;

procedure TJclStrHashSet.Union(ACollection: IStrCollection);
begin
  AddAll(ACollection);
end;

//=== { TJclHashSet } ========================================================

constructor TJclHashSet.Create(Capacity: Integer = DCLDefaultCapacity;
  AOwnsObject: Boolean = False);
begin
  inherited Create;
  FMap := TJclHashMap.Create(Capacity, AOwnsObject);
end;

destructor TJclHashSet.Destroy;
begin
  Clear;
  // (rom) no Free of FMap?
  inherited Destroy;
end;

function TJclHashSet.Add(AObject: TObject): Boolean;
begin
  Result := not FMap.ContainsKey(AObject);
  if Result then
    FMap.PutValue(AObject, RefUnique);
end;

function TJclHashSet.AddAll(ACollection: ICollection): Boolean;
var
  It: IIterator;
begin
  Result := False;
  if ACollection = nil then
    Exit;
  It := ACollection.First;
  while It.HasNext do
    Result := Add(It.Next) or Result;
end;

procedure TJclHashSet.Clear;
begin
  FMap.Clear;
end;

function TJclHashSet.Clone: TObject;
var
  NewSet: TJclHashSet;
begin
  NewSet := TJclHashSet.Create;
  NewSet.FMap := TJclHashMap(ICloneable(FMap).Clone);
  Result := NewSet;
end;

function TJclHashSet.Contains(AObject: TObject): Boolean;
begin
  Result := FMap.ContainsKey(AObject);
end;

function TJclHashSet.ContainsAll(ACollection: ICollection): Boolean;
var
  It: IIterator;
begin
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

function TJclHashSet.Equals(ACollection: ICollection): Boolean;
var
  It: IIterator;
  ItMap: IIterator;
begin
  Result := False;
  if ACollection = nil then
    Exit;
  if FMap.Size <> ACollection.Size then
    Exit;
  It := ACollection.First;
  ItMap := FMap.Values.First;
  while ItMap.HasNext do
    if ItMap.Next <> It.Next then
      Exit;
  Result := True;
end;

function TJclHashSet.First: IIterator;
begin
  Result := FMap.KeySet.First;
end;

procedure TJclHashSet.Intersect(ACollection: ICollection);
begin
  RetainAll(ACollection);
end;

function TJclHashSet.IsEmpty: Boolean;
begin
  Result := FMap.IsEmpty;
end;

function TJclHashSet.Last: IIterator;
begin
  Result := FMap.KeySet.Last;
end;

function TJclHashSet.Remove(AObject: TObject): Boolean;
begin
  Result := FMap.Remove(AObject) = RefUnique;
end;

function TJclHashSet.RemoveAll(ACollection: ICollection): Boolean;
var
  It: IIterator;
begin
  Result := True;
  if ACollection = nil then
    Exit;
  It := ACollection.First;
  while It.HasNext do
    Result := Remove(It.Next) and Result;
end;

function TJclHashSet.RetainAll(ACollection: ICollection): Boolean;
var
  ItMap: IIterator;
begin
  Result := False;
  if ACollection = nil then
    Exit;
  ItMap := FMap.Values.First;
  while ItMap.HasNext do
    if not ACollection.Contains(ItMap.Next) then
      ItMap.Remove;
end;

function TJclHashSet.Size: Integer;
begin
  Result := FMap.Size;
end;

procedure TJclHashSet.Subtract(ACollection: ICollection);
begin
  RemoveAll(ACollection);
end;

procedure TJclHashSet.Union(ACollection: ICollection);
begin
  AddAll(ACollection);
end;

function TJclStrHashSet.GetAsStrings: TStrings;
begin
  Result := TStringList.Create;
  try
    AppendToStrings(Result);
  except
    Result.Free;
    raise;
  end;
end;

procedure TJclStrHashSet.LoadFromStrings(Strings: TStrings);
begin
  Clear;
  AppendFromStrings(Strings);
end;

procedure TJclStrHashSet.AppendToStrings(Strings: TStrings);
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

procedure TJclStrHashSet.SaveToStrings(Strings: TStrings);
begin
  Strings.Clear;
  AppendToStrings(Strings);
end;

procedure TJclStrHashSet.AppendFromStrings(Strings: TStrings);
var
  I: Integer;
begin
  for I := 0 to Strings.Count - 1 do
    Add(Strings[I]);
end;

function TJclStrHashSet.GetAsDelimited(Separator: string): string;
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

procedure TJclStrHashSet.LoadDelimited(AString, Separator: string);
begin
  Clear;
  AppendDelimited(AString, Separator);
end;

procedure TJclStrHashSet.AppendDelimited(AString, Separator: string);
begin
  DCLAppendDelimited(Self, AString, Separator);
end;

end.

