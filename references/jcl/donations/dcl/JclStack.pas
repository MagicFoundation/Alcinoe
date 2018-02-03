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
{ The Original Code is Stack.pas.                                                                  }
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

unit JclStack;

{$I jcl.inc}

interface

uses
  JclBase, JclAbstractContainer, JclDCL_intf, JclDCLUtil;

type
  TJclIntfStack = class(TJclAbstractContainer, IIntfStack)
  private
    FElements: TDynIInterfaceArray;
    FCount: Integer;
    FCapacity: Integer;
  protected
    procedure Grow; virtual;
    { IIntfStack }
    function Contains(AInterface: IInterface): Boolean;
    function Empty: Boolean;
    function Pop: IInterface;
    procedure Push(AInterface: IInterface);
    function Size: Integer;
  public
    constructor Create(Capacity: Integer = DCLDefaultCapacity);
  end;

  TJclStrStack = class(TJclAbstractContainer, IStrStack)
  private
    FElements: TDynStringArray;
    FCount: Integer;
    FCapacity: Integer;
  protected
    procedure Grow; virtual;
    { IStrStack }
    function Contains(const AString: string): Boolean;
    function Empty: Boolean;
    function Pop: string;
    procedure Push(const AString: string);
    function Size: Integer;
  public
    constructor Create(Capacity: Integer = DCLDefaultCapacity);
  end;

  TJclStack = class(TJclAbstractContainer, IStack)
  private
    FElements: TDynObjectArray;
    FCount: Integer;
    FCapacity: Integer;
  protected
    procedure Grow; virtual;
    { IStack }
    function Contains(AObject: TObject): Boolean;
    function Empty: Boolean;
    function Pop: TObject;
    procedure Push(AObject: TObject);
    function Size: Integer;
  public
    constructor Create(Capacity: Integer = DCLDefaultCapacity);
  end;

implementation

//=== { TJclIntfStack } ======================================================

constructor TJclIntfStack.Create(Capacity: Integer = DCLDefaultCapacity);
begin
  inherited Create;
  FCount := 0;
  FCapacity := Capacity;
  SetLength(FElements, FCapacity);
end;

function TJclIntfStack.Contains(AInterface: IInterface): Boolean;
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
  for I := 0 to FCount - 1 do
    if FElements[I] = AInterface then
    begin
      Result := True;
      Break;
    end;
end;

function TJclIntfStack.Empty: Boolean;
begin
  Result := FCount = 0;
end;

procedure TJclIntfStack.Grow;
begin
  FCapacity := FCapacity + FCapacity div 4;
  SetLength(FElements, FCapacity);
end;

function TJclIntfStack.Pop: IInterface;
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  if FCount = 0 then
    Exit;
  Dec(FCount);
  Result := FElements[FCount];
end;

procedure TJclIntfStack.Push(AInterface: IInterface);
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  if AInterface = nil then
    Exit;
  if FCount = FCapacity then
    Grow;
  FElements[FCount] := AInterface;
  Inc(FCount);
end;

function TJclIntfStack.Size: Integer;
begin
  Result := FCount;
end;

//=== { TJclStrStack } =======================================================

constructor TJclStrStack.Create(Capacity: Integer = DCLDefaultCapacity);
begin
  inherited Create;
  FCount := 0;
  FCapacity := Capacity;
  SetLength(FElements, FCapacity);
end;

function TJclStrStack.Contains(const AString: string): Boolean;
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
  for I := 0 to FCount - 1 do
    if FElements[I] = AString then
    begin
      Result := True;
      Exit;
    end;
end;

function TJclStrStack.Empty: Boolean;
begin
  Result := FCount = 0;
end;

procedure TJclStrStack.Grow;
begin
  FCapacity := FCapacity + FCapacity div 4;
  SetLength(FElements, FCapacity);
end;

function TJclStrStack.Pop: string;
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  if FCount = 0 then
    Exit;
  Dec(FCount);
  Result := FElements[FCount];
end;

procedure TJclStrStack.Push(const AString: string);
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  if AString = '' then
    Exit;
  if FCount = FCapacity then
    Grow;
  FElements[FCount] := AString;
  Inc(FCount);
end;

function TJclStrStack.Size: Integer;
begin
  Result := FCount;
end;

//=== { TJclStack } ==========================================================

constructor TJclStack.Create(Capacity: Integer = DCLDefaultCapacity);
begin
  inherited Create;
  FCount := 0;
  FCapacity := Capacity;
  SetLength(FElements, FCapacity);
end;

function TJclStack.Contains(AObject: TObject): Boolean;
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
  for I := 0 to FCount - 1 do
    if FElements[I] = AObject then
    begin
      Result := True;
      Break;
    end;
end;

function TJclStack.Empty: Boolean;
begin
  Result := FCount = 0;
end;

procedure TJclStack.Grow;
begin
  FCapacity := FCapacity + FCapacity div 4;
  SetLength(FElements, FCapacity);
end;

function TJclStack.Pop: TObject;
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := nil;
  if FCount = 0 then
    Exit;
  Dec(FCount);
  Result := FElements[FCount];
end;

procedure TJclStack.Push(AObject: TObject);
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  if AObject = nil then
    Exit;
  if FCount = FCapacity then
    Grow;
  FElements[FCount] := AObject;
  Inc(FCount);
end;

function TJclStack.Size: Integer;
begin
  Result := FCount;
end;

end.
