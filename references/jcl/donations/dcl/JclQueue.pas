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
{**************************************************************************************************}
{                                                                                                  }
{ The Delphi Container Library                                                                     }
{                                                                                                  }
{**************************************************************************************************}

// Last modified: $Date$
// For history see end of file

unit JclQueue;

{$I jcl.inc}

interface

uses
  JclBase, JclAbstractContainer, JclDCL_intf, JclDCLUtil;

type
  TJclIntfQueue = class(TJclAbstractContainer, IIntfQueue)
  private
    FCapacity: Integer;
    FElements: TDynIInterfaceArray;
    FHead: Integer;
    FTail: Integer;
  protected
    { IIntfQueue }
    function Contains(AInterface: IInterface): Boolean;
    function Dequeue: IInterface;
    function Empty: Boolean;
    procedure Enqueue(AInterface: IInterface);
    function Size: Integer;
  public
    constructor Create(Capacity: Integer = DCLDefaultCapacity);
  end;

  TJclStrQueue = class(TJclAbstractContainer, IStrQueue)
  private
    FCapacity: Integer;
    FElements: TDynStringArray;
    FHead: Integer;
    FTail: Integer;
  protected
    { IStrQueue }
    function Contains(const AString: string): Boolean;
    function Dequeue: string;
    function Empty: Boolean;
    procedure Enqueue(const AString: string);
    function Size: Integer;
  public
    constructor Create(Capacity: Integer = DCLDefaultCapacity);
  end;

  TJclQueue = class(TJclAbstractContainer, IQueue)
  private
    FCapacity: Integer;
    FElements: TDynObjectArray;
    FHead: Integer;
    FTail: Integer;
  protected
    { IQueue }
    function Contains(AObject: TObject): Boolean;
    function Dequeue: TObject;
    function Empty: Boolean;
    procedure Enqueue(AObject: TObject);
    function Size: Integer;
  public
    constructor Create(Capacity: Integer = DCLDefaultCapacity);
  end;

implementation

//=== { TJclIntfQueue } ======================================================

constructor TJclIntfQueue.Create(Capacity: Integer = DCLDefaultCapacity);
begin
  inherited Create;
  FHead := 0;
  FTail := 0;
  FCapacity := Capacity;
  SetLength(FElements, FCapacity);
end;

function TJclIntfQueue.Contains(AInterface: IInterface): Boolean;
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
  I := FHead;
  while I <> FTail do
  begin
    if FElements[I] = AInterface then
    begin
      Result := True;
      Break;
    end;
    I := (I + 1) mod FCapacity;
  end;
end;

function TJclIntfQueue.Dequeue: IInterface;
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := nil;
  if FTail = FHead then
    Exit;
  Result := FElements[FHead];
  FElements[FHead] := nil;
  FHead := (FHead + 1) mod FCapacity;
end;

function TJclIntfQueue.Empty: Boolean;
begin
  Result := FTail = FHead;
end;

procedure TJclIntfQueue.Enqueue(AInterface: IInterface);
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
  FElements[FTail] := AInterface;
  FTail := (FTail + 1) mod FCapacity;
end;

function TJclIntfQueue.Size: Integer;
begin
  Result := FTail - FHead;
end;

//=== { TJclStrQueue } =======================================================

constructor TJclStrQueue.Create(Capacity: Integer = DCLDefaultCapacity);
begin
  inherited Create;
  FHead := 0;
  FTail := 0;
  FCapacity := Capacity;
  SetLength(FElements, FCapacity);
end;

function TJclStrQueue.Contains(const AString: string): Boolean;
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
  I := FHead;
  while I <> FTail do
  begin
    if FElements[I] = AString then
    begin
      Result := True;
      Break;
    end;
    I := (I + 1) mod FCapacity;
  end;
end;

function TJclStrQueue.Dequeue: string;
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := '';
  if FTail = FHead then
    Exit;
  Result := FElements[FHead];
  FElements[FHead] := '';
  FHead := (FHead + 1) mod FCapacity;
end;

function TJclStrQueue.Empty: Boolean;
begin
  Result := FTail = FHead;
end;

procedure TJclStrQueue.Enqueue(const AString: string);
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
  FElements[FTail] := AString;
  FTail := (FTail + 1) mod FCapacity;
end;

function TJclStrQueue.Size: Integer;
begin
  Result := FTail - FHead;
end;

//=== { TJclQueue } ==========================================================

constructor TJclQueue.Create(Capacity: Integer = DCLDefaultCapacity);
begin
  inherited Create;
  FCapacity := Capacity;
  SetLength(FElements, FCapacity);
end;

function TJclQueue.Contains(AObject: TObject): Boolean;
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
  I := FHead;
  while I <> FTail do
  begin
    if FElements[I] = AObject then
    begin
      Result := True;
      Break;
    end;
    I := (I + 1) mod FCapacity;
  end;
end;

function TJclQueue.Dequeue: TObject;
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := nil;
  if FTail = FHead then
    Exit;
  Result := FElements[FHead];
  FElements[FHead] := nil;
  FHead := (FHead + 1) mod FCapacity;
end;

function TJclQueue.Empty: Boolean;
begin
  Result := FTail = FHead;
end;

procedure TJclQueue.Enqueue(AObject: TObject);
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
  FElements[FTail] := AObject;
  FTail := (FTail + 1) mod FCapacity;
end;

function TJclQueue.Size: Integer;
begin
  Result := FTail - FHead;
end;

end.
