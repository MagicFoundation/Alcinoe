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
{ The Original Code is AbstractContainer.pas.                                                      }
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

unit JclAbstractContainer;

{$I jcl.inc}

interface

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  Libc,
  {$ENDIF UNIX}
  JclBase, JclDCL_Intf, JclDCLUtil;

type
  TJclIntfCriticalSection = class(TObject, IInterface)
  private
    FCriticalSection: TRTLCriticalSection;
  protected
    function QueryInterface(const IID: TGUID; out Obj): HRESULT; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TJclAbstractContainer = class(TInterfacedObject)
  {$IFDEF THREADSAFE}
  private
    FCriticalSection: TJclIntfCriticalSection;
  protected
    function EnterCriticalSection: IInterface;
  public
    constructor Create;
    destructor Destroy; override;
  {$ENDIF THREADSAFE}
  end;

implementation

//=== { TJclIntfCriticalSection } ============================================

constructor TJclIntfCriticalSection.Create;
begin
  inherited Create;
  InitializeCriticalSection(FCriticalSection);
end;

destructor TJclIntfCriticalSection.Destroy;
begin
  DeleteCriticalSection(FCriticalSection);
  inherited Destroy;
end;

function TJclIntfCriticalSection._AddRef: Integer;
begin
  EnterCriticalSection(FCriticalSection);
  Result := 0;
end;

function TJclIntfCriticalSection._Release: Integer;
begin
  LeaveCriticalSection(FCriticalSection);
  Result := 0;
end;

function TJclIntfCriticalSection.QueryInterface(const IID: TGUID; out Obj): HRESULT;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

//=== { TJclAbstractContainer } ==============================================

{$IFDEF THREADSAFE}

constructor TJclAbstractContainer.Create;
begin
  FCriticalSection := TJclIntfCriticalSection.Create;
end;

destructor TJclAbstractContainer.Destroy;
begin
  FCriticalSection.Free;
  inherited Destroy;
end;

function TJclAbstractContainer.EnterCriticalSection: IInterface;
begin
  Result := FCriticalSection as IInterface;
end;

{$ENDIF THREADSAFE}

end.

