{ **************************************************************************** }
{                                                                              }
{    Iterator wrapper interface for FindFirst/FindNext/FindClose               }
{    Copyright (c) 2001 Barry Kelly.                                           }
{    barry_j_kelly@hotmail.com                                                 }
{                                                                              }
{    The contents of this file are subject to the Mozilla Public License       }
{    Version 1.1 (the "License"); you may not use this file except in          }
{    compliance with the License. You may obtain a copy of the License at      }
{    http://www.mozilla.org/MPL/                                               }
{                                                                              }
{    Software distributed under the License is distributed on an "AS IS"       }
{    basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the   }
{    License for the specific language governing rights and limitations        }
{    under the License.                                                        }
{                                                                              }
{    The Original Code is FindFileIter.pas                                     }
{                                                                              }
{    The Initial Developer of the Original Code is Barry Kelly.                }
{    Portions created by Barry Kelly are Copyright (C) 2001                    }
{    Barry Kelly. All Rights Reserved.                                         }
{                                                                              }
{    Alternatively, the contents of this file may be used under the terms      }
{    of the Lesser GNU Public License (the  "LGPL License"), in which case     }
{    the provisions of LGPL License are applicable instead of those            }
{    above.  If you wish to allow use of your version of this file only        }
{    under the terms of the LPGL License and not to allow others to use        }
{    your version of this file under the MPL, indicate your decision by        }
{    deleting  the provisions above and replace  them with the notice and      }
{    other provisions required by the LGPL License.  If you do not delete      }
{    the provisions above, a recipient may use your version of this file       }
{    under either the MPL or the LPGL License.                                 }
{                                                                              }
{ **************************************************************************** }
{ $Id$ }
unit FindFileIter;

interface

uses
  SysUtils;

type
  IFindFileIterator = interface
    ['{E7D04349-3848-483E-9FEB-DAEB0B0FBEAF}']

    { returns false if next not found }
    function Next: Boolean;

    function SearchRec: TSearchRec;
    function Time: TDateTime;
    function Size: Int64;
    function Attr: Integer;
    function Name: TFileName;
  end;

{ returns false if no first file found }
function CreateFindFile(const Path: string; IncludeAttr: Integer;
  out iffi: IFindFileIterator): Boolean;

implementation

type
  TWin32FindFileIterator = class(TInterfacedObject, IFindFileIterator)
  private
    FSearchRec: TSearchRec;
    FValid: Boolean;
    FBasePath: string;
    function IsValid: Boolean;
  public
    constructor Create(const Path: string; IncludeAttr: Integer);
    destructor Destroy; override;

    function Attr: Integer;
    function Name: TFileName;
    function Next: Boolean;
    function SearchRec: TSearchRec;
    function Size: Int64;
    function Time: TDateTime;

    property Valid: Boolean read FValid;
  end;

function CreateFindFile(const Path: string; IncludeAttr: Integer;
  out iffi: IFindFileIterator): Boolean;
var
  iter: TWin32FindFileIterator;
begin
  iter := TWin32FindFileIterator.Create(Path, IncludeAttr);
  Result := iter.Valid;
  if Result then
    iffi := iter
  else
  begin
    iffi := nil;
    iter.Free;
  end;
end;

{ TWin32FindFileIterator }

constructor TWin32FindFileIterator.Create(const Path: string;
  IncludeAttr: Integer);
begin
  FValid := FindFirst(Path, IncludeAttr, FSearchRec) = 0;
  FBasePath := ExtractFilePath(Path);
end;

destructor TWin32FindFileIterator.Destroy;
begin
  FindClose(FSearchRec);
  inherited;
end;

function TWin32FindFileIterator.Attr: Integer;
begin
  Assert(IsValid);
  Result := FSearchRec.Attr;
end;

function TWin32FindFileIterator.Name: TFileName;
begin
  Assert(IsValid);
  Result := FBasePath + FSearchRec.Name;
end;

function TWin32FindFileIterator.Next: Boolean;
begin
  FValid := FindNext(FSearchRec) = 0;
  Result := FValid;
end;

function TWin32FindFileIterator.SearchRec: TSearchRec;
begin
  Assert(IsValid);
  Result := FSearchRec;
end;

{$IFDEF ConditionalExpressions}
  {$IF CompilerVersion >= 14}
    {$WARN SYMBOL_PLATFORM OFF}
  {$IFEND}
{$ENDIF}
function TWin32FindFileIterator.Size: Int64;
begin
  Assert(IsValid);
  {$IFDEF MSWINDOWS}
  Result := FSearchRec.FindData.nFileSizeHigh shl 32 +
    FSearchRec.FindData.nFileSizeLow;
  {$ELSE}
  Result := FSearchRec.Size;
  {$ENDIF}
end;

function TWin32FindFileIterator.Time: TDateTime;
begin
  Result := FileDateToDateTime(FSearchRec.Time);
end;

function TWin32FindFileIterator.IsValid: Boolean;
begin
  if not FValid then
    raise Exception.Create('Tried to access invalid FindFile iterator');
  Result := True;
end;

end.
