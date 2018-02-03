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
{ The Original Code is JclSharedMMFMem.pas.                                                        }
{                                                                                                  }
{ The Initial Developer of the Original Code is Andreas Hausladen.                                 }
{ Portions created by Andreas Hausladen are Copyright (C) Andreas Hausladen. All rights reserved.  }
{                                                                                                  }
{ Contributors:                                                                                    }
{   Andreas Hausladen (ahuser)                                                                     }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Description: Shared memory manager                                                               }
{                                                                                                  }
{**************************************************************************************************}

// Last modified: $Date$

unit JclSharedMMFMem;

{$I jcl.inc}

interface

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  // Needs the Win32API FileMapping functions
  {$ENDIF UNIX}
  SysUtils;

type
  ESharedMemError = class(Exception);

function SharedGetMem(var p{: Pointer}; const Name: string; Size: Cardinal;
  DesiredAccess: Cardinal = FILE_MAP_ALL_ACCESS): Integer;
  // liefert ERROR_ALREADY_EXISTS, wenn der Speicher bereits verwendet wird,
  // ansonsten 0

function SharedAllocMem(const Name: string; Size: Cardinal;
  DesiredAccess: Cardinal = FILE_MAP_ALL_ACCESS): Pointer;
  // Ruft SharedGetMem auf und löscht, falls der Speicher neu reserviert wurde
  // dessen Inhalt mit Nullen

function SharedFreeMem(var p{: Pointer}): Boolean;
  // Gibt den Speicher frei, wenn er nicht noch in Verwendung ist


function SharedOpenMem(var p{: Pointer}; const Name: string;
  DesiredAccess: Cardinal = FILE_MAP_ALL_ACCESS): Boolean; overload;
  // liefert True, wenn der Speicher vorhanden ist, ansonsten False

function SharedOpenMem(const Name: string;
  DesiredAccess: Cardinal = FILE_MAP_ALL_ACCESS): Pointer; overload;
  // liefert nil, wenn der Speicher nicht vorhanden ist

function SharedCloseMem(var p{: Pointer}): Boolean;
  // Gibt den Speicher frei, wenn er nicht noch in Verwendung ist

implementation

resourcestring
  SInvalidMMFName = 'Invalid MMF name "%s"';
  SMMFNameAlreadyOpened = 'Shared MMF name "%s" already opened.';

{$IFDEF MSWINDOWS}

function CloseFileMapping(hMap: Cardinal): Boolean;
begin
  Result := CloseHandle(hMap);
end;

{$ENDIF MSWINDOWS}


type
  PMMFHandleList = ^TMMFHandleList;
  TMMFHandleList = record
    Next: PMMFHandleList;
    Memory: Pointer;
    Handle: THandle;
    Name: string;
  end;

var
  MMFHandleList: PMMFHandleList = nil;

function SharedGetMem(var p{: Pointer}; const Name: string; Size: Cardinal;
  DesiredAccess: Cardinal = FILE_MAP_ALL_ACCESS): Integer;
var
  h: THandle;
  n, iterate: PMMFHandleList;
  Protect: Cardinal;
begin
  Result := 0;
  Pointer(p) := nil;

  if (Name = '') or (Pos('\', Name) > 0) or (Pos('/', Name) > 0) then
    raise ESharedMemError.CreateFmt(SInvalidMMFName, [Name]);

 // search for same name
  iterate := MMFHandleList;
  while iterate <> nil do
  begin
    if iterate^.Name = Name then
      raise ESharedMemError.CreateFmt(SMMFNameAlreadyOpened, [Name]);
    iterate := iterate^.Next;
  end;

 // open file mapping
  h := OpenFileMapping(DesiredAccess, False, PChar(Name));
  if h = 0 then
  begin
    if Size = 0 then
       Exit; // do not create it
    Protect := PAGE_READWRITE;
    {$IFDEF MSWINDOWS}
    if (Win32Platform = VER_PLATFORM_WIN32_WINDOWS) and
       (DesiredAccess = FILE_MAP_COPY) then
      Protect := PAGE_WRITECOPY;
    {$ENDIF MSWINDOWS}

    h := CreateFileMapping(INVALID_HANDLE_VALUE, nil, Protect, 0, Size, PChar(Name));
  end
  else
    Result := ERROR_ALREADY_EXISTS;

  case GetLastError of
    ERROR_ALREADY_EXISTS:
      Result := ERROR_ALREADY_EXISTS;
  else
    if h = 0 then
      RaiseLastOSError;
  end;

 // map view
  Pointer(p) := MapViewOfFile(h, DesiredAccess, 0, 0, Size);
  if Pointer(p) = nil then
  begin
    try
      RaiseLastOSError;
    except
      CloseFileMapping(h);
      raise;
    end;
  end;

 // add list item to MMFHandleList
  New(n);
  n^.Name := Name;
  n^.Handle := h;
  n^.Memory := Pointer(p);
  n^.Next := nil;

  if MMFHandleList = nil then
    MMFHandleList := n
  else
  begin
    iterate := MMFHandleList;
    while iterate^.Next <> nil do
      iterate := iterate^.Next;
    iterate^.Next := n;
  end;
end;

function SharedAllocMem(const Name: string; Size: Cardinal;
  DesiredAccess: Cardinal = FILE_MAP_ALL_ACCESS): Pointer;
begin
  if SharedGetMem(Result, Name, Size, DesiredAccess) <> ERROR_ALREADY_EXISTS then
    if ((DesiredAccess and (FILE_MAP_WRITE or FILE_MAP_COPY)) <> 0) and
       (Size > 0) then
      FillChar(Pointer(Result)^, Size, 0);
end;

function SharedFreeMem(var p{: Pointer}): Boolean;
var
  n, iterate: PMMFHandleList;
begin
  Result := False;
  iterate := MMFHandleList;
  n := nil;
  while iterate <> nil do
  begin
    if iterate^.Memory = Pointer(p) then
    begin
      UnmapViewOfFile(iterate^.Memory);
      CloseFileMapping(iterate^.Handle);

      if n = nil then
        MMFHandleList := iterate^.Next
      else
      begin
        n^.Next := iterate^.Next;
        Dispose(iterate);
        Pointer(p) := nil;
        Result := True;
        Break;
      end;
    end;
    n := iterate;
    iterate := iterate^.Next;
  end;
end;

function SharedOpenMem(var p{: Pointer}; const Name: string;
  DesiredAccess: Cardinal = FILE_MAP_ALL_ACCESS): Boolean;
begin
  Result := SharedGetMem(p, Name, 0, DesiredAccess) = ERROR_ALREADY_EXISTS;
end;

function SharedOpenMem(const Name: string;
  DesiredAccess: Cardinal = FILE_MAP_ALL_ACCESS): Pointer;
begin
  SharedGetMem(Result, Name, 0, DesiredAccess);
end;

function SharedCloseMem(var p{: Pointer}): Boolean;
begin
  Result := SharedFreeMem(p);
end;


procedure FinalizeMMFHandleList;
var
  n, iterate: PMMFHandleList;
begin
  iterate := MMFHandleList;
  while iterate <> nil do
  begin
    UnmapViewOfFile(iterate^.Memory);
    CloseFileMapping(iterate^.Handle);

    n := iterate^.Next;
    Dispose(iterate);
    iterate := n;
  end;
end;

finalization
  FinalizeMMFHandleList;

end.
