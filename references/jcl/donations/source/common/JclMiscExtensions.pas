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
{ The Original Code is JclMiscExtensions.pas.                                                      }
{                                                                                                  }
{ The Initial Developers of the Original Code are documented in the accompanying help file         }
{ JCLHELP.hlp. Portions created by these individuals are Copyright (C) of these individuals.       }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Contains new or modified functions for several units                                             }
{                                                                                                  }
{ Known Issues:                                                                                    }
{   This is a preview - class and functionnames might be changed                                   }
{                                                                                                  }
{ Unit owner: Uwe Schuster                                                                         }
{ Last modified: January 30, 2005                                                                  }
{                                                                                                  }
{**************************************************************************************************}

{
Todo
- unitversioning detail flags for GetLocationInfoStr
}

unit JclMiscExtensions;

{$I jcl.inc}

interface

uses
  Windows, Classes, SysUtils,
  JclDebug, JclFileUtils, JclPeImage, JclSysInfo, JclUnitVersioning;

//new function for JclPeImage
function InsertSection(const ExecutableFileName: TFileName;
  SectionStream: TStream; SectionName: string): Boolean;

//changed function for JclDebug
function GetLocationInfoStr(const Addr: Pointer; IncludeModuleName: Boolean;
  IncludeAddressOffset: Boolean; IncludeStartProcLineOffset: Boolean;
  IncludeVAdress: Boolean): string;

implementation

//(usc) this is a modified version of JclDebug.InsertDebugDataIntoExecutableFile
// JclDebug should use it too
function InsertSection(const ExecutableFileName: TFileName;
  SectionStream: TStream; SectionName: string): Boolean;
var
  ImageStream: TMemoryStream;
  NtHeaders: PImageNtHeaders;
  Sections, LastSection, NewSection: PImageSectionHeader;
  VirtualAlignedSize: DWORD;
  I, X, NeedFill: Integer;
  SectionDataSize: Integer;

  procedure RoundUpToAlignment(var Value: DWORD; Alignment: DWORD);
  begin
    if (Value mod Alignment) <> 0 then
      Value := ((Value div Alignment) + 1) * Alignment;
  end;

begin
  Result := Assigned(SectionStream) and (SectionName <> '');
  if not Result then
    Exit;
  ImageStream := TMemoryStream.Create;
  try
    try
      ImageStream.LoadFromFile(ExecutableFileName);
      SectionDataSize := SectionStream.Size;
      NtHeaders := PeMapImgNtHeaders(ImageStream.Memory);
      Assert(NtHeaders <> nil);
      Sections := PeMapImgSections(NtHeaders);
      Assert(Sections <> nil);
      // Check whether there is not a section with the name already. If so, return True (#0000069)
      if PeMapImgFindSection(NtHeaders, SectionName) <> nil then
      begin
        Result := True;
        Exit;
      end;

      LastSection := Sections;
      Inc(LastSection, NtHeaders^.FileHeader.NumberOfSections - 1);
      NewSection := LastSection;
      Inc(NewSection);

      // Increase the number of sections
      Inc(NtHeaders^.FileHeader.NumberOfSections);
      FillChar(NewSection^, SizeOf(TImageSectionHeader), #0);
      // JCLDEBUG Virtual Address
      NewSection^.VirtualAddress := LastSection^.VirtualAddress + LastSection^.Misc.VirtualSize;
      RoundUpToAlignment(NewSection^.VirtualAddress, NtHeaders^.OptionalHeader.SectionAlignment);
      // JCLDEBUG Physical Offset
      NewSection^.PointerToRawData := LastSection^.PointerToRawData + LastSection^.SizeOfRawData;
      RoundUpToAlignment(NewSection^.PointerToRawData, NtHeaders^.OptionalHeader.FileAlignment);
      // JCLDEBUG Section name
      StrPLCopy(PChar(@NewSection^.Name), SectionName, IMAGE_SIZEOF_SHORT_NAME);
      // JCLDEBUG Characteristics flags
      NewSection^.Characteristics := IMAGE_SCN_MEM_READ or IMAGE_SCN_CNT_INITIALIZED_DATA;

      // Size of virtual data area
      NewSection^.Misc.VirtualSize := SectionDataSize;
      VirtualAlignedSize := SectionDataSize;
      RoundUpToAlignment(VirtualAlignedSize, NtHeaders^.OptionalHeader.SectionAlignment);
      // Update Size of Image
      Inc(NtHeaders^.OptionalHeader.SizeOfImage, VirtualAlignedSize);
      // Raw data size
      NewSection^.SizeOfRawData := SectionDataSize;
      RoundUpToAlignment(NewSection^.SizeOfRawData, NtHeaders^.OptionalHeader.FileAlignment);
      // Update Initialized data size
      Inc(NtHeaders^.OptionalHeader.SizeOfInitializedData, NewSection^.SizeOfRawData);

      // Fill data to alignment
      NeedFill := Integer(NewSection^.SizeOfRawData) - SectionDataSize;

      // Note: Delphi linker seems to generate incorrect (unaligned) size of
      // the executable when adding TD32 debug data so the position could be
      // behind the size of the file then.
      ImageStream.Seek(NewSection^.PointerToRawData, soFromBeginning);
      ImageStream.CopyFrom(SectionStream, 0);
      X := 0;
      for I := 1 to NeedFill do
        ImageStream.WriteBuffer(X, 1);

      ImageStream.SaveToFile(ExecutableFileName);
    except
      Result := False;
    end;
  finally
    ImageStream.Free;
  end;
end;

const
  ModuleCodeOffset = $1000;

//(usc) this is a modified version of JclDebug.GetLocationInfoStr
// it adds the revision to the sourcename
function GetLocationInfoStr(const Addr: Pointer; IncludeModuleName, IncludeAddressOffset,
  IncludeStartProcLineOffset: Boolean; IncludeVAdress: Boolean): string;
var
  Info, StartProcInfo: TJclLocationInfo;
  OffsetStr, StartProcOffsetStr: string;
  Module: HMODULE;
  RevisionStr: string;
  Idx: Integer;
begin
  OffsetStr := '';
  if GetLocationInfo(Addr, Info) then
  with Info do
  begin
    if LineNumber > 0 then
    begin
      if IncludeStartProcLineOffset and GetLocationInfo(Pointer(Cardinal(Info.Address) -
        Cardinal(Info.OffsetFromProcName)), StartProcInfo) and (StartProcInfo.LineNumber > 0) then
          StartProcOffsetStr := Format(' + %d', [LineNumber - StartProcInfo.LineNumber])
      else
        StartProcOffsetStr := '';
      if IncludeAddressOffset then
      begin
        if OffsetFromLineNumber >= 0 then
          OffsetStr := Format(' + $%x', [OffsetFromLineNumber])
        else
          OffsetStr := Format(' - $%x', [-OffsetFromLineNumber])
      end;
      RevisionStr := '';
      {.$IFDEF UNITVERSIONING}
      GetUnitVersioning.LoadModuleUnitVersioningInfo(Info.DebugInfo.Module);
      Idx := GetUnitVersioning.IndexOf(SourceName);
      if Idx <> -1 then
        RevisionStr := ' ' + GetUnitVersioning.Items[Idx].Revision;
      {.$ENDIF UNITVERSIONING}
      Result := Format('[%p] %s.%s (Line %u, "%s%s"%s)%s', [Addr, UnitName, ProcedureName, LineNumber,
        SourceName, RevisionStr, StartProcOffsetStr, OffsetStr]);
    end
    else
    begin
      if IncludeAddressOffset then
        OffsetStr := Format(' + $%x', [OffsetFromProcName]);
      if UnitName <> '' then
        Result := Format('[%p] %s.%s%s', [Addr, UnitName, ProcedureName, OffsetStr])
      else
        Result := Format('[%p] %s%s', [Addr, ProcedureName, OffsetStr]);
    end;
  end
  else
    Result := Format('[%p]', [Addr]);
  if IncludeVAdress or IncludeModuleName then
  begin
    Module := ModuleFromAddr(Addr);
    if IncludeVAdress then
      OffsetStr :=  Format('(%p) ', [Pointer(DWORD(Addr) - Module - ModuleCodeOffset)]);
    if IncludeModuleName then
      Insert(Format('{%-12s}', [ExtractFileName(GetModulePath(Module))]), Result, 11);
    Result := OffsetStr + Result;
  end;
end;

end.
