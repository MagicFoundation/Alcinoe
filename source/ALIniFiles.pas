{*************************************************************
www:          http://sourceforge.net/projects/alcinoe/
svn:          svn checkout svn://svn.code.sf.net/p/alcinoe/code/ alcinoe-code
Author(s):    St�phane Vander Clock (alcinoe@arkadia.com)
Sponsor(s):   Arkadia SA (http://www.arkadia.com)

product:      Alcinoe inifiles
Version:      4.01

Description:  AnsiString version of delphi Unicode Tinifile

Legal issues: Copyright (C) 1999-2013 by Arkadia Software Engineering

              This software is provided 'as-is', without any express
              or implied warranty.  In no event will the author be
              held liable for any  damages arising from the use of
              this software.

              Permission is granted to anyone to use this software
              for any purpose, including commercial applications,
              and to alter it and redistribute it freely, subject
              to the following restrictions:

              1. The origin of this software must not be
                 misrepresented, you must not claim that you wrote
                 the original software. If you use this software in
                 a product, an acknowledgment in the product
                 documentation would be appreciated but is not
                 required.

              2. Altered source versions must be plainly marked as
                 such, and must not be misrepresented as being the
                 original software.

              3. This notice may not be removed or altered from any
                 source distribution.

              4. You must register this software by sending a picture
                 postcard to the author. Use a nice stamp and mention
                 your name, street address, EMail address and any
                 comment you like to say.

Know bug :

History:

Link :

* Please send all your feedback to alcinoe@arkadia.com
* If you have downloaded this source from a website different from
  sourceforge.net, please get the last version on http://sourceforge.net/projects/alcinoe/
* Please, help us to keep the development of these components free by
  promoting the sponsor on http://static.arkadia.com/html/alcinoe_like.html
**************************************************************}
unit ALIniFiles;

{$R-,T-,H+,X+}

interface

uses SysUtils,
     Classes,
     ALFcnString,
     ALStringList;

type
  EALIniFileException = class(Exception);

  TALCustomIniFile = class(TObject)
  private
    FFileName: AnsiString;
  protected
    const SectionNameSeparator: AnsiString = '\';
    procedure InternalReadSections(const Section: AnsiString; Strings: TALStrings; SubSectionNamesOnly, Recurse: Boolean); virtual;
  public
    constructor Create(const FileName: AnsiString);
    function SectionExists(const Section: AnsiString): Boolean;
    function ReadString(const Section, Ident, Default: AnsiString): AnsiString; virtual; abstract;
    procedure WriteString(const Section, Ident, Value: AnsiString); virtual; abstract;
    function ReadInteger(const Section, Ident: AnsiString; Default: Longint): Longint; virtual;
    procedure WriteInteger(const Section, Ident: AnsiString; Value: Longint); virtual;
    function ReadBool(const Section, Ident: AnsiString; Default: Boolean): Boolean; virtual;
    procedure WriteBool(const Section, Ident: AnsiString; Value: Boolean); virtual;
    function ReadBinaryStream(const Section, Name: AnsiString; Value: TStream): Integer; virtual;
    function ReadDate(const Section, Name: AnsiString; Default: TDateTime; const AFormatSettings: TALFormatSettings): TDateTime; virtual;
    function ReadDateTime(const Section, Name: AnsiString; Default: TDateTime; const AFormatSettings: TALFormatSettings): TDateTime; virtual;
    function ReadFloat(const Section, Name: AnsiString; Default: Double; const AFormatSettings: TALFormatSettings): Double; virtual;
    function ReadTime(const Section, Name: AnsiString; Default: TDateTime; const AFormatSettings: TALFormatSettings): TDateTime; virtual;
    procedure WriteBinaryStream(const Section, Name: AnsiString; Value: TStream); virtual;
    procedure WriteDate(const Section, Name: AnsiString; Value: TDateTime; const AFormatSettings: TALFormatSettings); virtual;
    procedure WriteDateTime(const Section, Name: AnsiString; Value: TDateTime; const AFormatSettings: TALFormatSettings); virtual;
    procedure WriteFloat(const Section, Name: AnsiString; Value: Double; const AFormatSettings: TALFormatSettings); virtual;
    procedure WriteTime(const Section, Name: AnsiString; Value: TDateTime; const AFormatSettings: TALFormatSettings); virtual;
    procedure ReadSection(const Section: AnsiString; Strings: TALStrings); virtual; abstract;
    procedure ReadSections(Strings: TALStrings); overload; virtual; abstract;
    procedure ReadSections(const Section: AnsiString; Strings: TALStrings); overload; virtual;
    procedure ReadSubSections(const Section: AnsiString; Strings: TALStrings; Recurse: Boolean = False); virtual;
    procedure ReadSectionValues(const Section: AnsiString; Strings: TALStrings); virtual; abstract;
    procedure EraseSection(const Section: AnsiString); virtual; abstract;
    procedure DeleteKey(const Section, Ident: AnsiString); virtual; abstract;
    procedure UpdateFile; virtual; abstract;
    function ValueExists(const Section, Ident: AnsiString): Boolean; virtual;
    property FileName: AnsiString read FFileName;
  end;

  TALIniFile = class(TALCustomIniFile)
  public
    destructor Destroy; override;
    function ReadString(const Section, Ident, Default: AnsiString): AnsiString; override;
    procedure WriteString(const Section, Ident, Value: AnsiString); override;
    procedure ReadSection(const Section: AnsiString; Strings: TALStrings); override;
    procedure ReadSections(Strings: TALStrings); override;
    procedure ReadSectionValues(const Section: AnsiString; Strings: TALStrings); override;
    procedure EraseSection(const Section: AnsiString); override;
    procedure DeleteKey(const Section, Ident: AnsiString); override;
    procedure UpdateFile; override;
  end;

implementation

uses RTLConsts,
     Windows,
     ALFcnFile;

{**************************************************************}
constructor TALCustomIniFile.Create(const FileName: AnsiString);
begin
  FFileName := FileName;
end;

{**************************************************************************}
function TALCustomIniFile.SectionExists(const Section: AnsiString): Boolean;
var
  S: TALStrings;
begin
  S := TALStringList.Create;
  try
    ReadSection(Section, S);
    Result := S.Count > 0;
  finally
    S.Free;
  end;
end;

{*************************************************************************************************}
function TALCustomIniFile.ReadInteger(const Section, Ident: AnsiString; Default: Longint): Longint;
var
  IntStr: AnsiString;
begin
  IntStr := ReadString(Section, Ident, '');
  if (Length(IntStr) > 2) and (IntStr[1] = '0') and
     ((IntStr[2] = 'X') or (IntStr[2] = 'x')) then
    IntStr := '$' + ALCopyStr(IntStr, 3, Maxint);
  Result := ALStrToIntDef(IntStr, Default);
end;

{****************************************************************************************}
procedure TALCustomIniFile.WriteInteger(const Section, Ident: AnsiString; Value: Longint);
begin
  WriteString(Section, Ident, ALIntToStr(Value));
end;

{**********************************************************************************************}
function TALCustomIniFile.ReadBool(const Section, Ident: AnsiString; Default: Boolean): Boolean;
begin
  Result := ReadInteger(Section, Ident, Ord(Default)) <> 0;
end;

{*******************************************************************************************************************************************}
function TALCustomIniFile.ReadDate(const Section, Name: AnsiString; Default: TDateTime; const AFormatSettings: TALFormatSettings): TDateTime;
var DateStr: AnsiString;
begin
  DateStr := ReadString(Section, Name, '');
  Result := Default;
  if DateStr <> '' then
  try
    Result := ALStrToDate(DateStr, AFormatSettings);
  except
    on EConvertError do
      // Ignore EConvertError exceptions
    else
      raise;
  end;
end;

{***********************************************************************************************************************************************}
function TALCustomIniFile.ReadDateTime(const Section, Name: AnsiString; Default: TDateTime; const AFormatSettings: TALFormatSettings): TDateTime;
var DateStr: AnsiString;
begin
  DateStr := ReadString(Section, Name, '');
  Result := Default;
  if DateStr <> '' then
  try
    Result := ALStrToDateTime(DateStr, AFormatSettings);
  except
    on EConvertError do
      // Ignore EConvertError exceptions
    else
      raise;
  end;
end;

{**************************************************************************************************************************************}
function TALCustomIniFile.ReadFloat(const Section, Name: AnsiString; Default: Double; const AFormatSettings: TALFormatSettings): Double;
var FloatStr: AnsiString;
begin
  FloatStr := ReadString(Section, Name, '');
  Result := Default;
  if FloatStr <> '' then
  try
    Result := ALStrToFloat(FloatStr, AFormatSettings);
  except
    on EConvertError do
      // Ignore EConvertError exceptions
    else
      raise;
  end;
end;

{*******************************************************************************************************************************************}
function TALCustomIniFile.ReadTime(const Section, Name: AnsiString; Default: TDateTime; const AFormatSettings: TALFormatSettings): TDateTime;
var TimeStr: AnsiString;
begin
  TimeStr := ReadString(Section, Name, '');
  Result := Default;
  if TimeStr <> '' then
  try
    Result := ALStrToTime(TimeStr, AFormatSettings);
  except
    on EConvertError do
      // Ignore EConvertError exceptions
    else
      raise;
  end;
end;

{********************************************************************************************************************************}
procedure TALCustomIniFile.WriteDate(const Section, Name: AnsiString; Value: TDateTime; const AFormatSettings: TALFormatSettings);
begin
  WriteString(Section, Name, ALDateToStr(Value, AFormatSettings));
end;

{************************************************************************************************************************************}
procedure TALCustomIniFile.WriteDateTime(const Section, Name: AnsiString; Value: TDateTime; const AFormatSettings: TALFormatSettings);
begin
  WriteString(Section, Name, ALDateTimeToStr(Value, AFormatSettings));
end;

{******************************************************************************************************************************}
procedure TALCustomIniFile.WriteFloat(const Section, Name: AnsiString; Value: Double; const AFormatSettings: TALFormatSettings);
begin
  WriteString(Section, Name, ALFloatToStr(Value, AFormatSettings));
end;

{********************************************************************************************************************************}
procedure TALCustomIniFile.WriteTime(const Section, Name: AnsiString; Value: TDateTime; const AFormatSettings: TALFormatSettings);
begin
  WriteString(Section, Name, ALTimeToStr(Value, AFormatSettings));
end;

{*************************************************************************************}
procedure TALCustomIniFile.WriteBool(const Section, Ident: AnsiString; Value: Boolean);
const Values: array[Boolean] of AnsiString = ('0', '1');
begin
  WriteString(Section, Ident, Values[Value]);
end;

{*******************************************************************************}
function TALCustomIniFile.ValueExists(const Section, Ident: AnsiString): Boolean;
var
  S: TALStrings;
begin
  S := TALStringList.Create;
  try
    ReadSection(Section, S);
    Result := S.IndexOf(Ident) > -1;
  finally
    S.Free;
  end;
end;

{*************************************************************************}
function TALCustomIniFile.ReadBinaryStream(const Section, Name: AnsiString;
  Value: TStream): Integer;
var
  Text: AnsiString;
  Stream: TMemoryStream;
  Pos: Integer;
begin
  Text := ReadString(Section, Name, '');
  if Text <> '' then begin

    if Value is TMemoryStream then Stream := TMemoryStream(Value)
    else Stream := TMemoryStream.Create;

    try
      Pos := Stream.Position;
      Stream.SetSize(Stream.Size + Length(Text) div 2);
      HexToBin(PAnsiChar(Text), PAnsiChar(Integer(Stream.Memory) + Stream.Position), Length(Text) div 2);
      Stream.Position := Pos;
      if Value <> Stream then
        Value.CopyFrom(Stream, Length(Text) div 2);
      Result := Stream.Size - Pos;
    finally
      if Value <> Stream then
        Stream.Free;
    end;

  end
  else Result := 0;
end;

{********************************************************************************************}
procedure TALCustomIniFile.WriteBinaryStream(const Section, Name: AnsiString; Value: TStream);
var
  Text: AnsiString;
  Stream: TMemoryStream;
begin
  SetLength(Text, (Value.Size - Value.Position) * 2);
  if Length(Text) > 0 then begin

    if Value is TMemoryStream then Stream := TMemoryStream(Value)
    else Stream := TMemoryStream.Create;

    try
      if Stream <> Value then begin
        Stream.CopyFrom(Value, Value.Size - Value.Position);
        Stream.Position := 0;
      end;
      BinToHex(PAnsiChar(Integer(Stream.Memory) + Stream.Position),
               PAnsiChar(Text),
               Stream.Size - Stream.Position);
    finally
      if Value <> Stream then Stream.Free;
    end;

  end;
  WriteString(Section, Name, Text);
end;

{*************************************************************************************************************************************}
procedure TALCustomIniFile.InternalReadSections(const Section: AnsiString; Strings: TALStrings; SubSectionNamesOnly, Recurse: Boolean);
var SLen, SectionLen, SectionEndOfs, I: Integer;
    S, SubSectionName: AnsiString;
    AllSections: TALStringList;
begin
  AllSections := TALStringList.Create;
  try
    ReadSections(AllSections);
    SectionLen := Length(Section);
    // Adjust end offset of section name to account for separator when present.
    SectionEndOfs := (SectionLen + 1) + Integer(SectionLen > 0);
    Strings.BeginUpdate;
    try
      for I := 0 to AllSections.Count - 1 do begin
        S := AllSections[I];
        SLen := Length(S);
        if (SectionLen = 0) or
           (SubSectionNamesOnly and (SLen > SectionLen) and ALSameText(Section, ALCopyStr(S, 1, SectionLen))) or
           (not SubSectionNamesOnly and (SLen >= SectionLen) and ALSameText(Section, ALCopyStr(S, 1, SectionLen))) then
        begin
          SubSectionName := ALCopyStr(S, SectionEndOfs, SLen + 1 - SectionEndOfs);
          if not Recurse and (ALPos(SectionNameSeparator, SubSectionName) <> 0) then Continue;
          if SubSectionNamesOnly then S := SubSectionName;
          Strings.Add(S);
        end;
      end;
    finally
      Strings.EndUpdate;
    end;
  finally
    AllSections.Free;
  end;
end;

{**************************************************************************************}
procedure TALCustomIniFile.ReadSections(const Section: AnsiString; Strings: TALStrings);
begin
  InternalReadSections(Section, Strings, False, True);
end;

{*******************************************************************************************************************}
procedure TALCustomIniFile.ReadSubSections(const Section: AnsiString; Strings: TALStrings; Recurse: Boolean = False);
begin
  InternalReadSections(Section, Strings, True, Recurse);
end;

{****************************}
destructor TALIniFile.Destroy;
begin
  UpdateFile;         // flush changes to disk
  inherited Destroy;
end;

{************************************************************************************}
function TALIniFile.ReadString(const Section, Ident, Default: AnsiString): AnsiString;
var
  Buffer: array[0..2047] of AnsiChar;
begin
  SetString(Result,
            Buffer,
            GetPrivateProfileStringA(PAnsiChar(Section),
                                     PAnsiChar(Ident),
                                     PAnsiChar(Default),
                                     Buffer,
                                     Length(Buffer),
                                     PAnsiChar(FFileName)));
end;

{************************************************************************}
procedure TALIniFile.WriteString(const Section, Ident, Value: AnsiString);
begin
  if not WritePrivateProfileStringA(PAnsiChar(Section),
                                    PAnsiChar(Ident),
                                    PAnsiChar(Value),
                                    PAnsiChar(FFileName)) then
    raise EALIniFileException.CreateResFmt(@SIniFileWriteError, [FileName]);
end;

{*****************************************************}
procedure TALIniFile.ReadSections(Strings: TALStrings);
const CStdBufSize = 16384; // chars
var P, LBuffer: PAnsiChar;
    LCharCount: Integer;
begin
  LBuffer := nil;
  try
    // try to read the file in a 16Kchars buffer
    GetMem(LBuffer, CStdBufSize);
    Strings.BeginUpdate;
    try
      Strings.Clear;
      LCharCount := GetPrivateProfileStringA(nil,
                                             nil,
                                             nil,
                                             LBuffer,
                                             CStdBufSize,
                                             PAnsiChar(FFileName));

      // the buffer is too small; approximate the buffer size to fit the contents
      if LCharCount = CStdBufSize - 2 then begin
        LCharCount := AlGetFileSize(FFileName);
        ReallocMem(LBuffer, LCharCount);
        LCharCount := GetPrivateProfileStringA(nil,
                                               nil,
                                               nil,
                                               LBuffer,
                                               LCharCount,
                                               PAnsiChar(FFileName));
      end;

      // chars were read from the file; get the section names
      if LCharCount <> 0 then begin
        P := LBuffer;
        while P^ <> #0 do begin
          Strings.Add(P);
          Inc(P, StrLen(P) + 1);
        end;
      end;
    finally
      Strings.EndUpdate;
    end;
  finally
    FreeMem(LBuffer);
  end;
end;

{*******************************************************************************}
procedure TALIniFile.ReadSection(const Section: AnsiString; Strings: TALStrings);
var
  Buffer, P: PAnsiChar;
  CharCount: Integer;
  BufSize: Integer;

  procedure ReadStringData;
  begin
    Strings.BeginUpdate;
    try
      Strings.Clear;
      if CharCount <> 0 then begin
        P := Buffer;
        while P^ <> #0 do begin
          Strings.Add(P);
          Inc(P, StrLen(P) + 1);
        end;
      end;
    finally
      Strings.EndUpdate;
    end;
  end;

begin
  BufSize := 1024;

  while True do begin
    GetMem(Buffer, BufSize);
    try
      CharCount := GetPrivateProfileStringA(PAnsiChar(Section),
                                            nil,
                                            nil,
                                            Buffer,
                                            BufSize,
                                            PAnsiChar(FFileName));
      if CharCount < BufSize - 2 then begin
        ReadStringData;
        Break;
      end;
    finally
      FreeMem(Buffer, BufSize);
    end;
    BufSize := BufSize * 4;
  end;
end;

{*************************************************************************************}
procedure TALIniFile.ReadSectionValues(const Section: AnsiString; Strings: TALStrings);
var KeyList: TALStringList;
    I: Integer;
begin
  KeyList := TALStringList.Create;
  try
    ReadSection(Section, KeyList);
    Strings.BeginUpdate;
    try
      Strings.Clear;
      for I := 0 to KeyList.Count - 1 do
        Strings.Add(KeyList[I] + '=' + ReadString(Section, KeyList[I], ''))
    finally
      Strings.EndUpdate;
    end;
  finally
    KeyList.Free;
  end;
end;

{***********************************************************}
procedure TALIniFile.EraseSection(const Section: AnsiString);
begin
  if not WritePrivateProfileStringA(PAnsiChar(Section), nil, nil, PAnsiChar(FFileName)) then
    raise EALIniFileException.CreateResFmt(@SIniFileWriteError, [FileName]);
end;

{***************************************************************}
procedure TALIniFile.DeleteKey(const Section, Ident: AnsiString);
begin
  WritePrivateProfileStringA(PAnsiChar(Section), PAnsiChar(Ident), nil, PAnsiChar(FFileName));
end;

{******************************}
procedure TALIniFile.UpdateFile;
begin
  WritePrivateProfileStringA(nil, nil, nil, PAnsiChar(FFileName));
end;

end.
