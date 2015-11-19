unit supertypes;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

interface

type
{$IFNDEF FPC}
{$IFDEF CPUX64}
  PtrInt = Int64;
  PtrUInt = UInt64;
{$ELSE}
  PtrInt = longint;
  PtrUInt = Longword;
{$ENDIF}
{$ENDIF}
  SuperInt = Int64;

{$if (sizeof(Char) = 1)}
  SOChar = WideChar;
  SOIChar = Word;
  PSOChar = PWideChar;
{$IFDEF FPC}
  SOString = UnicodeString;
{$ELSE}
  SOString = WideString;
{$ENDIF}
{$else}
  SOChar = Char;
  SOIChar = Word;
  PSOChar = PChar;
  SOString = string;
{$ifend}
implementation

end.
