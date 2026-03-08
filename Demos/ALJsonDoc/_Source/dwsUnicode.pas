{**********************************************************************}
{                                                                      }
{    "The contents of this file are subject to the Mozilla Public      }
{    License Version 1.1 (the "License"); you may not use this         }
{    file except in compliance with the License. You may obtain        }
{    a copy of the License at http://www.mozilla.org/MPL/              }
{                                                                      }
{    Software distributed under the License is distributed on an       }
{    "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express       }
{    or implied. See the License for the specific language             }
{    governing rights and limitations under the License.               }
{                                                                      }
{    Copyright Creative IT.                                            }
{    Current maintainer: Eric Grange                                   }
{                                                                      }
{**********************************************************************}
unit dwsUnicode;

{$I dws.inc}

interface

uses
   System.Classes, System.SysUtils;

type

   TUnicodeStringListFlag = (usflSorted, usflCaseInsensitive);
   TUnicodeStringListFlags = set of TUnicodeStringListFlag;

   // Simple Unicode string list, serves both to alleviate the lack of
   // Unicode TStringList in FreePascal and performance issues in both
   // Delphi and FPC implementations
   TUnicodeStringList = class
      private
         FItems : array of UnicodeString;
         FCount : NativeInt;
         FFlags : TUnicodeStringListFlags;

      protected
         function GetString(index : NativeInt) : UnicodeString; inline;
         procedure SetString(index : NativeInt; const v : UnicodeString); inline;
         function GetValueFromIndex(index : NativeInt) : UnicodeString; inline;
         function GetValue(const name : UnicodeString) : UnicodeString;
         procedure SetValue(const name, value: UnicodeString);

         function Compare(const s1, s2 : UnicodeString) : Integer; virtual;
         function CompareIndex(index1, index2 : NativeInt) : Integer;

         function GetSorted : Boolean;
         procedure SetSorted(const val : Boolean);
         function GetCaseSensitive : Boolean;
         procedure SetCaseSensitive(const val : Boolean);

      public
         procedure Assign(src : TUnicodeStringList);
         procedure AssignFromTStrings(src : TStrings);

         function Add(const s : UnicodeString) : NativeInt;
         procedure Insert(index : NativeInt; const s : UnicodeString);

         procedure Delete(index : NativeInt);
         procedure Clear;

         function Find(const s : UnicodeString; var index : NativeInt) : Boolean;
         function IndexOf(const s : UnicodeString) : NativeInt;
         function Contains(const s : UnicodeString) : Boolean; inline;

         function FindName(const name : UnicodeString; var index : NativeInt) : Boolean;
         function IndexOfName(const name : UnicodeString) : NativeInt;

         procedure Exchange(index1, index2 : NativeInt);
         procedure Sort;

         property Strings[index : NativeInt] : UnicodeString read GetString write SetString; default;
         property Values[const name : UnicodeString] : UnicodeString read GetValue write SetValue;
         property ValueFromIndex[index : NativeInt] : UnicodeString read GetValueFromIndex;
         property Count : NativeInt read FCount;

         property Sorted : Boolean read GetSorted write SetSorted;
         property CaseSensitive : Boolean read GetCaseSensitive write SetCaseSensitive;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses dwsUtils;

// ------------------
// ------------------ TUnicodeStringList ------------------
// ------------------

// GetString
//
function TUnicodeStringList.GetString(index : NativeInt) : UnicodeString;
begin
   Result := FItems[index];
end;

// SetString
//
procedure TUnicodeStringList.SetString(index : NativeInt; const v : UnicodeString);
begin
   FItems[index] := v;
end;

// GetValueFromIndex
//
function TUnicodeStringList.GetValueFromIndex(index : NativeInt) : UnicodeString;
var
   p : NativeInt;
begin
   p := Pos('=', FItems[index]);
   if p > 0 then
      Result := Copy(FItems[index], p+1)
   else Result := '';
end;

// GetValue
//
function TUnicodeStringList.GetValue(const name : UnicodeString) : UnicodeString;
var
   i : NativeInt;
begin
   i := IndexOfName(name);
   if i >= 0 then
      Result := Copy(FItems[i], Length(name)+1)
   else Result := '';
end;

// SetValue
//
procedure TUnicodeStringList.SetValue(const name, value: UnicodeString);
var
   i : NativeInt;
begin
   i := IndexOfName(name);
   if i >= 0 then
      FItems[i] := name + '=' + value
   else Add(name + '=' + value);
end;

// Compare
//
function TUnicodeStringList.Compare(const s1, s2 : UnicodeString) : Integer;
begin
   if usflCaseInsensitive in FFlags then
      Result := UnicodeCompareText(s1, s2)
   else Result := UnicodeCompareStr(s1, s2);
end;

// CompareIndex
//
function TUnicodeStringList.CompareIndex(index1, index2 : NativeInt) : Integer;
begin
   Result := Compare(FItems[index1], FItems[index2]);
end;

// GetSorted
//
function TUnicodeStringList.GetSorted : Boolean;
begin
   Result := usflSorted in FFlags;
end;

// SetSorted
//
procedure TUnicodeStringList.SetSorted(const val : Boolean);
begin
   if val then begin
      if not (usflSorted in FFlags) then begin
         Sort;
         Include(FFlags, usflSorted);
      end;
   end else Exclude(FFlags, usflSorted);
end;

// GetCaseSensitive
//
function TUnicodeStringList.GetCaseSensitive : Boolean;
begin
   Result := usflCaseInsensitive in FFlags;
end;

// SetCaseSensitive
//
procedure TUnicodeStringList.SetCaseSensitive(const val : Boolean);
begin
   if val <> (usflCaseInsensitive in FFlags) then begin
      if val then
         Include(FFlags, usflCaseInsensitive)
      else Exclude(FFlags, usflCaseInsensitive);
      if usflSorted in FFlags then
         Sort;
   end;
end;

// Assign
//
procedure TUnicodeStringList.Assign(src : TUnicodeStringList);
var
   i : NativeInt;
begin
   FCount := src.FCount;
   SetLength(FItems, FCount);
   for i := 0 to FCount-1 do
      FItems[i] := src.FItems[i];
   if FFlags <> src.FFlags then begin
      if usflSorted in FFlags then
         Sort;
   end;
end;

// AssignFromTStrings
//
procedure TUnicodeStringList.AssignFromTStrings(src : TStrings);
var
   i : NativeInt;
begin
   FCount := src.Count;
   SetLength(FItems, FCount);
   for i := 0 to FCount-1 do
      FItems[i] := UnicodeString(src[i]);
   if usflSorted in FFlags then
      Sort;
end;

// IndexOf
//
function TUnicodeStringList.IndexOf(const s : UnicodeString) : NativeInt;
begin
   if usflSorted in FFlags then begin
      if Find(s, Result) then Exit;
   end else begin
      for Result := 0 to Count-1 do
         if FItems[Result] = s then Exit;
   end;
   Result := -1;
end;

// Contains
//
function TUnicodeStringList.Contains(const s : UnicodeString) : Boolean;
begin
   Result := (IndexOf(s) >= 0);
end;

// FindName
//
function TUnicodeStringList.FindName(const name : UnicodeString; var index : NativeInt) : Boolean;
var
   lo, hi, mid, cmp, n, nc : NativeInt;
   initial : UnicodeString;
begin
   Result := False;
   initial := Name + '=';
   n := Length(initial);
   lo := 0;
   hi := Count-1;
   while lo <= hi do begin
      mid := (lo+hi) shr 1;
      nc := Length(FItems[mid]);
      if nc >= n then begin
         cmp := UnicodeCompareLen(PWideChar(Pointer(FItems[mid])), PWideChar(Pointer(initial)), n);
      end else begin
         cmp := UnicodeCompareLen(PWideChar(Pointer(FItems[mid])), PWideChar(Pointer(initial)), nc);
         if cmp = 0 then
            cmp := -1;
      end;
      if cmp < 0 then
         lo := mid+1
      else begin
         hi := mid-1;
         if cmp = 0 then
            Result := True;
      end;
   end;
   index := lo;
end;

// IndexOfName
//
function TUnicodeStringList.IndexOfName(const name : UnicodeString) : NativeInt;
var
   n, nc : NativeInt;
begin
   if not Sorted then begin
      n:=Length(name);
      for Result:=0 to Count-1 do begin
         nc:=Length(FItems[Result]);
         if     (nc>n) and (FItems[Result][n+1]='=')
            and (UnicodeCompareLen(PWideChar(Pointer(name)),
                                   PWideChar(Pointer(FItems[Result])), n)=0) then Exit;
      end;
      Result:=-1;
   end else begin
      if not FindName(name, Result) then
         Result:=-1;
   end;
end;

// Add
//
function TUnicodeStringList.Add(const s : UnicodeString) : NativeInt;
begin
   if usflSorted in FFlags then begin
      Find(s, Result);
      Insert(Result, s);
   end else begin
      if FCount = Length(FItems) then
         SetLength(FItems, FCount+(FCount div 4)+4);
      FItems[FCount] := s;
      Result := FCount;
      Inc(FCount);
   end;
end;

// Insert
//
procedure TUnicodeStringList.Insert(index : NativeInt; const s : UnicodeString);
begin
   if FCount = Length(FItems) then
      SetLength(FItems, (FCount div 4)+4);
   if index < FCount then begin
      System.Move(FItems[index], FItems[index+1], (FCount-index)*SizeOf(String));
      PPointer(FItems[index])^ := nil;
   end;
   FItems[index] := s;
end;

// Delete
//
procedure TUnicodeStringList.Delete(index : NativeInt);
var
   n : NativeInt;
begin
   FItems[index] := '';
   n := FCount-index-1;
   if n > 0 then
      System.Move(FItems[index+1], FItems[index], n*SizeOf(String));
end;

// Clear
//
procedure TUnicodeStringList.Clear;
begin
   SetLength(FItems, 0);
   FCount := 0;
end;

// Find
//
function TUnicodeStringList.Find(const s : UnicodeString; var index : NativeInt) : Boolean;
var
   low, high, mid, cmp : NativeInt;
begin
   Result := False;
   low := 0;
   high := Count-1;
   while low <= high do begin
      mid := (low + high) shr 1;
      cmp := Compare(FItems[mid], s);
      if cmp < 0 then
         low := mid + 1
      else begin
         high := mid - 1;
         if cmp = 0 then begin
            Result := True;
         end;
      end;
   end;
   index := low;
end;

// Exchange
//
procedure TUnicodeStringList.Exchange(index1, index2 : NativeInt);
var
   p1, p2 : PPointer;
   buf : Pointer;
begin
   p1 := @FItems[index1];
   p2 := @FItems[index2];
   buf := p1^;
   p1^ := p2^;
   p2^ := buf;
end;

// Sort
//
procedure TUnicodeStringList.Sort;
var
   qs : TQuickSort;
begin
   case FCount of
      0, 1 : ;
      2 : if Compare(FItems[0], FItems[1]) > 0 then
         Exchange(0, 1);
   else
      qs.CompareMethod := CompareIndex;
      qs.SwapMethod := Exchange;
      qs.Sort(0, FCount-1);
   end;
end;

end.
