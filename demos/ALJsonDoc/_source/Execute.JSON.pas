unit Execute.JSON;

(*
  TypInfo based JSON parser/builder for Delphi XE8 (c)2015 by Execute SARL

  Paul TOTH <contact@execute.fr>
  http://www.execute.fr

  ------------------------------------------------------------------------
  This program is free software; you can redistribute it and/or
  modify it under the terms of the GNU General Public License
  as published by the Free Software Foundation; either version 2
  of the License, or (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
  ------------------------------------------------------------------------

  2015-08-09 : version 1: ttoJSON(@v, TypeOf(t))
  2015-08-09 : version 2: JSON<T>.toJSON(v)
  2015-08-10 ! version 3: JSON.toJSON<T>(v) or JSON.toJSON(v) with type inference

  uses
    Execute.JSON;

  type
    TMyObject = class
      Name: string;
      Date: TDateTime;
      Nums: array of Integer;
      Plus: string;
    end;


    TMyRecord = record
      Name: string;
      Date: TDateTime;
      Nums: array of Integer;
      Minus: string; // not the same field as TMyObject !
    end;

  var
    t: TMyObject;
    r: TMyRecord;
    s: string;
  begin
    t := TMyObject.Create;
    t.Name := 'Paul';
    t.Date := Now();
    t.Nums := [1, 2, 3];
    t.Plus := 'plus';
    s := t.toJSON();
    t.Free;

  // s = '{"Name":"Paul","Date":"2015-08-07T17:32:27","Nums":[1,2,3],"Plus":"plus"}';

    t := TMyObject.Create;
    t.FromJSON(s);
    t.Free;

    fromJSON(@r, TypeInfo(TMyRecord), s);  // "Plus" is ignored, "Minus" still empty

    s := toJSON(@r, TypeInfo(TMyRecord));

  // s = '{"Name":"Paul","Date":"2015-08-07T17:32:27","Nums":[1,2,3],"Minus":null}';
  end;

  you can define how class fields are handled with predefined methods

type
   TMyObject = class
      Name: string;
      Date: TDateTime;
      Nums: array of Integer;
      List: TStringList;
      constructor Create;
      destructor Destroy; override;
      function JSONBuildField(const Name: string; Builder: TJSONBuilder): Boolean;
      function JSONParseField(const Name: string; Parser: TJSONParser): Boolean;
    end;

constructor TMyObject.create;
begin
  inherited;
  List := TStringList.Create;
end;

destructor TMyObject.Destroy;
begin
  List.Free;
  inherited;
end;

function TMyObject.JSONBuildField(const Name: string; Builder: TJSONBuilder): Boolean;
begin
  if Name = 'List' then
  begin
    Builder.AppendStrings(List);
    Exit(True);
  end;

  Result := False;
end;

function TMyObject.JSONParseField(const Name: string; Parser: TJSONParser): Boolean;
begin
  if Name = 'List' then
  begin
    Parser.GetStrings(List);
    Exit(True);
  end;

  Result := False;
end;

NB: you can use Builder.AppendObject() and Parser.ParseObject() to handle sub objects

This library DO NOT CREATE sub objects !

WARNING : inline fixed sized array do not have RTTI information

  do not use

    r = record
      a: array[0..5] of Integer;
    end;

  use

    ta = array[0..5] of Integer;

    r = record
      a: ta;
    end;

*)

interface

uses
  System.Classes,
  System.SysUtils,
  System.DateUtils,
  System.TypInfo;

const
  JSON_TRUE  = 'true';
  JSON_FALSE = 'false';
  JSON_NULL  = 'null';
  JSON_NIL   = JSON_NULL;

  JSON_BOOL  : array[False..True] of string = (JSON_FALSE, JSON_TRUE);

type
  EJSONError = class(Exception)
  end;

  // Build part let build a JSON string from a Delphi variable

  PBuildChain = ^TBuildChain;

  // add some functions to TStringBuilder for JSON
  // keep the "Build Chain"
  TJSONBuilder = class(TStringBuilder)
  private
    FBuild: PBuildChain;
    function KnowInstance(Instance: Pointer): Boolean;
    function BuildField(Name: string): Boolean;
    function GetPath: string;
  public
    procedure AppendString(const Str: string);
    procedure AppendStrings(List: TStrings); //overload;
//    procedure AppendStrings(List: TArray<string>); overload;
    procedure AppendObject(Obj: TObject); // to build JSON part of a subObject
  end;

  // Each time an object is converted to JSON, it's JSONBuildField method is saved with its instance and path
  // a child object can save it's own properties with a relative path name "childField"
  // a parent objet can save sub-objects properties with a global path name "parentField.childField"
  TBuildChain = record
    Build : function(Instance: TObject; const Name: string; Builder: TJSONBuilder): Boolean;
    Sender: TObject;
    Path  : string;
    Next  : PBuildChain;
  end;

  // Parser part let read back the JSON string

  PParseChain = ^TParseChain;

  TJSONParser = class
  private
    FText   : string;
    FIndex  : Integer;
    FEof    : Boolean;
    FParse  : PParseChain;
    function ParseField(Name: string): Boolean;
  public
    constructor Create(const AText: string);
    procedure ParseObject(Obj: TObject);  // to parse a subObject
    procedure ParseType(Instance, TypeInfo: Pointer);
    function NextChar: Char;
    function ReadChar: Char;
    function Skip(Ch: Char): Boolean;
    procedure Drop(Ch: Char);
    function SkipStr(const Str: string): Boolean;
    function GetKey: string;
    function GetValue: string;
    function GetString: string;
    function GetChar: Char;
    procedure GetStrings(List: TStrings); //overload;
//    function GetStrings: TArray<string>; overload;
    function GetNumber: string;
    function GetInteger: Integer;
    function GetBoolean: Boolean;
    function GetFloat: Double;
    function GetCurrency: Currency;
    function GetDateTime: TDateTime;
    function BeginObject: Boolean;
    function EndObject: Boolean;
    function BeginArray: Boolean;
    function EndArray: Boolean;
  end;

  // the ParseChain works like the BuildChain

  TParseChain = record
    Parse : function(Instance: TObject; const Name: string; Parser: TJSONParser): Boolean;
    Sender: TObject;
    Path  : string;
    Next  : PParseChain;
  end;

  // some RTTI infos

  TClassFieldInfo = record
    Name    : string;
    Offset  : Cardinal;
    TypeInfo: PTypeInfo;
  end;

  TClassFieldInfos = array of TClassFieldInfo;

  TClassFieldInfosHelper = record helper for TClassFieldInfos
    function IndexOf(Name: string): Integer;
  end;

  TClassPropInfos = array of PPropInfo;

  // this Helper do the big part of the job

  TTypeInfoHelper = record helper for TTypeInfo
    function DataPtr(Offset: Integer): Pointer; inline;
    function DataByte(Offset: Integer): Byte; inline;
    function DataLong(Offset: Integer): Integer; inline;

    function GetOrd(Instance: Pointer): Integer;
    procedure SetOrd(Instance: Pointer; Value: Integer);

  // records
    function RecordFieldsOfs : Integer;
    function RecordFieldCount: Integer;
    function RecordFieldType(Index: Integer): PRecordTypeField;
    function RecordFieldByName(const Name: string): PRecordTypeField;

  // dynarray
    function DynArrayLength(Instance: Pointer): Integer; inline;
    function DynArrayElType: PTypeInfo; inline;
    function DynArrayElSize: Integer; inline;

  // class
    procedure MergeClassFieldInfos(var Infos: TClassfieldInfos);
    function GetClassMethodAddress(const Name: string): Pointer;
    function GetClassPropInfos: TClassPropInfos;
    function GetClassFieldInfos: TClassFieldInfos;
    function GetMethodAddress(const Name: string): Pointer;

  // toJSON
    function toJSON(Instance: Pointer): string;
    procedure BuildJSON(Instance: Pointer; Builder: TJSONBuilder);
    procedure BuildJSONClass(Instance: Pointer; Builder: TJSONBuilder);
    procedure BuildJSONRecord(Instance: Pointer; Builder: TJSONBuilder);
    procedure BuildJSONDynArray(Instance: Pointer; Builder: TJSONBuilder);
    procedure BuildJSONArray(Instance: Pointer; Builder: TJSONBuilder);
    procedure BuildJSONSet(Instance: Pointer; Builder: TJSONBuilder);

  // fromJSON
    procedure FromJSON(Instance: Pointer; const JSON: string);
    procedure ParseJSON(Instance: Pointer; Parser: TJSONParser);
    procedure ParseJSONClass(Instance: Pointer; Parser: TJSONParser);
    procedure ParseJSONRecord(Instance: Pointer; Parser: TJSONParser);
    procedure ParseJSONDynArray(Instance: Pointer; Parser: TJSONParser);
    procedure ParseJSONArray(Instance: Pointer; Parser: TJSONParser);
    procedure ParseJSONSet(Instance: Pointer; Parser: TJSONParser);
    procedure ParseJSONShortString(Instance: Pointer; Parser: TJSONParser);
  end;

  TRecordTypeFieldHelper = record helper for TRecordTypeField
    function InstanceSize: Integer;
    function GetInstance(RecordInstance: Pointer): Pointer; inline;
  end;

  // finally, this Object Helper simplify everything

  TObjectHelper = class helper for TObject
    function toJSON: string;
    procedure fromJSON(const JSON: string);
  end;

  // thank's to Stefan Glienke to point me out the type inference !
  JSON = class
    class function toJSON<T>(const instance :T): string; inline;
    class procedure fromJSON<T>(var instance: T; const Str: string); inline;
  end;

var
  JSONFormat: TFormatSettings;

implementation

class function JSON.toJSON<T>(const instance :T): string;
begin
  Result := PTypeInfo(TypeInfo(T)).toJSON(@instance);
end;

class procedure JSON.fromJSON<T>(var instance: T; const Str: string);
begin
  PTypeInfo(TypeInfo(T)).FromJSON(@instance, Str);
end;

function JSONEscape(const Str: string): string;
var
  Index : Integer;
  Escape: Integer;
  Len   : Integer;
begin
  Escape := 0;
  Result := '"' + Str + '"';
  Len := Length(Result);
  for Index := 2 to Len - 1 do
    if CharInSet(Result[Index], ['\', '"']) then
    begin
      Result[Index] := '\';
      Inc(Escape);
    end;
  if Escape = 0 then
    Exit;
  Inc(Len, Escape);
  SetLength(Result, Len);
  Result[Len] := '"';
  Dec(Len);
  for Index := Length(Str) + 1 downto 2 do
  begin
    if Result[Index] = '\' then
    begin
      Result[Len] := Str[Index - 1];
      Dec(Len);
      Dec(Escape);
      if Escape = 0 then
        Exit;
    end;
    Result[Len] := Result[Index];
    Dec(Len);
  end;
end;

function JSONEncodeDate(const Value: TDate): string;
begin
  if Value < 1 then
    Result := JSON_NULL
  else
    Result := FormatDateTime('''"''yyyy-mm-dd''"''', Value);
end;

function JSONEncodeTime(const Value: TTime): string;
begin
  Result := FormatDateTime('''"''hh:nn:ss''"''', Value);
end;

function JSONEncodeDateTime(const Value: TDate): string;
begin
  if Value < 1 then
    Result := JSON_NIL
  else
    Result := FormatDateTime('''"''yyyy-mm-dd"T"hh:nn:ss''"''', Value);
end;

function JSONEncodeFloat(const Value: Extended): string;
begin
  Result := FloatToStr(Value, JSONFormat);
end;

function GetNum(const Str: string; Index, Count: Integer): Integer;
var
  Num: Integer;
begin
  Result := 0;
  while Count > 0 do
  begin
    Num := Ord(Str[Index]) - Ord('0');
    if (Num < 0) or (Num > 9) then
      raise EJSONError.Create('Invalid date ' + Str);
    Result := 10 * Result + Num;
    Inc(Index);
    Dec(Count);
  end;
end;

{ TJSONParser }

constructor TJSONParser.Create(const AText: string);
begin
  FText := AText;
  FIndex:= 1;
  FEof := FIndex > Length(FText);
end;

procedure TJSONParser.ParseObject(Obj: TObject);
begin
  PTypeInfo(Obj.ClassType.ClassInfo).ParseJSONClass(Obj, Self);
end;

procedure TJSONParser.ParseType(Instance, TypeInfo: Pointer);
begin
  PTypeInfo(TypeInfo).ParseJSON(Instance, Self);
end;

function TJSONParser.ParseField(Name: string): Boolean;
var
  Chain: PParseChain;
begin
  if FParse = nil then
    Exit(False);
  Chain := FParse;
  repeat
    if FParse = Chain then
      FParse.Path := Name + '.'
    else
      Name := Chain.Path + Name;
    Result := (@Chain.Parse <> nil) and Chain.Parse(Chain.Sender, Name, Self);
    Chain := Chain.Next;
  until Result or (Chain = nil);
end;

function TJSONParser.NextChar: Char;
begin
  if FEof then
    raise EJSONError.Create('End of JSON stream');
  Result := FText[FIndex];
end;

function TJSONParser.ReadChar: Char;
begin
  Result := NextChar;
  Inc(FIndex);
  FEof := FIndex > Length(FText);
end;

function TJSONParser.Skip(Ch: Char): Boolean;
begin
  Result := (FEof = False) and (FText[FIndex] = Ch);
  if Result then
    ReadChar;
end;

procedure TJSONParser.Drop(Ch: Char);
begin
  if not Skip(Ch) then
    raise EJSONError.Create('Expected char "' + ch +'" not found in "' + Copy(FText, 1, FIndex) + '"');
end;

function TJSONParser.SkipStr(const Str: string): Boolean;
var
  Start: Integer;
  Index: Integer;
begin
  Result := False;
  if FEof then
    Exit;
  Start := FIndex;
  if Start + Length(Str) > Length(FText) then
    Exit;
  for Index := 1 to Length(Str) do
  begin
    if FText[Start] <> Str[Index] then
      Exit;
    Inc(Start);
  end;
  Result := True;
  FIndex := Start;
  FEof := FIndex > Length(FText);
end;

function TJSONParser.GetKey: string;
begin
  Result := GetString;
  Drop(':');
end;

function TJSONParser.GetValue: string;
begin
  if NextChar = '"' then
    Result := GetString
  else
  if SkipStr(JSON_NIL) then
    Result := ''
  else
  if SkipStr(JSON_TRUE) then
    Result := '1'
  else
  if SkipStr(JSON_FALSE) then
    Result := '0'
  else
  if BeginArray then
  begin
    repeat
      GetValue;
    until EndArray;
    Result := '[]';
  end else
  if BeginObject then
  begin
    repeat
      GetKey;
      GetValue;
    until EndObject;
    Result := '{}';
  end else
  begin
    GetNumber;
  end;
end;

function TJSONParser.GetString: string;
var
  Start : Integer;
  Escape: Integer;
  Len   : Integer;
  Index : Integer;
  Ch    : Char;
begin
  if SkipStr(JSON_NULL) then
    Exit('');
  Drop('"');
  Start := FIndex;
  Escape := 0;
  while not Skip('"') do
  begin
    if ReadChar = '\' then
    begin
      Inc(Escape);
      ReadChar;
    end;
  end;
  Len := FIndex - Start - 1;
  Result := Copy(FText, Start, Len);
  if Escape > 0 then
  begin
    Start := 1;
    Index := 1;
    while Index <= Len do
    begin
      Ch := Result[Index];
      if Ch = '\' then
      begin
        Inc(Index);
        Ch := Result[Index];
      end;
      if Index > Start then
        Result[Start] := Ch;
      Inc(Index);
      Inc(Start);
    end;
    SetLength(Result, Len - Escape);
  end;
end;

function TJSONParser.GetChar: Char;
var
  Str: string;
begin
  Str := GetString;
  if Str = '' then
    Result := #0
  else
    Result := Str[1];
end;

procedure TJSONParser.GetStrings(List: TStrings);
begin
  if BeginArray then
  begin
    repeat
      if BeginObject then
      begin
        List.Add(GetKey);
        GetValue;
        Drop('}');
      end else begin
        List.Add(GetString);
      end;
    until EndArray;
  end;
end;

//function TJSONParser.GetStrings: TArray<string>;
//var
//  Len: Integer;
//begin
//  Result := nil;
//  Len := 0;
//  if BeginArray then
//  begin
//    repeat
//      SetLength(Result, Len + 1);
//      Result[Len] := GetString;
//      Inc(Len);
//    until EndArray;
//  end;
//end;

function TJSONParser.GetNumber: string;
// -0.5e+1.25
var
  Start: Integer;
  Loop : Integer;
begin
  Result := '';
  Start := FIndex;
  Loop := 0;
  repeat
    Inc(Loop);
    if not Skip('-') then
      Skip('+');
    while (FEof = False) and CharInSet(NextChar, ['0'..'9']) do
      ReadChar;
    if Skip('.') then
    begin
      while (FEof = False) and CharInSet(NextChar, ['0'..'9']) do
        ReadChar;
    end;
  until (FEof) or (Loop = 2) or (Skip('e') = False);
  Result := Copy(FText, Start, FIndex - Start);
end;

function TJSONParser.GetInteger: Integer;
begin
  Result := StrToInt(GetNumber);
end;

function TJSONParser.GetBoolean: Boolean;
begin
  if SkipStr(JSON_TRUE) then
    Exit(True);
  if SkipStr(JSON_FALSE) or SkipStr(JSON_NIL) then
    Exit(False);
  Result := GetInteger <> 0;
end;

function TJSONParser.GetFloat: Double;
begin
  Result := StrToFloat(GetNumber, JSONFormat);
end;

function TJSONParser.GetCurrency: Currency;
begin
  Result := StrToCurr(GetNumber, JSONFormat);
end;

function TJSONParser.GetDateTime: TDateTime;
// "2012-04-23T18:25:43"
var
  Str: string;
  y, m, d, h, s: Integer;
begin
  Result := 0;
  if SkipStr(JSON_NIL) then
    Exit;
  Str := GetString;

  // Time only
  if (Length(Str) = 8) and (Str[3] = ':') and (Str[6] = ':') then
  begin
    h := GetNum(Str, 1, 2);
    m := GetNum(Str, 4, 2);
    s := GetNum(Str, 7, 2);
    Result := EncodeTime(h, m, s, 0);
    Exit;
  end;

  // Date required
  if (Length(Str) < 10) or (Str[5] <> '-') or (Str[8] <> '-') then
    raise EJSonError.Create('Invalid date ' + Str);

  y := GetNum(Str, 1, 4);
  m := GetNum(Str, 6, 2);
  d := GetNum(Str, 9, 2);
  Result := EncodeDate(y, m, d);

  // DateTime ?
  if (Length(Str) > 10) then
  begin
    if (Length(Str) < 19) or (Str[11] <> 'T') or (Str[14] <> ':') or (Str[17] <> ':') then
      raise EJSONError.Create('Invalid date/time ' + Str);
    h := GetNum(Str, 12, 2);
    m := GetNum(Str, 15, 2);
    s := GetNum(Str, 18, 2);
    Result := Result + EncodeTime(h, m, s, 0);
  end;
end;

function TJSONParser.BeginObject: Boolean;
begin
  Result := Skip('{') and not Skip('}');
end;

function TJSONParser.EndObject: Boolean;
begin
  if Skip(',') then
    Exit(False);
  Drop('}');
  Result := True;
end;

function TJSONParser.BeginArray: Boolean;
begin
  Result := Skip('[') and not Skip(']');
end;

function TJSONParser.EndArray: Boolean;
begin
  if Skip(',') then
    Exit(False);
  Drop(']');
  Result := True;
end;

{ TClassFieldInfos }

function TClassFieldInfosHelper.IndexOf(Name: string): Integer;
begin
  Result := Length(Self) - 1;
  while Result >= 0 do
  begin
    if Self[Result].Name = Name then
      Exit;
    Dec(Result);
  end;
end;

{ TTypeInfoHelper }

function TTypeInfoHelper.DataPtr(Offset: Integer): Pointer;
var
  Value: PByte;
begin
  Value := PByte(TypeData);
  Inc(Value, Offset);
  Result := Value;
end;

function TTypeInfoHelper.DataByte(Offset: Integer): Byte;
begin
  Result := PByte(DataPtr(Offset))^;
end;

function TTypeInfoHelper.DataLong(Offset: Integer): Integer;
begin
  Result := PInteger(DataPtr(Offset))^;
end;

function TTypeInfoHelper.GetOrd(Instance: Pointer): Integer;
begin
  case TypeData.OrdType of
    otSByte: Result := PShortInt(Instance)^;
    otUByte: Result := PByte(Instance)^;
    otSWord: Result := PSmallInt(Instance)^;
    otUWord: Result := PWord(Instance)^;
    otSLong: Result := PInteger(Instance)^;
    otULong: Result := PCardinal(Instance)^;
  else
    raise EJSONError.Create('Unexpected ordinal type ?!');
  end;
end;

procedure TTypeInfoHelper.SetOrd(Instance: Pointer; Value: Integer);
begin
  case TypeData.OrdType of
    otSByte: PShortInt(Instance)^ := Value;
    otUByte: PByte(Instance)^ := Value;
    otSWord: PSmallInt(Instance)^ := Value;
    otUWord: PWord(Instance)^ := Value;
    otSLong: PInteger(Instance)^ := Value;
    otULong: PCardinal(Instance)^ := Value;
  end;
end;

function TTypeInfoHelper.RecordFieldsOfs: Integer;
var
  NumOps: Byte;
begin
  Result := 2 * SizeOf(Integer) + TypeData.ManagedFldCount * SizeOf(TManagedField);
  NumOps := DataByte(Result);
  Inc(Result, 1 + NumOps * SizeOf(Pointer));
end;

function TTypeInfoHelper.RecordFieldCount: Integer;
begin
  if Kind <> tkRecord then
    Exit(0);
  Result := DataLong(RecordFieldsOfs);
end;

function TTypeInfoHelper.RecordFieldType(Index: Integer): PRecordTypeField;
var
  Offset: Integer;
begin
  if (Kind <> tkRecord) or (Index < 0) then
    Exit(nil);
  Offset := RecordFieldsOfs;
  if DataLong(Offset) <= Index then
    Exit(nil);
  Inc(Offset, SizeOf(Integer));
  Result := DataPtr(Offset);
  while Index > 0 do
  begin
    Inc(Offset, Result.InstanceSize);
    Result := DataPtr(Offset);
    Dec(Index);
  end;
end;

function TTypeInfoHelper.RecordFieldByName(const Name: string): PRecordTypeField;
var
  Offset: Integer;
  Count : Integer;
begin
  if Kind <> tkRecord then
    Exit(nil);
  Offset := RecordFieldsOfs;
  Count := DataLong(Offset);
  Inc(Offset, SizeOf(Integer));
  Result := DataPtr(Offset);
  while Count > 0 do
  begin
    if string(Result.Name) = Name then
      Exit;
    Inc(Offset, Result.InstanceSize);
    Result := DataPtr(Offset);
    Dec(Count);
  end;
  Result := nil;
end;

function TTypeInfoHelper.DynArrayLength(Instance: Pointer): Integer;
begin
  Result := DynArraySize(PPointer(Instance)^);
end;

function TTypeInfoHelper.DynArrayElType: PTypeInfo;
begin
  Result := TypeData.elType2^;
end;

function TTypeInfoHelper.DynArrayElSize: Integer;
begin
  Result := TypeData.elSize;
end;

function TTypeInfoHelper.GetClassPropInfos: TClassPropInfos;
begin
  SetLength(Result, TypeData.PropCount);
  GetPropInfos(@Self, PPropList(Result));
end;

function TTypeInfoHelper.GetClassFieldInfos: TClassFieldInfos;
var
  TypeInfo: PTypeInfo;
begin
  Result := nil;
  MergeClassFieldInfos(Result);
  TypeInfo := @Self;
  while TypeInfo.TypeData.ParentInfo <> nil do
  begin
    TypeInfo := TypeInfo.TypeData.ParentInfo^;
    TypeInfo.MergeClassFieldInfos(Result);
  end;
end;

procedure TTypeInfoHelper.MergeClassFieldInfos(var Infos: TClassFieldInfos);
var
  Table: PVmtFieldTable;
  Prev : Integer;
  Count: Integer;
  CTab : PVmtFieldClassTab;
  Field: PVmtFieldEntry absolute Table;
  Attr : PAttrData absolute Table;
  Ptr  : PByte absolute Table;
  ExFld: PFieldExEntry absolute Table;
  ExCnt: Integer;
  Index: Integer;
  Dup  : Integer;
begin
  Table := PPointer(PByte(TypeData.ClassType) + vmtFieldTable)^;
  if Table = nil then
    Exit;

  Count := Table.Count;
  CTab   := Table.ClassTab;
  Inc(Table);

  // classic fields (?)
  Prev := Length(Infos);
  SetLength(Infos, Prev + Count);
  for Index := 0 to Count - 1 do
  begin
    Infos[Prev + Index].Name := string(Field.Name);
    Infos[Prev + Index].Offset := Field.FieldOffset;
    Infos[Prev + Index].TypeInfo := CTab.ClassRef[Field.TypeIndex].ClassInfo;
    Attr := PAttrData(Field.NameFld.Tail);
    Inc(Ptr, Attr.Len);
  end;

  // extended fields (?!)
  ExCnt := PWord(Ptr)^;
  Inc(Ptr, 2);

  for Index := 0 to ExCnt - 1 do
  begin
    Dup := Count - 1;
    while Dup >= 0 do
    begin
      if Infos[Prev + Dup].Offset = ExFld.Offset then
        Break;
      Dec(Dup);
    end;
    if Dup < 0 then
    begin
      Dup := Length(Infos);
      SetLength(Infos, dup + 1);
      Infos[Dup].Name := string(ExFld.Name);
      Infos[Dup].Offset := ExFld.Offset;
      if ExFld.TypeRef = nil then
      begin
        infos[Dup].TypeInfo := nil
      end else begin
        Infos[Dup].TypeInfo := ExFld.TypeRef^;
      end;
    end;
    Attr := ExFld.AttrData;
    Inc(Ptr, Attr.Len);
  end;
end;

function TTypeInfoHelper.GetMethodAddress(const Name: string): Pointer;
var
  TypeInfo: PTypeInfo;
begin
  Result := GetClassMethodAddress(Name);
  TypeInfo := @Self;
  while (Result = nil) and (TypeInfo.TypeData.ParentInfo <> nil) do
  begin
    TypeInfo := TypeInfo.TypeData.ParentInfo^;
    Result := TypeInfo.GetClassMethodAddress(Name);
  end;
end;

function TTypeInfoHelper.GetClassMethodAddress(const Name: string): Pointer;
var
  Table: PVmtMethodTable;
  Entry: PVmtMethodEntry absolute Table;
  ExCnt: PWord absolute Table;
  ExEnt: PVmtMethodExEntry absolute Table;
  Count: Integer;
  Index: Integer;
begin
  Result := nil;

  Table := PPointer(PByte(TypeData.ClassType) + vmtMethodTable)^;
  if Table = nil then
    Exit;

  Count := Table.Count;
  Inc(Table);

  for Index := 1 to Count do
  begin
    if string(Entry.Name) = Name then
    begin
      Result := Entry.CodeAddress;
      Exit;
    end;
    Inc(PByte(Entry), Entry.Len);
  end;

  Count := ExCnt^;
  Inc(ExCnt);

  for Index := 1 to Count do
  begin
    if string(ExEnt.Entry.Name) = Name then
    begin
      Result := ExEnt.Entry.CodeAddress;
      Exit;
    end;
    Inc(ExEnt);
  end;
end;

function TTypeInfoHelper.toJSON(Instance: Pointer): string;
var
  Builder: TJSONBuilder;
begin
  Builder := TJSONBuilder.Create;
  try
    BuildJSON(Instance, Builder);
    Result := Builder.ToString;
  finally
    Builder.Free;
  end;
end;

procedure TTypeInfoHelper.BuildJSON(Instance: Pointer; Builder: TJSONBuilder);
begin
  case Kind of
    tkRecord  : BuildJSONRecord(Instance, Builder);
    tkDynArray: BuildJSONDynArray(PPointer(Instance)^, Builder);
    tkArray   : BuildJSONArray(Instance, Builder);
    tkSet     : BuildJSONSet(Instance, Builder);
    tkInteger : Builder.Append(IntToStr(GetOrd(Instance)));
    tkChar    : Builder.AppendString(string(AnsiChar(Instance^)));
    tkWChar   : Builder.AppendString(Char(Instance^));
    tkString  : Builder.AppendString(string(ShortString(Instance^)));
    tkClass   :
    begin
//      BuildJSONClass(PPointer(Instance)^, Builder);
      if PPointer(Instance)^= nil then
        Builder.Append(JSON_NIL)
      else
        PTypeInfo(TObject(Instance^).ClassInfo).BuildJSONClass(PPointer(Instance^), Builder);
    end;
    tkInt64   :
    begin
      if TypeData.MinInt64Value = 0 then
        Builder.Append(UIntToStr(UInt64(Instance^)))
      else
        Builder.Append(IntToStr(Int64(Instance^)));
    end;
    tkLString :
    begin
      if PPointer(Instance^) = nil then
        Builder.Append(JSON_NIL)
      else
        Builder.AppendString(string(AnsiString(Instance^)));
    end;
    tkUString :
    begin
      if PPointer(Instance^) = nil then
        Builder.Append(JSON_NIL)
      else
        Builder.AppendString(string(Instance^));
    end;
    tkEnumeration:
    begin
      if @Self = TypeInfo(Boolean) then
        Builder.Append(JSON_BOOL[GetOrd(Instance) <> 0])
      else
        Builder.Append(JSONEscape(GetEnumName(@Self, GetOrd(Instance))));
    end;
    tkFloat:
    begin
      if @Self = TypeInfo(TDate) then
        Builder.Append(JSONEncodeDate(TDate(Instance^)))
      else
      if @Self = TypeInfo(TTime) then
        Builder.Append(JSONEncodeTime(TTime(Instance^)))
      else
      if @Self = TypeInfo(TDateTime) then
        Builder.Append(JSONEncodeDateTime(TDateTime(Instance^)))
      else
        case TypeData.FloatType of
          ftSingle   : Builder.Append(JSONEncodeFloat(Single(Instance^)));
          ftDouble   : Builder.Append(JSONEncodeFloat(Double(Instance^)));
          ftExtended : Builder.Append(JSONEncodeFloat(Extended(Instance^)));
          ftComp     : Builder.Append(JSONEncodeFloat(Comp(Instance^)));
          ftCurr     : Builder.Append(JSONEncodeFloat(Currency(Instance^)));
        end;
    end;
//    tkUnknown: ;
//    tkEnumeration: ;
//    tkMethod: ;
//    tkWString: ;
//    tkVariant: ;
//    tkInterface: ;
//    tkClassRef: ;
//    tkPointer: ;
//    tkProcedure: ;
  else
    Builder.Append('"' + Name + '"');
  end;
end;

procedure TTypeInfoHelper.BuildJSONClass(Instance: Pointer; Builder: TJSONBuilder);
var
  Fields: TClassFieldInfos;
  Count : Integer;
  Index : Integer;
  Chain : TBuildChain;
begin
  if Instance = nil then
  begin
    Builder.Append(JSON_NIL);
    Exit;
  end;

  if @Self = TStringList.ClassInfo then
  begin
    Builder.AppendStrings(TStringList(Instance));
    Exit;
  end;

  if Builder.KnowInstance(Instance) then
    raise EJSONError.Create('Circular reference in ' + Builder.GetPath);

  Chain.Next := Builder.FBuild;
  Chain.Path := '';
  Chain.Build := GetMethodAddress('JSONBuildField');
  Chain.Sender := Instance;
  Builder.FBuild := @Chain;

  Builder.Append('{');

  Fields := GetClassFieldInfos;
  Count := Length(Fields);

  for Index := 0 to Count - 1 do
  begin
    if Index > 0 then
      Builder.Append(',');
    Builder.Append('"');
    Builder.Append(Fields[Index].Name);
    Builder.Append('":');
    if Builder.BuildField(Fields[Index].Name) = False then
    begin
      if Fields[Index].TypeInfo = nil then
        raise EJSONError.Create('No RTTI informations for ' + Builder.GetPath);
      Fields[index].TypeInfo.BuildJSON(PByte(Instance) + Fields[Index].Offset, Builder);
    end;
  end;

  Builder.Append('}');

  Builder.FBuild := Chain.Next;
end;

procedure TTypeInfoHelper.BuildJSONRecord(Instance: Pointer; Builder: TJSONBuilder);
var
  Count: Integer;
  Index: Integer;
  Field: PRecordTypeField;
  Chain: TBuildChain;
begin
  Chain.Build := nil;
  Chain.Sender := nil;
  Chain.Path := '';
  Chain.Next := Builder.FBuild;
  Builder.FBuild := @Chain;

  Builder.Append('{');
  Count := RecordFieldCount;
  for Index := 0 to Count - 1 do
  begin
    if Index > 0 then
      Builder.Append(',');
    Field := RecordFieldType(Index);
    Builder.Append('"');
    Builder.Append(Field.Name);
    Builder.Append('":');
    if Builder.BuildField(string(Field.Name)) = False then
    begin
      Field.Field.TypeRef^.BuildJSON(Field.GetInstance(Instance), Builder);
    end;
  end;
  Builder.Append('}');

  Builder.FBuild := Chain.Next;
end;

procedure TTypeInfoHelper.BuildJSONDynArray(Instance: Pointer; Builder: TJSONBuilder);
var
  Len  : Integer;
  Index: Integer;
begin
  Builder.Append('[');
  Len := DynArraySize(Instance);
  for Index := 0 to Len - 1 do
  begin
    if Index > 0 then
      Builder.Append(',');
    DynArrayElType.BuildJSON(Instance, Builder);
    Inc(PByte(Instance), DynArrayElSize);
  end;
  Builder.Append(']');
end;

procedure TTypeInfoHelper.BuildJSONArray(Instance: Pointer; Builder: TJSONBuilder);
var
  Len  : Integer;
  Index: Integer;
  Step : Integer;
begin
  Builder.Append('[');
  Len := TypeData.ArrayData.ElCount;
  Step := TypeData.ArrayData.Size div Len;
  // DimCount can be > 1 with Dims[] all null (no RTTI info)
  for Index := 0 to Len - 1 do
  begin
    if Index > 0 then
      Builder.Append(',');
    TypeData.ArrayData.ElType^.BuildJSON(Instance, Builder);
    Inc(PByte(Instance), Step);
  end;
  Builder.Append(']');
end;

procedure TTypeInfoHelper.BuildJSONSet(Instance: Pointer; Builder: TJSONBuilder);
var
  Value: Integer;
  Index: Integer;
  Count: Integer;
begin
  Value := GetOrd(Instance);
  Index := 0;
  Count := 0;
  Builder.Append('[');
  while Value > 0 do
  begin
    if Value and 1 > 0 then
    begin
      if Count > 0 then
        Builder.Append(',');
       Builder.AppendString(GetEnumName(TypeData.CompType^, Index));
       Inc(Count);
    end;
    Value := Value shr 1;
    Inc(Index);
  end;
  Builder.Append(']');
end;

procedure TTypeInfoHelper.FromJSON(Instance: Pointer; const JSON: string);
var
  Parser: TJSONParser;
begin
  Parser := TJSONParser.Create(JSON);
  try
    ParseJSON(Instance, Parser);
  finally
    Parser.Free;
  end;
end;

procedure TTypeInfoHelper.ParseJSON(Instance: Pointer; Parser: TJSONParser);
begin
  case Kind of
    tkClass   : ParseJSONClass(PPointer(Instance)^, Parser);
    tkRecord  : ParseJSONRecord(Instance, Parser);
    tkDynArray: ParseJSONDynArray(Instance, Parser);
    tkArray   : ParseJSONArray(Instance, Parser);
    tkSet     : ParseJSONSet(Instance, Parser);
    tkString  : ParseJSONShortString(Instance, Parser);
    tkInteger : SetOrd(Instance, Parser.GetInteger);
    tkChar    : AnsiChar(Instance^) := AnsiChar(Parser.GetChar);
    tkWChar   : Char(Instance^) := Parser.GetChar;
    tkLString : AnsiString(Instance^) := AnsiString(Parser.GetString);
    tkUString : string(Instance^)  := Parser.GetString;
    tkInt64   :
    begin
      if TypeData.MinInt64Value = 0 then
        UInt64(Instance^) := Uint64(StrToInt64(Parser.GetNumber))
      else
        Int64(Instance^) := StrToInt64(Parser.GetNumber);
    end;
    tkEnumeration:
    begin
      if @Self = TypeInfo(Boolean) then
      begin
        SetOrd(Instance, Ord(Parser.GetBoolean));
      end else begin
        SetOrd(Instance, GetEnumValue(@Self, Parser.GetString));
      end;
    end;
    tkFloat:
    begin
      if @Self = TypeInfo(TDate) then
        TDate(Instance^) := DateOf(Parser.GetDateTime)
      else
      if @Self = TypeInfo(TTime) then
        TTime(Instance^) := TimeOf(PArser.GetDateTime)
      else
      if @Self = TypeInfo(TDateTime) then
        TDateTime(Instance^) := Parser.GetDateTime
      else
        case TypeData.FloatType of
          ftSingle   : Single(Instance^) := Parser.GetFloat;
          ftDouble   : Double(Instance^) := Parser.GetFloat;
          ftExtended : Extended(Instance^) := Parser.GetFloat;
          ftComp     : Comp(Instance^) := Parser.GetFloat;
          ftCurr     : Currency(Instance^) := Parser.GetCurrency;
        end;

    end;
//    tkUnknown: ;
//    tkMethod: ;
//    tkWString: ;
//    tkVariant: ;
//    tkInterface: ;
//    tkClassRef: ;
//    tkPointer: ;
//    tkProcedure: ;
  end;
end;

procedure TTypeInfoHelper.ParseJSONClass(Instance: Pointer; Parser: TJSONParser);
var
  Fields: TClassFieldInfos;
  Key   : string;
  Index : Integer;
  Chain : TParseChain;
begin
  if Instance = nil then
    Parser.GetValue
  else begin

    if @Self = TStringList.ClassInfo then
    begin
      Parser.GetStrings(TStringList(Instance));
      Exit;
    end;

    if Parser.BeginObject then
    begin
      Chain.Next := Parser.FParse;
      Chain.Path := '';
      Chain.Parse := GetMethodAddress('JSONParseField');
      Chain.Sender := Instance;
      Parser.FParse := @Chain;
      Fields := GetClassFieldInfos;
      repeat
        Key := Parser.GetKey;
        if Parser.ParseField(Key) = False then
        begin
          Index := Fields.IndexOf(Key);
          if (Index < 0) or (Instance = nil) then
            Parser.GetValue
          else begin
            Fields[Index].TypeInfo.ParseJSON(PByte(instance) + Fields[Index].Offset, Parser);
          end;
        end;
      until Parser.EndObject;
      Parser.FParse := Chain.Next;
    end;
  end;
end;

procedure TTypeInfoHelper.ParseJSONRecord(Instance: Pointer; Parser: TJSONParser);
var
  Key  : string;
  Field: PRecordTypeField;
  Chain: TParseChain;
begin
  if Parser.BeginObject then
  begin
    Chain.Parse := nil;
    Chain.Sender := nil;
    Chain.Path := '';
    Chain.Next := Parser.FParse;
    Parser.FParse := @Chain;
    repeat
      Key := Parser.GetKey;
      if Parser.ParseField(Key) = False then
      begin
        Field := RecordFieldByName(Key);
        if Field = nil then
          Parser.GetValue
        else begin
          Field.Field.TypeRef^.ParseJSON(Field.GetInstance(Instance), Parser);
        end;
      end;
    until Parser.EndObject;
    Parser.FParse := Chain.Next;
  end;
end;

procedure TTypeInfoHelper.ParseJSONDynArray(Instance: Pointer; Parser: TJSONParser);
var
  Item : PByte;
  Len  : Integer;
begin
  if Parser.BeginArray then
  begin
    Len := 0;
    repeat
      Inc(Len);
      DynArraySetLength(PPointer(Instance)^, @Self, 1, @Len);
      Item := PPointer(Instance)^;
      Inc(Item, (Len - 1) * DynArrayElSize);
      DynArrayElType.ParseJSON(Item, Parser);
    until Parser.EndArray;
  end;
end;

procedure TTypeInfoHelper.ParseJSONArray(Instance: Pointer; Parser: TJSONParser);
var
  Len  : Integer;
  Step : Integer;
begin
  Len := TypeData.ArrayData.ElCount;
  Step := TypeData.ArrayData.Size div Len;
  if Parser.BeginArray then
  begin
    repeat
      if Len <= 0 then
        Parser.GetValue
      else begin
        TypeData.ArrayData.ElType^.ParseJSON(Instance, Parser);
        Inc(PByte(Instance), Step);
        Dec(Len);
      end;
    until Parser.EndArray;
  end;
end;

procedure TTypeInfoHelper.ParseJSONSet(Instance: Pointer; Parser: TJSONParser);
var
  Value: Integer;
  Index: Integer;
begin
  Value := 0;
  if Parser.BeginArray then
  begin
    repeat
      Index := GetEnumValue(TypeData.CompType^, Parser.GetString);
      Value := Value or (1 shl Index);
    until Parser.EndArray;
  end;
  SetOrd(Instance, Value);
end;

procedure TTypeInfoHelper.ParseJSONShortString(Instance: Pointer; Parser: TJSONParser);
var
  Str: AnsiString;
  Len: Integer;
begin
  Str := AnsiString(Parser.GetString);
  Len := Length(Str);
  if Len > TypeData.MaxLength then
    Len := TypeData.MaxLength;
  ShortString(Instance^)[0] := AnsiChar(Len);
  if Len > 0 then
    Move(Str[1], ShortString(Instance^)[1], Len);
end;

{ TRecordTypeFieldHelper }

function TRecordTypeFieldHelper.GetInstance(RecordInstance: Pointer): Pointer;
begin
  Result := RecordInstance;
  Inc(PByte(Result), Field.FldOffset);
end;

function TRecordTypeFieldHelper.InstanceSize: Integer;
begin
  Result := SizeOf(TRecordTypeField) - 255 + Length(Name) + AttrData.Len;
end;

{ TObjectHelper }

function TObjectHelper.toJSON: string;
begin
  Result := PTypeInfo(ClassType.ClassInfo).toJSON(@Self);
end;

procedure TObjectHelper.fromJSON(const JSON: string);
begin
  PTypeInfo(ClassType.ClassInfo).FromJSON(@Self, JSON);
end;

{ TJSONBuilder }

procedure TJSONBuilder.AppendString(const Str: string);
begin
  Append(JSONEscape(Str));
end;

procedure TJSONBuilder.AppendStrings(List: TStrings);
var
  Index: Integer;
begin
  Append('[');
  if List <> nil then
    for Index := 0 to List.Count - 1 do
    begin
      if Index > 0 then
        Append(',');
      if List.Objects[Index] = nil then
        AppendString(List[Index])
      else begin
        Append('{');
        AppendString(List[Index]);
        Append(':');
        AppendObject(List.Objects[Index]);
        Append('}');
      end;
    end;
  Append(']');
end;

procedure TJSONBuilder.AppendObject(Obj: TObject);
begin
  PTypeInfo(Obj.ClassType.classInfo).BuildJSONClass(Obj, Self);
end;

function TJSONBuilder.KnowInstance(Instance: Pointer): Boolean;
var
  Chain: PBuildChain;
begin
  Result := False;
  if FBuild = nil then
    Exit;
  Chain := FBuild;
  repeat
    if Chain.Sender = Instance then
      Result := True
    else begin
      Chain := Chain.Next;
    end;
  until Result or (Chain = nil);
end;

function TJSONBuilder.BuildField(Name: string): Boolean;
var
  Chain: PBuildChain;
begin
  if FBuild = nil then
    Exit(False);
  Chain := FBuild;
  repeat
    if Chain = FBuild then
      FBuild.Path := Name + '.'
    else
      Name := Chain.Path + Name;
    Result := (@Chain.Build <> nil) and Chain.Build(Chain.Sender, Name, Self);
    Chain := Chain.Next;
  until Result or (Chain = nil);
end;

function TJSONBuilder.GetPath: string;
var
  Chain: PBuildChain;
begin
  Result := '';
  Chain := FBuild;
  while Chain <> nil do
  begin
    Result := Chain.Path + Result;
    if Chain.Sender <> nil then
      Result := Chain.Sender.ClassName + '.' + Result;
    Chain := Chain.Next;
  end;
end;

initialization
{$WARN SYMBOL_DEPRECATED OFF}
{$WARN SYMBOL_PLATFORM OFF}
  GetLocaleFormatSettings(1033{en-US}, JSONFormat);
{$WARN SYMBOL_PLATFORM ON}
{$WARN SYMBOL_DEPRECATED ON}
end.
