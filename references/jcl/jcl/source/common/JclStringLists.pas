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
{ The Original Code is NewStringListUnit.pas.                                                      }
{                                                                                                  }
{ The Initial Developer of the Original Code is Romullo Sousa.                                     }
{ Portions created by Romullo Sousa are Copyright (C) Romullo Sousa. All rights reserved.          }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{     Romullo Sousa (romullobr)                                                                    }
{     Leo Simas (Leh_U)                                                                            }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ This unit contains several improvements of the standard TStringList.                             }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclStringLists;

{$I jcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF HAS_UNITSCOPE}
  {$IFDEF MSWINDOWS}
  Winapi.Windows,
  {$ENDIF MSWINDOWS}
  System.Variants,
  System.Classes, System.SysUtils,
  {$ELSE ~HAS_UNITSCOPE}
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  Variants,
  Classes, SysUtils,
  {$ENDIF ~HAS_UNITSCOPE}
  JclBase,
  JclPCRE;

{$DEFINE HAS_TSTRINGS_COMPARESTRINGS}
{$IFDEF FPC}
 {$UNDEF HAS_TSTRINGS_COMPARESTRINGS}
{$ENDIF FPC}

type
  EJclStringListError = class(EJclError);

  IJclStringList = interface;

  TJclStringListObjectsMode = (omNone, omObjects, omVariants, omInterfaces);

  TJclStringListSortCompare = function(List: IJclStringList; Index1, Index2: Integer): Integer;

  IJclStringList = interface(IInterface)
    ['{8DC5B71C-4756-404D-8636-7872CD299796}']
    { From TStrings/TStringList }
    function Add(const S: string): Integer; overload;
    function AddObject(const S: string; AObject: TObject): Integer;
    function Get(Index: Integer): string;
    function GetCapacity: Integer;
    function GetCount: Integer;
    function GetObjects(Index: Integer): TObject;
    function GetTextStr: string;
    function GetValue(const Name: string): string;
    {$IFDEF FPC}
    function Find(const S: string; out Index: Integer): Boolean;
    {$ELSE ~FPC}
    function Find(const S: string; var Index: Integer): Boolean;
    {$ENDIF ~FPC}
    function IndexOf(const S: string): Integer;
    function GetCaseSensitive: Boolean;
    function GetDuplicates: TDuplicates;
    function GetOnChange: TNotifyEvent;
    function GetOnChanging: TNotifyEvent;
    function GetSorted: Boolean;
    function Equals(Strings: TStrings): Boolean;
    function IndexOfName(const Name: string): Integer;
    function IndexOfObject(AObject: TObject): Integer; 
    function LoadFromFile(const FileName: string): IJclStringList;
    function LoadFromStream(Stream: TStream): IJclStringList;
    function SaveToFile(const FileName: string): IJclStringList;
    function SaveToStream(Stream: TStream): IJclStringList;
    function GetCommaText: string;
    function GetDelimitedText: string;
    function GetDelimiter: Char;
    function GetName(Index: Integer): string;
    {$IFDEF COMPILER7_UP}
    function GetNameValueSeparator: Char;
    function GetValueFromIndex(Index: Integer): string;
    {$ENDIF COMPILER7_UP}
    function GetQuoteChar: Char;
    procedure SetCommaText(const Value: string);
    procedure SetDelimitedText(const Value: string);
    procedure SetDelimiter(const Value: Char);
    {$IFDEF COMPILER7_UP}
    procedure SetNameValueSeparator(const Value: Char);
    procedure SetValueFromIndex(Index: Integer; const Value: string);
    {$ENDIF COMPILER7_UP}
    procedure SetQuoteChar(const Value: Char);
    procedure AddStrings(Strings: TStrings); overload;
    procedure SetObjects(Index: Integer; const Value: TObject);
    procedure Put(Index: Integer; const S: string);
    procedure SetCapacity(NewCapacity: Integer);
    procedure SetTextStr(const Value: string);
    procedure SetValue(const Name, Value: string);
    procedure SetCaseSensitive(const Value: Boolean);
    procedure SetDuplicates(const Value: TDuplicates);
    procedure SetOnChange(const Value: TNotifyEvent);
    procedure SetOnChanging(const Value: TNotifyEvent);
    procedure SetSorted(const Value: Boolean);
    property Count: Integer read GetCount;
    property Strings[Index: Integer]: string read Get write Put; default;
    property Text: string read GetTextStr write SetTextStr;
    property Objects[Index: Integer]: TObject read GetObjects write SetObjects;
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Values[const Name: string]: string read GetValue write SetValue;
    property Duplicates: TDuplicates read GetDuplicates write SetDuplicates;
    property Sorted: Boolean read GetSorted write SetSorted;
    property CaseSensitive: Boolean read GetCaseSensitive write SetCaseSensitive;
    property OnChange: TNotifyEvent read GetOnChange write SetOnChange;
    property OnChanging: TNotifyEvent read GetOnChanging write SetOnChanging;
    property DelimitedText: string read GetDelimitedText write SetDelimitedText;
    property Delimiter: Char read GetDelimiter write SetDelimiter;
    property Names[Index: Integer]: string read GetName;
    property QuoteChar: Char read GetQuoteChar write SetQuoteChar;
    property CommaText: string read GetCommaText write SetCommaText;
    {$IFDEF COMPILER7_UP}
    property ValueFromIndex[Index: Integer]: string read GetValueFromIndex write SetValueFromIndex;
    property NameValueSeparator: Char read GetNameValueSeparator write SetNameValueSeparator;
    {$ENDIF COMPILER7_UP}
    { New }
    function Assign(Source: TPersistent): IJclStringList;
    function LoadExeParams: IJclStringList;
    function Exists(const S: string): Boolean;
    function ExistsName(const S: string): Boolean;
    function DeleteBlanks: IJclStringList;
    function KeepIntegers: IJclStringList;
    function DeleteIntegers: IJclStringList;
    function ReleaseInterfaces: IJclStringList;
    function FreeObjects(AFreeAndNil: Boolean = False): IJclStringList;
    function Clone: IJclStringList;
    function Insert(Index: Integer; const S: string): IJclStringList;
    function InsertObject(Index: Integer; const S: string; AObject: TObject): IJclStringList;
    function Sort(ACompareFunction: TJclStringListSortCompare = nil): IJclStringList;
    function SortAsInteger: IJclStringList;
    function SortByName: IJclStringList;
    function Delete(AIndex: Integer): IJclStringList; overload;
    function Delete(const AString: string): IJclStringList; overload;
    function Exchange(Index1, Index2: Integer): IJclStringList;
    function Add(const A: array of const): IJclStringList; overload;
    function AddStrings(const A: array of string): IJclStringList; overload;
    function BeginUpdate: IJclStringList;
    function EndUpdate: IJclStringList;
    function Trim: IJclStringList;
    function Join(const ASeparator: string = ''): string;
    function Split(const AText, ASeparator: string; AClearBeforeAdd: Boolean = True): IJclStringList;
    function ExtractWords(const AText: string; const ADelims: TSetOfAnsiChar = [#0..' ']; AClearBeforeAdd: Boolean = True): IJclStringList;
    function Last: string;
    function First: string;
    function LastIndex: Integer;
    function Clear: IJclStringList;
    {$IFDEF JCL_PCRE}
    function DeleteRegEx(const APattern: string): IJclStringList;
    function KeepRegEx(const APattern: string): IJclStringList;
    function Files(const APattern: string = '*'; ARecursive: Boolean = False; const ARegExPattern: string = ''): IJclStringList;
    function Directories(const APattern: string = '*'; ARecursive: Boolean = False; const ARegExPattern: string = ''): IJclStringList;
    {$ENDIF JCL_PCRE}
    function GetStringsRef: TStrings;
    function ConfigAsSet: IJclStringList;
    function Delimit(const ADelimiter: string): IJclStringList;
    function GetInterfaceByIndex(Index: Integer): IInterface;
    function GetLists(Index: Integer): IJclStringList;
    function GetVariants(AIndex: Integer): Variant;
    function GetKeyInterface(const AKey: string): IInterface;
    function GetKeyObject(const AKey: string): TObject;
    function GetKeyVariant(const AKey: string): Variant;
    function GetKeyList(const AKey: string): IJclStringList;
    function GetObjectsMode: TJclStringListObjectsMode;
    procedure SetInterfaceByIndex(Index: Integer; const Value: IInterface);
    procedure SetLists(Index: Integer; const Value: IJclStringList);
    procedure SetVariants(Index: Integer; const Value: Variant);
    procedure SetKeyInterface(const AKey: string; const Value: IInterface);
    procedure SetKeyObject(const AKey: string; const Value: TObject);
    procedure SetKeyVariant(const AKey: string; const Value: Variant);
    procedure SetKeyList(const AKey: string; const Value: IJclStringList);
    property Interfaces[Index: Integer]: IInterface read GetInterfaceByIndex write SetInterfaceByIndex;
    property Lists[Index: Integer]: IJclStringList read GetLists write SetLists;
    property Variants[Index: Integer]: Variant read GetVariants write SetVariants;
    property KeyList[const AKey: string]: IJclStringList read GetKeyList write SetKeyList;
    property KeyObject[const AKey: string]: TObject read GetKeyObject write SetKeyObject;
    property KeyInterface[const AKey: string]: IInterface read GetKeyInterface write SetKeyInterface;
    property KeyVariant[const AKey: string]: Variant read GetKeyVariant write SetKeyVariant;
    property ObjectsMode: TJclStringListObjectsMode read GetObjectsMode;
    {$IFDEF SUPPORTS_FOR_IN}
    function GetEnumerator: TStringsEnumerator;
    {$ENDIF SUPPORTS_FOR_IN}
  end;

type
  TJclInterfacedStringList = class(TStringList, IInterface)
   private
    FOwnerInterface: IInterface;
  public
    { IInterface }
     function _AddRef: Integer; stdcall;
     function _Release: Integer; stdcall;
     function QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} IID: TGUID; out Obj): HResult;  stdcall;
     procedure AfterConstruction; override;
  end;


  TJclStringList = class(TJclInterfacedStringList, IInterface, IJclStringList)
  private
    FObjectsMode: TJclStringListObjectsMode;
    FSelfAsInterface: IJclStringList;
    {$IFDEF JCL_PCRE}
    FLastRegExPattern: string;
    FRegEx: TJclRegEx;
    {$ENDIF JCL_PCRE}
    FCompareFunction: TJclStringListSortCompare;
    function CanFreeObjects: Boolean;
    {$IFDEF JCL_PCRE}
    function MatchRegEx(const S, APattern: string): Boolean;
    {$ENDIF JCL_PCRE}
    procedure EnsureObjectsMode(AMode: TJclStringListObjectsMode);
  protected
    FRefCount: Integer;
    {$IFNDEF HAS_TSTRINGS_COMPARESTRINGS}
    function CompareStrings(const S1, S2: string): Integer; virtual;
    {$ENDIF ~HAS_TSTRINGS_COMPARESTRINGS}
  public
    constructor Create;
    destructor Destroy; override;
    { IInterface }
    // function QueryInterface(const IID: TGUID; out Obj): HRESULT; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    { IJclStringList }
    // function Add(const S: string): Integer; overload;
    // function AddObject(const S: string; AObject: TObject): Integer;
    // function Get(Index: Integer): string;
    // function GetCapacity: Integer;
    // function GetCount: Integer;
    function GetObjects(Index: Integer): TObject;
    // function GetTextStr: string;
    function GetValue(const Name: string): string;
    // function Find(const S: string; var Index: Integer): Boolean;
    // function IndexOf(const S: string): Integer;
    function GetCaseSensitive: Boolean;
    function GetDuplicates: TDuplicates;
    function GetOnChange: TNotifyEvent;
    function GetOnChanging: TNotifyEvent;
    function GetSorted: Boolean;
    // function Equals(Strings: TStrings): Boolean;
    // function IndexOfName(const Name: string): Integer;
    // function IndexOfObject(AObject: TObject): Integer;
    function LoadFromFile(const FileName: string): IJclStringList; reintroduce;
    function LoadFromStream(Stream: TStream): IJclStringList; reintroduce;
    function SaveToFile(const FileName: string): IJclStringList; reintroduce;
    function SaveToStream(Stream: TStream): IJclStringList; reintroduce;
    function GetCommaText: string;
    function GetDelimitedText: string;
    function GetDelimiter: Char;
    function GetName(Index: Integer): string;
    {$IFDEF COMPILER7_UP}
    function GetNameValueSeparator: Char;
    function GetValueFromIndex(Index: Integer): string;
    {$ENDIF COMPILER7_UP}
    function GetQuoteChar: Char;
    procedure SetCommaText(const Value: string);
    procedure SetDelimitedText(const Value: string);
    procedure SetDelimiter(const Value: Char);
    {$IFDEF COMPILER7_UP}
    procedure SetNameValueSeparator(const Value: Char);
    procedure SetValueFromIndex(Index: Integer; const Value: string);
    {$ENDIF COMPILER7_UP}
    procedure SetQuoteChar(const Value: Char);
    // procedure AddStrings(Strings: TStrings); overload;
    procedure SetObjects(Index: Integer; const Value: TObject);
    // procedure Put(Index: Integer; const S: string);
    // procedure SetCapacity(NewCapacity: Integer);
    // procedure SetTextStr(const Value: string);
    procedure SetValue(const Name, Value: string);
    procedure SetCaseSensitive(const Value: Boolean);
    procedure SetDuplicates(const Value: TDuplicates);
    procedure SetOnChange(const Value: TNotifyEvent);
    procedure SetOnChanging(const Value: TNotifyEvent);
    procedure SetSorted(const Value: Boolean);
    property Count: Integer read GetCount;
    property Strings[Index: Integer]: string read Get write Put; default;
    property Text: string read GetTextStr write SetTextStr;
    property Objects[Index: Integer]: TObject read GetObjects write SetObjects;
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Values[const Name: string]: string read GetValue write SetValue;
    property Duplicates: TDuplicates read GetDuplicates write SetDuplicates;
    property Sorted: Boolean read GetSorted write SetSorted;
    property CaseSensitive: Boolean read GetCaseSensitive write SetCaseSensitive;
    property OnChange: TNotifyEvent read GetOnChange write SetOnChange;
    property OnChanging: TNotifyEvent read GetOnChanging write SetOnChanging;
    property DelimitedText: string read GetDelimitedText write SetDelimitedText;
    property Delimiter: Char read GetDelimiter write SetDelimiter;
    property Names[Index: Integer]: string read GetName;
    property QuoteChar: Char read GetQuoteChar write SetQuoteChar;
    property CommaText: string read GetCommaText write SetCommaText;
    {$IFDEF COMPILER7_UP}
    property ValueFromIndex[Index: Integer]: string read GetValueFromIndex write SetValueFromIndex;
    property NameValueSeparator: Char read GetNameValueSeparator write SetNameValueSeparator;
    {$ENDIF COMPILER7_UP}
    { New }
    function Assign(Source: TPersistent): IJclStringList; reintroduce;
    function LoadExeParams: IJclStringList;
    function Exists(const S: string): Boolean;
    function ExistsName(const S: string): Boolean;
    function DeleteBlanks: IJclStringList;
    function KeepIntegers: IJclStringList;
    function DeleteIntegers: IJclStringList;
    function ReleaseInterfaces: IJclStringList;
    function FreeObjects(AFreeAndNil: Boolean = False): IJclStringList;
    function Clone: IJclStringList;
    function Insert(Index: Integer; const S: string): IJclStringList; reintroduce;
    function InsertObject(Index: Integer; const S: string; AObject: TObject): IJclStringList; reintroduce;
    function Sort(ACompareFunction: TJclStringListSortCompare = nil): IJclStringList; reintroduce;
    function SortAsInteger: IJclStringList;
    function SortByName: IJclStringList;
    function Delete(AIndex: Integer): IJclStringList; reintroduce; overload;
    function Delete(const AString: string): IJclStringList; reintroduce; overload;
    function Exchange(Index1, Index2: Integer): IJclStringList; reintroduce;
    function Add(const A: array of const): IJclStringList; reintroduce; overload;
    function AddStrings(const A: array of string): IJclStringList; reintroduce; overload;
    function BeginUpdate: IJclStringList;
    function EndUpdate: IJclStringList;
    function Trim: IJclStringList;
    function Join(const ASeparator: string = ''): string;
    function Split(const AText, ASeparator: string; AClearBeforeAdd: Boolean = True): IJclStringList;
    function ExtractWords(const AText: string; const ADelims: TSetOfAnsiChar = [#0..' ']; AClearBeforeAdd: Boolean = True): IJclStringList;
    function Last: string;
    function First: string;
    function LastIndex: Integer;
    function Clear: IJclStringList; reintroduce;
    {$IFDEF JCL_PCRE}
    function DeleteRegEx(const APattern: string): IJclStringList;
    function KeepRegEx(const APattern: string): IJclStringList;
    function Files(const APattern: string = '*'; ARecursive: Boolean = False; const ARegExPattern: string = ''): IJclStringList;
    function Directories(const APattern: string = '*'; ARecursive: Boolean = False; const ARegExPattern: string = ''): IJclStringList;
    {$ENDIF JCL_PCRE}
    function GetStringsRef: TStrings;
    function ConfigAsSet: IJclStringList;
    function Delimit(const ADelimiter: string): IJclStringList;
    function GetInterfaceByIndex(Index: Integer): IInterface;
    function GetLists(Index: Integer): IJclStringList;
    function GetVariants(AIndex: Integer): Variant;
    function GetKeyInterface(const AKey: string): IInterface;
    function GetKeyObject(const AKey: string): TObject;
    function GetKeyVariant(const AKey: string): Variant;
    function GetKeyList(const AKey: string): IJclStringList;
    function GetObjectsMode: TJclStringListObjectsMode;
    procedure SetInterfaceByIndex(Index: Integer; const Value: IInterface);
    procedure SetLists(Index: Integer; const Value: IJclStringList);
    procedure SetVariants(Index: Integer; const Value: Variant);
    procedure SetKeyInterface(const AKey: string; const Value: IInterface);
    procedure SetKeyObject(const AKey: string; const Value: TObject);
    procedure SetKeyVariant(const AKey: string; const Value: Variant);
    procedure SetKeyList(const AKey: string; const Value: IJclStringList);
    property Interfaces[Index: Integer]: IInterface read GetInterfaceByIndex write SetInterfaceByIndex;
    property Lists[Index: Integer]: IJclStringList read GetLists write SetLists;
    property Variants[Index: Integer]: Variant read GetVariants write SetVariants;
    property KeyList[const AKey: string]: IJclStringList read GetKeyList write SetKeyList;
    property KeyObject[const AKey: string]: TObject read GetKeyObject write SetKeyObject;
    property KeyInterface[const AKey: string]: IInterface read GetKeyInterface write SetKeyInterface;
    property KeyVariant[const AKey: string]: Variant read GetKeyVariant write SetKeyVariant;
    property ObjectsMode: TJclStringListObjectsMode read GetObjectsMode;
  end;

function JclStringList: IJclStringList; overload;
function JclStringListStrings(AStrings: TStrings): IJclStringList; overload;
function JclStringListStrings(const A: array of string): IJclStringList; overload;
function JclStringList(const A: array of const): IJclStringList; overload;
function JclStringList(const AText: string): IJclStringList; overload;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JCL\source\common';
    Extra: '';
    Data: nil
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  {$IFDEF HAS_UNITSCOPE}
  System.TypInfo,
  {$ELSE ~HAS_UNITSCOPE}
  TypInfo,
  {$ENDIF ~HAS_UNITSCOPE}
  JclFileUtils,
  JclStrings;

type
  TVariantWrapper = class(TObject)
  private
    FValue: Variant;
  end;

  TInterfaceWrapper = class(TObject)
  private
    FValue: IInterface;
  end;

function JclStringList: IJclStringList;
begin
  Result := TJclStringList.Create;
end;

function JclStringList(const AText: string): IJclStringList; overload;
begin
  Result := JclStringList;
  Result.Text := AText;
end;

function JclStringListStrings(AStrings: TStrings): IJclStringList; overload;
begin
  Result := JclStringList;
  Result.AddStrings(AStrings);
end;

function JclStringListStrings(const A: array of string): IJclStringList;
begin
  Result := JclStringList.AddStrings(A);
end;

function JclStringList(const A: array of const): IJclStringList;
begin
  Result := JclStringList.Add(A);
end;

//=== { TJclInterfacedStringList } ==============================================

procedure TJclInterfacedStringList.AfterConstruction;
Var
  MyOwner : TPersistent;
begin
  inherited;
  MyOwner := GetOwner;
  if Assigned(MyOwner) then
    MyOwner.GetInterface(IUnknown,FOwnerInterface);
end;


function TJclInterfacedStringList._AddRef: Integer;stdcall;
begin
  if assigned(FOwnerInterface) then
    Result := FOwnerInterface._AddRef
  else
    Result := -1;
end;


function TJclInterfacedStringList._Release: Integer;stdcall;
begin
  if assigned(FOwnerInterface) then
    Result := FOwnerInterface._Release
  else
    Result := -1;
end;


function TJclInterfacedStringList.QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} IID: TGUID; out Obj): HResult;stdcall;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

//=== { TJclStringList } =====================================================

function TJclStringList.Add(const A: array of const): IJclStringList;
const
  BoolToStr: array [Boolean] of string[5] = ('false', 'true');
var
  I: Integer;
begin
  Result := BeginUpdate;
  try
    for I := Low(A) to High(A) do
      case A[I].VType of
        vtInteger:
          Add(IntToStr(A[I].VInteger));
        vtBoolean:
          Add(string(BoolToStr[A[I].VBoolean]));
        vtChar:
          Add(string(AnsiString(A[I].VChar)));
        vtExtended:
          Add(FloatToStr(A[I].VExtended^));
        vtString:
          Add(string(A[I].VString^));
        vtPChar:
          Add(string(AnsiString(A[I].VPChar)));
        vtPWideChar:
          Add(string(WideString(A[I].VPWideChar)));
        vtObject:
          Add(A[I].VObject.ClassName);
        vtClass:
          Add(A[I].VClass.ClassName);
        vtAnsiString:
          Add(string(A[I].VAnsiString));
        vtWideString:
          Add(string(A[I].VWideString));
        vtCurrency:
          Add(CurrToStr(A[I].VCurrency^));
        vtVariant:
          Add(string(A[I].VVariant^));
        vtInt64:
          Add(IntToStr(A[I].VInt64^));
        {$IFDEF SUPPORTS_UNICODE_STRING}
        vtUnicodeString:
          Add(string(A[I].VUnicodeString));
        {$ENDIF SUPPORTS_UNICODE_STRING}
      end;
  finally
    Result := EndUpdate;
  end;
end;

function TJclStringList.AddStrings(const A: array of string): IJclStringList;
var
  I: Integer;
begin
  Result := BeginUpdate;
  try
    for I := Low(A) to High(A) do
      Add(A[I]);
  finally
    Result := EndUpdate;
  end;
end;

function TJclStringList.BeginUpdate: IJclStringList;
begin
  inherited BeginUpdate;
  Result := FSelfAsInterface;
end;

function TJclStringList.Clear: IJclStringList;
begin
  if CanFreeObjects then
    FreeObjects(False);
  inherited Clear;
  Result := FSelfAsInterface;
end;

function TJclStringList.EndUpdate: IJclStringList;
begin
  inherited EndUpdate;
  Result := FSelfAsInterface;
end;

function TJclStringList.ExtractWords(const AText: string; const ADelims: TSetOfAnsiChar;
  AClearBeforeAdd: Boolean): IJclStringList;
var
  L, I, X: Integer;
begin
  Result := BeginUpdate;
  try
    if AClearBeforeAdd then
      Clear;
    I := 1;
    L := Length(AText);
    while I <= L do
    begin
      while (I <= L) and (AnsiChar(AText[I]) in ADelims) do
        Inc(I);
      X := I;
      while (I <= L) and not (AnsiChar(AText[I]) in ADelims) do
        Inc(I);
      if X <> I then
        Add(Copy(AText, X, I - X));
    end;
  finally
    Result := EndUpdate;
  end;
end;

function TJclStringList.First: string;
begin
  Result := Strings[0];
end;

function TJclStringList.Join(const ASeparator: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to LastIndex - 1 do
    Result := Result + Strings[I] + ASeparator;
  if Count > 0 then
    Result := Result + Last;
end;

function TJclStringList.Last: string;
begin
  Result := Strings[LastIndex];
end;

function TJclStringList.Split(const AText, ASeparator: string;
  AClearBeforeAdd: Boolean = True): IJclStringList;
var
  LStartIndex, LEndIndex: Integer;
  LLengthSeparator: Integer;
begin
  Result := FSelfAsInterface;
  if AText <> '' then
  begin
    Result := BeginUpdate;
    try
      if AClearBeforeAdd then
        Clear;
      LLengthSeparator := Length(ASeparator);
      LStartIndex := 1;
      LEndIndex := StrSearch(ASeparator, AText, LStartIndex);
      while LEndIndex > 0 do
      begin
        Add(Copy(AText, LStartIndex, LEndIndex - LStartIndex));
        LStartIndex := LEndIndex + LLengthSeparator;
        LEndIndex := StrSearch(ASeparator, AText, LStartIndex);
      end;
      Add(Copy(AText, LStartIndex, MaxInt));
    finally
      Result := EndUpdate;
    end;
  end;
end;

function TJclStringList.Trim: IJclStringList;
var
  I: Integer;
begin
  Result := BeginUpdate;
  try
    for I := 0 to LastIndex do
      Strings[I] := {$IFDEF HAS_UNITSCOPE}System.{$ENDIF}SysUtils.Trim(Strings[I]);
  finally
    Result := EndUpdate;
  end;
end;

function TJclStringList._AddRef: Integer;
begin
  Result := InterlockedIncrement(FRefCount);
end;

function TJclStringList._Release: Integer;
begin
  Result := InterlockedDecrement(FRefCount);
  if Result = 1 then
  begin
    // When there is only one reference, it is the internal reference,
    // so we release it. The compiler will call _Release again and
    // the object will be destroyed.
    FSelfAsInterface := nil;
  end
  else
  if Result = 0 then
    Destroy;
end;

{$IFDEF JCL_PCRE}
function TJclStringList.DeleteRegEx(const APattern: string): IJclStringList;
var
  I: Integer;
begin
  Result := BeginUpdate;
  try
    for I := LastIndex downto 0 do
      if MatchRegEx(Strings[I], APattern) then
        Delete(I);
  finally
    Result := EndUpdate;
  end;
end;

function TJclStringList.KeepRegEx(const APattern: string): IJclStringList;
var
  I: Integer;
begin
  Result := BeginUpdate;
  try
    for I := LastIndex downto 0 do
      if not MatchRegEx(Strings[I], APattern) then
        Delete(I);
  finally
    Result := EndUpdate;
  end;
end;

function TJclStringList.MatchRegEx(const S, APattern: string): Boolean;
begin
  if FRegEx = nil then
    FRegEx := TJclRegEx.Create;
  if FLastRegExPattern <> APattern then
  begin
    if CaseSensitive then
      FRegEx.Options := FRegEx.Options - [roIgnoreCase]
    else
      FRegEx.Options := FRegEx.Options + [roIgnoreCase];
    FRegEx.Compile(APattern, False, True);
    FLastRegExPattern := APattern;
  end;
  Result := FRegEx.Match(S);
end;
{$ENDIF JCL_PCRE}

destructor TJclStringList.Destroy;
begin
  if CanFreeObjects then
    FreeObjects(False);
  {$IFDEF JCL_PCRE}
  FreeAndNil(FRegEx);
  {$ENDIF JCL_PCRE}
  inherited Destroy;
end;

{$IFDEF JCL_PCRE}
function TJclStringList.Directories(const APattern: string = '*';
  ARecursive: Boolean = False; const ARegExPattern: string = ''): IJclStringList;

  procedure DoDirectories(const APattern: string);
  var
    LSearchRec: TSearchRec;
    LFullName: string;
    LPath: string;
  begin
    LPath := ExtractFilePath(APattern);
    if FindFirst(APattern, faAnyFile, LSearchRec) = 0 then
      try
        repeat
          if (LSearchRec.Attr and faDirectory = 0) or
             (LSearchRec.Name = '.') or (LSearchRec.Name = '..') then
            Continue;
          LFullName := LPath + LSearchRec.Name;
          if (ARegExPattern = '') or MatchRegEx(LFullName, ARegExPattern) then
            Add(LFullName);
          if ARecursive then
            DoDirectories(PathAddSeparator(LFullName) + ExtractFileName(APattern));
        until FindNext(LSearchRec) <> 0;
      finally
        FindClose(LSearchRec);
      end;
  end;

begin
  Result := BeginUpdate;
  try
    if DirectoryExists(APattern) then
      DoDirectories(PathAddSeparator(APattern) + '*')
    else
      DoDirectories(APattern);
  finally
    Result := EndUpdate;
  end;
end;

function TJclStringList.Files(const APattern: string = '*';
  ARecursive: Boolean = False; const ARegExPattern: string = ''): IJclStringList;

  procedure DoFiles(const APattern: string);
  var
    LSearchRec: TSearchRec;
    LFullName: string;
    LDirectories: IJclStringList;
    LPath: string;
    I: Integer;
  begin
    LPath := ExtractFilePath(APattern);
    if FindFirst(APattern, faAnyFile and not faDirectory, LSearchRec) = 0 then
    begin
      try
        repeat
          if (LSearchRec.Attr and faDirectory <> 0) or
             (LSearchRec.Name = '.') or (LSearchRec.Name = '..') then
            Continue;
          LFullName := LPath + LSearchRec.Name;
          if (ARegExPattern = '') or MatchRegEx(LFullName, ARegExPattern) then
            Add(LFullName);
        until FindNext(LSearchRec) <> 0;
      finally
        FindClose(LSearchRec);
      end;
    end;
    if ARecursive then
    begin
      LDirectories := JclStringList.Directories(LPath + '*', False);
      for I := 0 to LDirectories.LastIndex do
        DoFiles(PathAddSeparator(LDirectories[I]) + ExtractFileName(APattern));
    end;
  end;

begin
  Result := BeginUpdate;
  try
    if DirectoryExists(APattern) then
      DoFiles(PathAddSeparator(APattern) + '*')
    else
      DoFiles(APattern);
  finally
    Result := EndUpdate;
  end;
end;
{$ENDIF JCL_PCRE}

function TJclStringList.LastIndex: Integer;
begin
  { The code bellow is more optimized than "Result := Count - 1". }
  Result := Count;
  Dec(Result);
end;

constructor TJclStringList.Create;
begin
  inherited Create;
  if QueryInterface(IJclStringList, FSelfAsInterface) <> 0 then
    System.Error(reIntfCastError);
end;

function TJclStringList.GetLists(Index: Integer): IJclStringList;
begin
  Result := Interfaces[Index] as IJclStringList;
  if Result = nil then
  begin
    Result := JclStringList;
    Interfaces[Index] := Result;
  end;
end;

procedure TJclStringList.SetLists(Index: Integer; const Value: IJclStringList);
begin
  Interfaces[Index] := Value;
end;

function TJclStringList.GetStringsRef: TStrings;
begin
  Result := Self;
end;

function TJclStringList.GetKeyInterface(const AKey: string): IInterface;
var
  I: Integer;
begin
  I := IndexOf(AKey);
  if I >= 0 then
    Result := Interfaces[I]
  else
    Result := nil;
end;

function TJclStringList.GetKeyObject(const AKey: string): TObject;
var
  I: Integer;
begin
  I := IndexOf(AKey);
  if I >= 0 then
    Result := Objects[I]
  else
    Result := nil;
end;

procedure TJclStringList.SetKeyInterface(const AKey: string; const Value: IInterface);
var
  I: Integer;
begin
  I := IndexOf(AKey);
  if I < 0 then
    I := Add(AKey);
  Interfaces[I] := Value
end;

procedure TJclStringList.SetKeyObject(const AKey: string; const Value: TObject);
var
  I: Integer;
begin
  I := IndexOf(AKey);
  if I < 0 then
    AddObject(AKey, Value)
  else
    Objects[I] := Value;
end;

function TJclStringList.ConfigAsSet: IJclStringList;
begin
  Sorted := True;
  Duplicates := dupIgnore;
  Result := FSelfAsInterface;
end;

function TJclStringList.GetKeyVariant(const AKey: string): Variant;
var
  I: Integer;
begin
  I := IndexOf(AKey);
  if I >= 0 then
    Result := Variants[I]
  else
    Result := Unassigned;
end;

procedure TJclStringList.SetKeyVariant(const AKey: string; const Value: Variant);
var
  I: Integer;
begin
  I := IndexOf(AKey);
  if I < 0 then
    I := Add(AKey);
  Variants[I] := Value
end;

function TJclStringList.GetValue(const Name: string): string;
begin
  Result := inherited Values[Name];
end;

procedure TJclStringList.SetValue(const Name, Value: string);
begin
  inherited Values[Name] := Value;
end;

function TJclStringList.GetInterfaceByIndex(Index: Integer): IInterface;
var
  V: TInterfaceWrapper;
begin
  if FObjectsMode <> omInterfaces then
    EnsureObjectsMode(omInterfaces);
  V := TInterfaceWrapper(inherited Objects[Index]);
  if V = nil then
    Result := nil
  else
    Result := V.FValue;
end;

procedure TJclStringList.SetInterfaceByIndex(Index: Integer; const Value: IInterface);
var
  V: TInterfaceWrapper;
begin
  if FObjectsMode <> omInterfaces then
    EnsureObjectsMode(omInterfaces);
  V := TInterfaceWrapper(inherited Objects[Index]);
  if V = nil then
  begin
    V := TInterfaceWrapper.Create;
    inherited Objects[Index] := V;
  end;
  V.FValue := Value;
end;

function TJclStringList.GetObjects(Index: Integer): TObject;
begin
  if FObjectsMode <> omObjects then
    EnsureObjectsMode(omObjects);
  Result := inherited Objects[Index];
end;

procedure TJclStringList.SetObjects(Index: Integer; const Value: TObject);
begin
  if FObjectsMode <> omObjects then
    EnsureObjectsMode(omObjects);
  inherited Objects[Index] := Value;
end;

function TJclStringList.GetVariants(AIndex: Integer): Variant;
var
  V: TVariantWrapper;
begin
  if FObjectsMode <> omVariants then
    EnsureObjectsMode(omVariants);
  V := TVariantWrapper(inherited Objects[AIndex]);
  if V = nil then
    Result := Unassigned
  else
    Result := V.FValue;
end;

procedure TJclStringList.SetVariants(Index: Integer; const Value: Variant);
var
  V: TVariantWrapper;
begin
  if FObjectsMode <> omVariants then
    EnsureObjectsMode(omVariants);
  V := TVariantWrapper(inherited Objects[Index]);
  if V = nil then
  begin
    V := TVariantWrapper.Create;
    inherited Objects[Index] := V;
  end;
  V.FValue := Value;
end;

procedure TJclStringList.EnsureObjectsMode(AMode: TJclStringListObjectsMode);
begin
  if FObjectsMode <> AMode then
  begin
    if FObjectsMode <> omNone then
    begin
      raise EJclStringListError.CreateFmt('Objects cannot be used as "%s" because it has been used as "%s".',
        [GetEnumName(TypeInfo(TJclStringListObjectsMode), Ord(AMode)),
        GetEnumName(TypeInfo(TJclStringListObjectsMode), Ord(FObjectsMode))]);
    end;
    FObjectsMode := AMode;
  end;
end;

function TJclStringList.GetKeyList(const AKey: string): IJclStringList;
begin
  Result := KeyInterface[AKey] as IJclStringList;
  if Result = nil then
  begin
    Result := JclStringList;
    KeyInterface[AKey] := Result;
  end;
end;

procedure TJclStringList.SetKeyList(const AKey: string; const Value: IJclStringList);
begin
  KeyInterface[AKey] := Value;
end;

function TJclStringList.Delete(AIndex: Integer): IJclStringList;
begin
  if CanFreeObjects then
    inherited Objects[AIndex].Free;
  inherited Delete(AIndex);
  Result := FSelfAsInterface;
end;

function TJclStringList.Delete(const AString: string): IJclStringList;
begin
  Result := Delete(IndexOf(AString));
end;

function TJclStringList.Exchange(Index1, Index2: Integer): IJclStringList;
begin
  inherited Exchange(Index1, Index2);
  Result := FSelfAsInterface;
end;

function LocalSort(List: TStringList; Index1, Index2: Integer): Integer;
begin
  Result := TJclStringList(List).FCompareFunction(TJclStringList(List).FSelfAsInterface, Index1, Index2);
end;

function TJclStringList.Sort(ACompareFunction: TJclStringListSortCompare = nil): IJclStringList;
begin
  FCompareFunction := ACompareFunction;
  if not Assigned(ACompareFunction) then
    inherited Sort
  else
    inherited CustomSort(@LocalSort);
  Result := FSelfAsInterface;
end;

function LocalSortAsInteger(List: TStringList; Index1, Index2: Integer): Integer;
begin
  Result := StrToInt(List[Index1]) - StrToInt(List[Index2]);
end;

function TJclStringList.SortAsInteger: IJclStringList;
begin
  inherited CustomSort(@LocalSortAsInteger);
  Result := FSelfAsInterface;
end;

{$IFNDEF HAS_TSTRINGS_COMPARESTRINGS}
function TJclStringList.CompareStrings(const S1, S2: string): Integer;
begin
  Result := AnsiCompareText(S1, S2);
end;
{$ENDIF ~HAS_TSTRINGS_COMPARESTRINGS}

function LocalSortByName(List: TStringList; Index1, Index2: Integer): Integer;
begin
  Result := TJclStringList(List).CompareStrings(List.Names[Index1], List.Names[Index2]);
end;

function TJclStringList.SortByName: IJclStringList;
begin
  inherited CustomSort(@LocalSortByName);
  Result := FSelfAsInterface;
end;

function TJclStringList.Insert(Index: Integer; const S: string): IJclStringList;
begin
  inherited Insert(Index, S);
  Result := FSelfAsInterface;
end;

function TJclStringList.InsertObject(Index: Integer; const S: string; AObject: TObject): IJclStringList;
begin
  inherited InsertObject(Index, S, AObject);
  Result := FSelfAsInterface;
end;

function TJclStringList.GetCaseSensitive: Boolean;
begin
  Result := inherited CaseSensitive;
end;

function TJclStringList.GetDuplicates: TDuplicates;
begin
  Result := inherited Duplicates;
end;

function TJclStringList.GetOnChange: TNotifyEvent;
begin
  Result := inherited OnChange;
end;

function TJclStringList.GetOnChanging: TNotifyEvent;
begin
  Result := inherited OnChanging;
end;

function TJclStringList.GetSorted: Boolean;
begin
  Result := inherited Sorted;
end;

procedure TJclStringList.SetCaseSensitive(const Value: Boolean);
begin
  inherited CaseSensitive := Value;
end;

procedure TJclStringList.SetDuplicates(const Value: TDuplicates);
begin
  inherited Duplicates := Value;
end;

procedure TJclStringList.SetOnChange(const Value: TNotifyEvent);
begin
  inherited OnChange := Value;
end;

procedure TJclStringList.SetOnChanging(const Value: TNotifyEvent);
begin
  inherited OnChanging := Value;
end;

procedure TJclStringList.SetSorted(const Value: Boolean);
begin
  inherited Sorted := Value;
end;

function TJclStringList.LoadFromFile(const FileName: string): IJclStringList;
begin
  inherited LoadFromFile(FileName);
  Result := FSelfAsInterface;
end;

function TJclStringList.LoadFromStream(Stream: TStream): IJclStringList;
begin
  inherited LoadFromStream(Stream);
  Result := FSelfAsInterface;
end;

function TJclStringList.SaveToFile(const FileName: string): IJclStringList;
begin
  inherited SaveToFile(FileName);
  Result := FSelfAsInterface;
end;

function TJclStringList.SaveToStream(Stream: TStream): IJclStringList;
begin
  inherited SaveToStream(Stream);
  Result := FSelfAsInterface;
end;

function TJclStringList.GetCommaText: string;
begin
  Result := inherited CommaText;
end;

function TJclStringList.GetDelimitedText: string;
begin
  Result := inherited DelimitedText;
end;

function TJclStringList.GetDelimiter: Char;
begin
  Result := inherited Delimiter;
end;

function TJclStringList.GetName(Index: Integer): string;
begin
  Result := inherited Names[Index];
end;

{$IFDEF COMPILER7_UP}

function TJclStringList.GetNameValueSeparator: Char;
begin
  Result := inherited NameValueSeparator;
end;

function TJclStringList.GetValueFromIndex(Index: Integer): string;
begin
  Result := inherited ValueFromIndex[Index];
end;

{$ENDIF COMPILER7_UP}

function TJclStringList.GetQuoteChar: Char;
begin
  Result := inherited QuoteChar;
end;

procedure TJclStringList.SetCommaText(const Value: string);
begin
  inherited CommaText := Value;
end;

procedure TJclStringList.SetDelimitedText(const Value: string);
begin
  inherited DelimitedText := Value;
end;

procedure TJclStringList.SetDelimiter(const Value: Char);
begin
  inherited Delimiter := Value;
end;

{$IFDEF COMPILER7_UP}

procedure TJclStringList.SetNameValueSeparator(const Value: Char);
begin
  inherited NameValueSeparator := Value;
end;

procedure TJclStringList.SetValueFromIndex(Index: Integer; const Value: string);
begin
  inherited ValueFromIndex[Index] := Value;
end;

{$ENDIF COMPILER7_UP}

procedure TJclStringList.SetQuoteChar(const Value: Char);
begin
  inherited QuoteChar := Value;
end;

function TJclStringList.Delimit(const ADelimiter: string): IJclStringList;
var
  I: Integer;
begin
  Result := BeginUpdate;
  try
    for I := 0 to LastIndex do
      Strings[I] := ADelimiter + Strings[I] + ADelimiter;
  finally
    Result := EndUpdate;
  end;
end;

function TJclStringList.LoadExeParams: IJclStringList;
var
  I: Integer;
  S: string;
begin
  Result := BeginUpdate;
  try
    Clear;
    for I := 1 to ParamCount do
    begin
      S := ParamStr(I);
      if (S[1] = '-') or (S[1] = '/') then
        System.Delete(S, 1, 1);
      Add(S);
    end;
  finally
    Result := EndUpdate;
  end;
end;

function TJclStringList.Exists(const S: string): Boolean;
begin
  Result := IndexOf(S) >= 0;
end;

function TJclStringList.ExistsName(const S: string): Boolean;
begin
  Result := IndexOfName(S) >= 0;
end;

function TJclStringList.DeleteBlanks: IJclStringList;
var
  I: Integer;
begin
  Result := BeginUpdate;
  try
    for I := LastIndex downto 0 do
      if {$IFDEF HAS_UNITSCOPE}System.{$ENDIF}SysUtils.Trim(Strings[I]) = '' then
        Delete(I);
  finally
    Result := EndUpdate;
  end;
end;

function TJclStringList.KeepIntegers: IJclStringList;
var
  I, X: Integer;
begin
  Result := BeginUpdate;
  try
    X := 0;
    for I := LastIndex downto 0 do
      if not TryStrToInt(Strings[I], X) then
        Delete(I);
  finally
    Result := EndUpdate;
  end;
end;

function TJclStringList.DeleteIntegers: IJclStringList;
var
  I, X: Integer;
begin
  Result := BeginUpdate;
  try
    X := 0;
    for I := LastIndex downto 0 do
      if TryStrToInt(Strings[I], X) then
        Delete(I);
  finally
    Result := EndUpdate;
  end;
end;

function TJclStringList.FreeObjects(AFreeAndNil: Boolean = False): IJclStringList;
var
  I: Integer;
begin
  if AFreeAndNil then
    Result := BeginUpdate;
  for I := 0 to LastIndex do
  begin
    inherited Objects[I].Free;
    if AFreeAndNil then
      inherited Objects[I] := nil;
  end;
  if AFreeAndNil then
    Result := EndUpdate
  else
    Result := FSelfAsInterface;
end;

function TJclStringList.ReleaseInterfaces: IJclStringList;
var
  I: Integer;
begin
  Result := BeginUpdate;
  try
    for I := 0 to LastIndex do
      Interfaces[I] := nil;
  finally
    Result := EndUpdate;
  end;
end;

function TJclStringList.Clone: IJclStringList;
begin
  Result := JclStringList.Assign(Self);
end;

function TJclStringList.Assign(Source: TPersistent): IJclStringList;
var
  L: TJclStringList;
  I: Integer;
begin
  inherited Assign(Source);
  Result := FSelfAsInterface;
  if Source is TJclStringList then
  begin
    L := TJclStringList(Source);
    FObjectsMode := L.FObjectsMode;
    if not (FObjectsMode in [omNone, omObjects]) then
    begin
      Result := BeginUpdate;
      try
        for I := 0 to LastIndex do
        begin
          inherited Objects[I] := nil;
          case FObjectsMode of
            omVariants:
              Variants[I] := L.Variants[I];
            omInterfaces:
              Interfaces[I] := L.Interfaces[I];
          end;
        end;
      finally
        Result := EndUpdate;
      end;
    end;
  end;
end;

function TJclStringList.CanFreeObjects: Boolean;
begin
  Result := not (FObjectsMode in [omNone, omObjects]);
end;

function TJclStringList.GetObjectsMode: TJclStringListObjectsMode;
begin
  Result := FObjectsMode;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
