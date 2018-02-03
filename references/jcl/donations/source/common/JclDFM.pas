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
{ The Original Code is JclDFM.pas.                                                                 }
{                                                                                                  }
{ The Initial Developer of the Original Code is documented in the accompanying                     }
{ help file JCL.chm. Portions created by these individuals are Copyright (C) of these individuals. }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Contains classes and routines for DFM reading and writing...                                     }
{                                                                                                  }
{ Known Issues:                                                                                    }
{   This is a preview - class and functionnames might be changed                                   }
{                                                                                                  }
{ Unit owner: Uwe Schuster                                                                         }
{ Last modified: January 4, 2004                                                                   }
{                                                                                                  }
{**************************************************************************************************}

{
Is
"The Initial Developer of the Original Code is documented in the accompanying
help file JCL.chm."
enough and Peter3 must not explicit listet in the header as author of DFMCleaner
on with JclDFM is based ?

JCL Style checks/fixes:
- "for i := 0 to Count - 1" instead of "to Pred(Count)"
- .GetCount w/o check that FList is Assigned
  "Result := FList.Count"
- .GetItem w/o indexcheck - TList will trow the exception
- .Delete w/o indexcheck - TList will trow the exception
//- TObject(FList[i]).Free should be okay in TDFMProperties.Clear
- inherited functions with "inherited function;" and not "inherited;"
- no assign alignment
    ShortVar := 1;
    LongVariable := 2;
  instead of
    ShortVar     := 1;
    LongVariable := 2;
JCL Style todo:
**- class/function seperation by
  //--------------------------------------------------------------------------------------------------
  // ...
  //--------------------------------------------------------------------------------------------------
*- check/fix function order in interface and implementation
TODO:
**- use TObjectList in **TDFMProperties and **TDFMComponents
**- support vaList in AsString
**- make TDFMProperty.As... writable + Add/Clear/Delete/Insert
- check .Add and .Insert (if Add or Insert into the ObjectList fails we should have
    a memoryleak because the property or component is still created) 
*- add support for value replacing (for example .Color clHotLight does exist in D6 but not in D5)
- check it with Kylix
- categorize the removed properties and replaced values
  (removing DesignSize doesn't matter but AutoCheck might break the functionality) 
- improve DFMLevel writing (autoclean, binary write for D2 or lower)
- tests
}

unit JclDFM;

{$I jcl.inc}

interface

uses
  SysUtils, Classes, TypInfo, Contnrs, Math;

//--------------------------------------------------------------------------------------------------
// miscellaneous TFiler descendant declarations
//--------------------------------------------------------------------------------------------------

type
  PJclDFMStdPropertyProcRec = ^TJclDFMStdPropertyProcRec;
  TJclDFMStdPropertyProcRec = record
    Name: string;
    ReadProc: TReaderProc;
    WriteProc: TWriterProc;
  end;

  PJclDFMBinaryPropertyProcRec = ^TJclDFMBinaryPropertyProcRec;
  TJclDFMBinaryPropertyProcRec = record
    Name: string;
    ReadProc, WriteProc: TStreamProc;
  end;

  PJclDFMPropertyProcRec = ^TJclDFMPropertyProcRec;
  TJclDFMPropertyProcRec = record
    case Binary: Boolean of
      False: (StdPropertyProcRecPtr: PJclDFMStdPropertyProcRec);
      True: (BinaryPropertyProcRecPtr: PJclDFMBinaryPropertyProcRec);
  end;

  // (usc) properties Count and Items should be enough -> remove Binary... and Std...
  TJclDFMFiler = class(TFiler)
  private
    FAllPropertyList: TList;
    FBinaryPropertyProcList: TList;
    FStdPropertyProcList: TList;
    function GetBinaryCount: Integer;
    function GetBinaryPropertyProcRec(AIndex: Integer): TJclDFMBinaryPropertyProcRec;
    function GetCount: Integer;
    function GetItems(AIndex: Integer): TJclDFMPropertyProcRec;
    function GetStdCount: Integer;
    function GetStdPropertyProcRec(AIndex: Integer): TJclDFMStdPropertyProcRec;
  public
    constructor Create(Stream: TStream; BufSize: Integer);
    destructor Destroy; override;

    procedure ClearPropertyLists;
    procedure DefineBinaryProperty(const Name: string;
      ReadData, WriteData: TStreamProc;
      HasData: Boolean); override;
    procedure DefineProperty(const Name: string;
      ReadData: TReaderProc; WriteData: TWriterProc;
      HasData: Boolean); override;
    procedure FlushBuffer; override;
    function GetBinaryReadProcByName(AName: string): TStreamProc;

    property BinaryCount: Integer read GetBinaryCount;
    property BinaryItems[AIndex: Integer]: TJclDFMBinaryPropertyProcRec read
      GetBinaryPropertyProcRec;
    property Count: Integer read GetCount;
    property Items[AIndex: Integer]: TJclDFMPropertyProcRec read GetItems;
    property StdCount: Integer read GetStdCount;
    property StdItems[AIndex: Integer]: TJclDFMStdPropertyProcRec read
      GetStdPropertyProcRec;
  end;

  TJclDFMReader = class(TReader)
  private
    procedure ReadBinary(AStream: TStream);
  end;

  // (rom) these names are too short  use dwlDelphi1 etc
  // (usc) vcl1, ..., vcl7 and clx1, clx2, clx3 (or clx140, clx141, clx145)
  //       might fit better
  TJclDFMWriteLevel = (dwlVCL1, dwlVCL2, dwlVCL3, dwlVCL4, dwlVCL5, dwlVCL6, dwlVCL7);

  TJclDFMLevelItemRec = record
    MinimumWriteLevel: TJclDFMWriteLevel;
    PropertyName: string;
  end;

const
  // (usc) not in use and will be moved out
  DFMPropertyList: array [0..6] of TJclDFMLevelItemRec =
   (
    (MinimumWriteLevel: dwlVCL6; PropertyName: '*.DesignSize'),
    (MinimumWriteLevel: dwlVCL6; PropertyName: 'TPageControl.TabIndex'),
    (MinimumWriteLevel: dwlVCL6; PropertyName: 'TJvPageControl.TabIndex'),
    (MinimumWriteLevel: dwlVCL6; PropertyName: 'TImage.Proportional'),
    (MinimumWriteLevel: dwlVCL6; PropertyName: 'TJvComboBox.AutoDropDown'),
    (MinimumWriteLevel: dwlVCL6; PropertyName: 'TComboBox.AutoDropDown'),
    (MinimumWriteLevel: dwlVCL6; PropertyName: 'TComboBox.OnCloseUp')
   );

type
  TJclDFMWriter = class(TWriter)
  private
    FNestingLevel: Integer;
    FWriteLevel: TJclDFMWriteLevel;
    function GetSkipUnicode: Boolean;
    procedure NewLine;
    procedure WriteBinary(AStream: TStream);
    procedure WriteIndent;
    procedure WriteStr(const S: string);
  public
    constructor Create(Stream: TStream; BufSize: Integer);

    function DecNestingLevel: Integer;
    function IncNestingLevel: Integer;

    property NestingLevel: Integer read FNestingLevel write FNestingLevel;
    property SkipUnicode: Boolean read GetSkipUnicode;
    property WriteLevel: TJclDFMWriteLevel read FWriteLevel write FWriteLevel;
  end;

//--------------------------------------------------------------------------------------------------
// DFM representation classes declarations
//--------------------------------------------------------------------------------------------------

  TJclDFMCollectionProperty = class;

  TJclDFMProperties = class;

  TJclDFMProperty = class(TObject)
  private
    FData: Pointer;
    FName: string;
    FTyp: TValueType;
    procedure FreeData;
    function GetAsBoolean: Boolean;
    // (rom) maybe GetAsDFMCollection?
    function GetAsCollectionProperty: TJclDFMCollectionProperty;
    function GetAsDFMProperties: TJclDFMProperties;
    function GetAsExtended: Extended;
    function GetAsInt64: Int64;
    function GetAsInteger: Integer;
    function GetAsStream: TMemoryStream;
    function GetAsString: string;
    function GetAsStrings: TStrings;
    function GetAsWideString: WideString;
    procedure ReadValue(AReader: TJclDFMReader);
    procedure SetAsBoolean(AValue: Boolean);
    procedure SetAsExtended(AValue: Extended);
    procedure SetAsInt64(AValue: Int64);
    procedure SetAsInteger(AValue: Integer);
    procedure SetAsString(AValue: string);
    procedure SetAsWideString(AValue: WideString);
    procedure SetTyp(AValue: TValueType);
    procedure WriteValue(AWriter: TJclDFMWriter);
  public
    constructor Create;
    destructor Destroy; override;

    procedure ReadProperty(AReader: TJclDFMReader);
    procedure WriteProperty(AWriter: TJclDFMWriter);

    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsCollectionProperty: TJclDFMCollectionProperty read
      GetAsCollectionProperty;
    property AsDFMProperties: TJclDFMProperties read GetAsDFMProperties;
    property AsExtended: Extended read GetAsExtended write SetAsExtended;
    property AsInt64: Int64 read GetAsInt64 write SetAsInt64;
    property AsInteger: Integer read GetAsInteger write SetAsInteger;
    property AsStream: TMemoryStream read GetAsStream;
    property AsString: string read GetAsString write SetAsString;
    property AsStrings: TStrings read GetAsStrings;
    property AsWideString: WideString read GetAsWideString write SetAsWideString;
    property Name: string read FName write FName;
    property Typ: TValueType read FTyp write SetTyp;
  end;

  TJclDFMProperties = class(TObject)
  private
    FPropertyList: TObjectList;
    function GetCount: Integer;
    function GetItem(AIndex: Integer): TJclDFMProperty;
  public
    constructor Create;
    destructor Destroy; override;
    // (rom) cut down on empty lines
    function Add: TJclDFMProperty;
    function AddProperties: TJclDFMProperties;
    procedure Clear;
    procedure Delete(AIndex: Integer);
    function Insert(AIndex: Integer): TJclDFMProperty;
    function InsertProperties(AIndex: Integer): TJclDFMProperties;
    procedure ReadProperties(AReader: TJclDFMReader);
    procedure WriteProperties(AWriter: TJclDFMWriter);

    property Count: Integer read GetCount;
    property Items[AIndex: Integer]: TJclDFMProperty read GetItem; default;
  end;

  TJclDFMCollectionPropertyData = class(TObject)
  private
    FHasIndex: Boolean;
    FIndex: Integer;
    FProperties: TJclDFMProperties;
  public
    constructor Create;
    destructor Destroy; override;

    property HasIndex: Boolean read FHasIndex write FHasIndex;
    property Index: Integer read FIndex write FIndex; //todo - is Index allowed ?
    property Properties: TJclDFMProperties read FProperties;
  end;

  TJclDFMCollectionProperty = class(TObject)
  private
    FCollectionPropertyDataList: TObjectList;
    function GetCount: Integer;
    function GetItem(AIndex: Integer): TJclDFMCollectionPropertyData;
  public
    constructor Create;
    destructor Destroy; override;

    function Add: TJclDFMCollectionPropertyData;
    procedure Clear;
    procedure Delete(AIndex: Integer);
    function Insert(AIndex: Integer): TJclDFMCollectionPropertyData;

    property Count: Integer read GetCount;
    property Items[AIndex: Integer]: TJclDFMCollectionPropertyData read GetItem; default;
  end;

  TJclDFMComponents = class;

  TJclDFMComponent = class(TObject)
  private
    FComponentClassName: string;
    FComponentName: string;
    FFilerFlags: TFilerFlags;
    FFilerPosition: Integer;
    FProperties: TJclDFMProperties;
    FSubComponents: TJclDFMComponents;

    function InternalFindComponent(ADFMComponent: TJclDFMComponent; AComponentName: string): TJclDFMComponent;
    procedure InternalFindComponentsByClass(ADFMComponent: TJclDFMComponent;
      AComponentClassName: string; AResultList: TList);
    procedure ReadHeader(AReader: TReader);
    procedure WriteHeader(AWriter: TJclDFMWriter);
  public
    constructor Create;
    destructor Destroy; override;

    function FindComponent(AComponentName: string): TJclDFMComponent;
    function FindComponentsByClass(AComponentClassName: string;
      AResultList: TList): Integer;
    procedure GetObjectBinary(AStream: TStream; AWithChilds: Boolean = True);
    procedure GetObjectText(AStream: TStream; AWithChilds: Boolean = True);
    procedure ReadComponent(AReader: TJclDFMReader);
    procedure WriteComponent(AWriter: TJclDFMWriter; AWithChilds: Boolean = True);

    property ComponentClassName: string read FComponentClassName write FComponentClassName;
    property ComponentName: string read FComponentName write FComponentName;
    property FilerFlags: TFilerFlags read FFilerFlags write FFilerFlags;
    property FilerPosition: Integer read FFilerPosition write FFilerPosition;
    property Properties: TJclDFMProperties read FProperties;
    property SubComponents: TJclDFMComponents read FSubComponents;
  end;

  TJclDFMRootComponent = class(TJclDFMComponent)
  public
    procedure LoadFromFile(AFileName: string);
    procedure LoadFromStream(AInput: TStream);
    procedure SaveToFile(AFileName: string);
    procedure SaveToStream(AOutput: TStream);
  end;

  TJclDFMComponents = class(TObject)
  private
    FComponentList: TObjectList;
    function GetCount: Integer;
    function GetItem(AIndex: Integer): TJclDFMComponent;
  public
    constructor Create;
    destructor Destroy; override;

    function Add: TJclDFMComponent;
    procedure Clear;
    procedure Delete(AIndex: Integer);
    function Insert(AIndex: Integer): TJclDFMComponent;
    procedure ReadComponents(AReader: TJclDFMReader);
    procedure WriteComponents(AWriter: TJclDFMWriter);

    property Count: Integer read GetCount;
    property Items[AIndex: Integer]: TJclDFMComponent read GetItem; default;
  end;

//--------------------------------------------------------------------------------------------------
// miscellaneous routines
//--------------------------------------------------------------------------------------------------

function ValueTypeToString(const AValueType: TValueType): string;
procedure DFMRemoveUnwantedComponentsAndProps(ADFMComponent: TJclDFMComponent;
  AComponentSkipList, APropertySkipList: TStrings);
procedure DFMGetAllComponentTypes(ADFMComponent: TJclDFMComponent;
  AComponentList: TStrings); overload;
procedure DFMGetAllComponentTypes(AFileName: string; AComponentList: TStrings); overload;
procedure DFMReplacePropertyValues(ADFMComponent: TJclDFMComponent;
  APropertyReplaceList: TStrings);

implementation

function IsBinDFM(Stream: TStream): Boolean;
var
  ASignature: Byte;
begin
  Stream.Read(ASignature, SizeOf(ASignature));
  Result := ASignature = $FF;
  Stream.Seek(-SizeOf(ASignature), soFromCurrent);
end;

const
{$IFNDEF DELPHI6_UP}
  sLineBreak = #13#10;
{$ENDIF}
  SingleQuote = '''';
  FilerBufferSize = 4096;
  BytesPerLine = 32;

//==================================================================================================
// TJclDFMFiler
//==================================================================================================

constructor TJclDFMFiler.Create(Stream: TStream; BufSize: Integer);
begin
  inherited Create(Stream, BufSize);
  FStdPropertyProcList := TList.Create;
  FBinaryPropertyProcList := TList.Create;
  FAllPropertyList := TList.Create;
end;

//--------------------------------------------------------------------------------------------------

destructor TJclDFMFiler.Destroy;
begin
  ClearPropertyLists;
  FStdPropertyProcList.Free;
  FBinaryPropertyProcList.Free;
  FAllPropertyList.Free;
  inherited Destroy;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclDFMFiler.ClearPropertyLists;
var
  I: Integer;
begin
  if FStdPropertyProcList.Count > 0 then
  begin
    for I := 0 to FStdPropertyProcList.Count - 1 do
      Dispose(FStdPropertyProcList[I]);
    FStdPropertyProcList.Clear;
  end;
  if FBinaryPropertyProcList.Count > 0 then
  begin
    for I := 0 to FBinaryPropertyProcList.Count - 1 do
      Dispose(FBinaryPropertyProcList[I]);
    FBinaryPropertyProcList.Clear;
  end;
  if FAllPropertyList.Count > 0 then
  begin
    for I := 0 to FAllPropertyList.Count - 1 do
      Dispose(FAllPropertyList[I]);
    FAllPropertyList.Clear;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclDFMFiler.DefineBinaryProperty(const Name: string;
  ReadData, WriteData: TStreamProc;
  HasData: Boolean);
var
  PBinaryPropertyRec: PJclDFMBinaryPropertyProcRec;
  PropertyRecPtr: PJclDFMPropertyProcRec;
begin
  New(PBinaryPropertyRec);
  PBinaryPropertyRec^.Name := Name;
  PBinaryPropertyRec^.ReadProc := ReadData;
  PBinaryPropertyRec^.WriteProc := WriteData;
  FBinaryPropertyProcList.Add(PBinaryPropertyRec);
  New(PropertyRecPtr);
  PropertyRecPtr^.Binary := True;
  PropertyRecPtr^.BinaryPropertyProcRecPtr := PBinaryPropertyRec;
  FAllPropertyList.Add(PropertyRecPtr);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclDFMFiler.DefineProperty(const Name: string;
  ReadData: TReaderProc; WriteData: TWriterProc;
  HasData: Boolean);
var
  PStdPropertyRec: PJclDFMStdPropertyProcRec;
  PropertyRecPtr: PJclDFMPropertyProcRec;
begin
  New(PStdPropertyRec);
  PStdPropertyRec^.Name := Name;
  PStdPropertyRec^.ReadProc := ReadData;
  PStdPropertyRec^.WriteProc := WriteData;
  FStdPropertyProcList.Add(PStdPropertyRec);
  New(PropertyRecPtr);
  PropertyRecPtr^.Binary := False;
  PropertyRecPtr^.StdPropertyProcRecPtr := PStdPropertyRec;
  FAllPropertyList.Add(PropertyRecPtr);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclDFMFiler.FlushBuffer;
begin
//do nothing
end;

//--------------------------------------------------------------------------------------------------

function TJclDFMFiler.GetBinaryCount: Integer;
begin
  Result := FBinaryPropertyProcList.Count;
end;

//--------------------------------------------------------------------------------------------------

function TJclDFMFiler.GetBinaryPropertyProcRec(AIndex: Integer): TJclDFMBinaryPropertyProcRec;
begin
  Result := PJclDFMBinaryPropertyProcRec(FBinaryPropertyProcList[AIndex])^;
end;

//--------------------------------------------------------------------------------------------------

function TJclDFMFiler.GetBinaryReadProcByName(AName: string): TStreamProc;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FBinaryPropertyProcList.Count - 1 do
    with PJclDFMBinaryPropertyProcRec(FBinaryPropertyProcList[I])^ do
      if Name = AName then
      begin
        Result := ReadProc;
        Break;
      end;
end;

//--------------------------------------------------------------------------------------------------

function TJclDFMFiler.GetCount: Integer;
begin
  Result := FAllPropertyList.Count;
end;

//--------------------------------------------------------------------------------------------------

function TJclDFMFiler.GetItems(AIndex: Integer): TJclDFMPropertyProcRec;
begin
  Result := PJclDFMPropertyProcRec(FAllPropertyList[AIndex])^;
end;

//--------------------------------------------------------------------------------------------------

function TJclDFMFiler.GetStdCount: Integer;
begin
  Result := FStdPropertyProcList.Count;
end;

//--------------------------------------------------------------------------------------------------

function TJclDFMFiler.GetStdPropertyProcRec(AIndex: Integer): TJclDFMStdPropertyProcRec;
begin
  Result := PJclDFMStdPropertyProcRec(FStdPropertyProcList[AIndex])^;
end;

//==================================================================================================
// TJclDFMReader
//==================================================================================================

procedure TJclDFMReader.ReadBinary(AStream: TStream);
var
  I: Integer;
  Count: Longint;
  Buffer: array [0..BytesPerLine-1] of Char;
begin
  ReadValue;
  Read(Count, SizeOf(Count));

  while Count > 0 do
  begin
    I := Min(Count, BytesPerLine);
    Read(Buffer, I);
    AStream.Write(Buffer, I);
    Dec(Count, I);
  end;
end;

//==================================================================================================
// TJclDFMWriter
//==================================================================================================

constructor TJclDFMWriter.Create(Stream: TStream; BufSize: Integer);
begin
  inherited Create(Stream, BufSize);
  FNestingLevel := 0;
//it doesn't compile with D3 or lower because of overload, int64 and widestrings
//but it's not dangerous to list it here
  {$IFDEF DELPHI1_UP}
  FWriteLevel := dwlVCL1;
  {$ENDIF}
  {$IFDEF DELPHI2_UP}
  FWriteLevel := dwlVCL2;
  {$ENDIF}
  {$IFDEF DELPHI3_UP}
  FWriteLevel := dwlVCL3;
  {$ENDIF}
  {$IFDEF DELPHI4_UP}
  FWriteLevel := dwlVCL4;
  {$ENDIF}
  {$IFDEF DELPHI5_UP}
  FWriteLevel := dwlVCL5;
  {$ENDIF}
  {$IFDEF DELPHI6_UP}
  FWriteLevel := dwlVCL6;
  {$ENDIF}
  {$IFDEF DELPHI7_UP}
  FWriteLevel := dwlVCL7;
  {$ENDIF}
end;

//--------------------------------------------------------------------------------------------------

function TJclDFMWriter.DecNestingLevel: Integer;
begin
  Dec(FNestingLevel);
  Result := FNestingLevel;  
end;

//--------------------------------------------------------------------------------------------------

function TJclDFMWriter.GetSkipUnicode: Boolean;
begin
  Result := FWriteLevel < dwlVCL6;
end;

//--------------------------------------------------------------------------------------------------

function TJclDFMWriter.IncNestingLevel: Integer;
begin
  Inc(FNestingLevel);
  Result := FNestingLevel;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclDFMWriter.NewLine;
begin
  WriteStr(sLineBreak);
  WriteIndent;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclDFMWriter.WriteBinary(AStream: TStream);
var
  MultiLine: Boolean;
  I: Integer;
  Count: Longint;
  Buffer: array [0..BytesPerLine-1] of Char;
  Text: array [0..BytesPerLine*2-1] of Char;
begin
  Count := AStream.Size;
  AStream.Position := 0;

  WriteStr('{');
  Inc(FNestingLevel);
  MultiLine := Count >= BytesPerLine;
  while Count > 0 do
  begin
    if MultiLine then
      NewLine;
    I := Min(Count, BytesPerLine);
    AStream.Read(Buffer, I);
    BinToHex(Buffer, Text, I);
    Write(Text, I * 2);
    Dec(Count, I);
  end;
  Dec(FNestingLevel);
  WriteStr('}');
end;

//--------------------------------------------------------------------------------------------------

procedure TJclDFMWriter.WriteIndent;
const
  Blanks: array [0..1] of Char = '  ';
var
  I: Integer;
begin
  for I := 1 to FNestingLevel do
    Write(Blanks, SizeOf(Blanks));
end;

//--------------------------------------------------------------------------------------------------

procedure TJclDFMWriter.WriteStr(const S: string);
begin
  Write(S[1], Length(S));
end;

//==================================================================================================
// TJclDFMProperty
//==================================================================================================

constructor TJclDFMProperty.Create;
begin
  inherited Create;

  FName := '';
  FTyp  := vaInt8;
  FData := nil;
end;

//--------------------------------------------------------------------------------------------------

destructor TJclDFMProperty.Destroy;
begin
  FreeData;

  inherited Destroy;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclDFMProperty.FreeData;
begin
  if Assigned(FData) and (not (FTyp in [vaInt8, vaInt16, vaInt32])) then
  begin
    if (FTyp in [vaString, vaLString]) or
      (FTyp in [vaWString {$IFDEF DELPHI6_UP}, vaUTF8String {$ENDIF}]) or
      (FTyp in [vaIdent, vaFalse, vaTrue, vaNil, vaNull]) then
      Dispose(FData)
    else
    if (FTyp = vaList) or (FTyp = vaBinary) or (FTyp = vaSet) or
      (FTyp = vaCollection) then
      TObject(FData).Free
    else
      FreeMem(FData); //vaExtended, vaSingle, vaCurrency, vaDate, vaInt64
  end;
end;

//--------------------------------------------------------------------------------------------------

function TJclDFMProperty.GetAsBoolean: Boolean;
begin
  Result := FTyp = vaTrue;
end;

//--------------------------------------------------------------------------------------------------

function TJclDFMProperty.GetAsCollectionProperty: TJclDFMCollectionProperty;
begin
  Result := nil;
  if FTyp = vaCollection then
    Result := FData;
end;

//--------------------------------------------------------------------------------------------------

function TJclDFMProperty.GetAsDFMProperties: TJclDFMProperties;
begin
  Result := nil;
  if FTyp = vaList then
    Result := FData;
end;

//--------------------------------------------------------------------------------------------------

function TJclDFMProperty.GetAsExtended: Extended;
begin
  Result := 0;
  if FTyp in [vaExtended, vaSingle, vaCurrency, vaDate] then   //todo - single, currency, data
    Result := PExtended(FData)^;
end;

//--------------------------------------------------------------------------------------------------

function TJclDFMProperty.GetAsInt64: Int64;
begin
  if FTyp = vaInt64 then
    Result := PInt64(FData)^
  else
    Result := GetAsInteger;
end;

//--------------------------------------------------------------------------------------------------

function TJclDFMProperty.GetAsInteger: Integer;
begin
  Result := 0;
  if FTyp in [vaInt8, vaInt16, vaInt32] then
    Result := Integer(FData);
end;

//--------------------------------------------------------------------------------------------------

function TJclDFMProperty.GetAsStream: TMemoryStream;
begin
  Result := nil;
  if FTyp = vaBinary then
    Result := FData;
end;

//--------------------------------------------------------------------------------------------------

function TJclDFMProperty.GetAsString: string;
var
  I: Integer;
  S: string;
  ListProperties: TJclDFMProperties;
begin
  Result := '';
  if FTyp = vaList then
  begin
    ListProperties := FData;
    if Assigned(ListProperties) then
      for I := 0 to ListProperties.Count - 1 do
      begin
        if Result <> '' then
          Result := Result + sLineBreak;
        Result := Result + ListProperties[I].AsString;
      end;
  end
  else
  if FTyp in [vaInt8, vaInt16, vaInt32] then
    Result := IntToStr(GetAsInteger)
  else
  if FTyp = vaExtended then
    Result := FloatToStr(GetAsExtended)
  else
  if FTyp = vaSingle then
    Result := FloatToStr(GetAsExtended)
  else
  if FTyp = vaCurrency then
    Result := FloatToStr(GetAsExtended)
  else
  if FTyp = vaDate then
    Result := FloatToStr(GetAsExtended)
  else
  if FTyp in [vaString, vaLString] then
    Result := PString(FData)^
  else
  if FTyp in [vaIdent, vaFalse, vaTrue, vaNil, vaNull] then
    Result := PString(FData)^
  else
  if FTyp = vaSet then
  begin
    Result := '[';
    with AsStrings do
      for I := 0 to Count - 1 do
      begin
        S := Strings[I];
        if S = '' then
          Break;
        if I > 0 then
          Result := Result + ', ';
        Result := Result + S;
      end;
    Result := Result + ']';
  end
  else
  if FTyp = vaInt64 then
    Result := IntToStr(GetAsInt64);
  //todo - support widestring
end;

//--------------------------------------------------------------------------------------------------

function TJclDFMProperty.GetAsStrings: TStrings;
begin
  Result := nil;
  if FTyp = vaSet then
    Result := FData;
end;

//--------------------------------------------------------------------------------------------------

function TJclDFMProperty.GetAsWideString: WideString;
begin
  Result := '';
  if FTyp in [vaWString {$IFDEF DELPHI6_UP}, vaUTF8String {$ENDIF}] then
    Result := PWideString(FData)^
  else
    Result := GetAsString;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclDFMProperty.ReadProperty(AReader: TJclDFMReader);
begin
  FName := AReader.ReadStr;
  ReadValue(AReader);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclDFMProperty.ReadValue(AReader: TJclDFMReader);
var
  APos: Integer;
  S: string;

  CollectionData: TJclDFMCollectionPropertyData;
  TempDFMProperty: TJclDFMProperty;
begin
  APos := AReader.Position;
  SetTyp(AReader.ReadValue); //todo - extern Typ := AReader.ReadValue ?
  AReader.Position := APos;
  case FTyp of
    vaList:
      begin
        AReader.ReadValue;
        while not AReader.EndOfList do
          AsDFMProperties.Add.ReadValue(AReader);
        AReader.ReadListEnd;
      end;
    vaInt8, vaInt16, vaInt32:
      AsInteger := AReader.ReadInteger;
    vaExtended:
      AsExtended := AReader.ReadFloat;
    vaSingle:
      AsExtended := AReader.ReadSingle; //todo - check if saving in extended is okay
    vaCurrency:
      AsExtended := AReader.ReadCurrency * 10000; //todo - check if saving in extended is okay
    vaDate:
      AsExtended := AReader.ReadDate; //todo - check if saving in extended is okay
    vaWString {$IFDEF DELPHI6_UP}, vaUTF8String {$ENDIF}:
      AsWideString := AReader.ReadWideString;
    vaString, vaLString:
      AsString := AReader.ReadString;
    vaIdent, vaFalse, vaTrue, vaNil, vaNull:
      AsString := AReader.ReadIdent;
    vaBinary:
      AReader.ReadBinary(AsStream);
    vaSet:
      begin
        AReader.ReadValue;
        while True do
        begin
          S := AReader.ReadStr;
          if S = '' then
            Break
          else
            AsStrings.Add(S);
        end;
      end;
    vaCollection:
      begin
        AReader.ReadValue;
        while not AReader.EndOfList do
        begin
          CollectionData := AsCollectionProperty.Add;

          if AReader.NextValue in [vaInt8, vaInt16, vaInt32] then
          begin
            CollectionData.HasIndex := True;
            TempDFMProperty := TJclDFMProperty.Create;
            try
              TempDFMProperty.ReadValue(AReader);
              CollectionData.Index := TempDFMProperty.AsInteger;
            finally
              TempDFMProperty.Free;
            end;
          end
          else
            CollectionData.HasIndex := False;

          AReader.CheckValue(vaList);
          while not AReader.EndOfList do
            CollectionData.Properties.Add.ReadProperty(AReader);
          AReader.ReadListEnd;
        end;
        AReader.ReadListEnd;
      end;
    vaInt64:
      AsInt64 := AReader.ReadInt64;
    else
      AReader.SkipValue;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclDFMProperty.SetAsBoolean(AValue: Boolean);
begin
  if FTyp in [vaFalse, vaTrue] then
  begin
    if not AValue then
    begin
      SetTyp(vaFalse); //todo - extern Typ := AReader.ReadValue ?
      AsString := 'False';
    end
    else
    begin
      SetTyp(vaTrue); //todo - extern Typ := AReader.ReadValue ?
      AsString := 'True';
    end;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclDFMProperty.SetAsExtended(AValue: Extended);
begin
  if FTyp in [vaExtended, vaSingle, vaCurrency, vaDate] then   //todo - single, currency, data
    PExtended(FData)^ := AValue;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclDFMProperty.SetAsInt64(AValue: Int64);
begin
  if FTyp = vaInt64 then
    PInt64(FData)^ := AValue;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclDFMProperty.SetAsInteger(AValue: Integer);
begin
  if FTyp in [vaInt8, vaInt16, vaInt32] then
    Integer(FData) := AValue;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclDFMProperty.SetAsString(AValue: string);
begin
  if FTyp in [vaString, vaLString, vaIdent, vaFalse, vaTrue, vaNil, vaNull] then
    PString(FData)^ := AValue;
  //todo - support widestring
end;

//--------------------------------------------------------------------------------------------------

procedure TJclDFMProperty.SetAsWideString(AValue: WideString);
begin
  if FTyp in [vaWString {$IFDEF DELPHI6_UP}, vaUTF8String {$ENDIF}] then
    PWideString(FData)^ := AValue
  else
    AsString := AValue; //todo - check
end;

//--------------------------------------------------------------------------------------------------

procedure TJclDFMProperty.SetTyp(AValue: TValueType);
var
  ExtendedPtr: PExtended;
  WideStringPtr: PWideString;
  StringPtr: PString;
  Int64Ptr: PInt64;
begin
  if FTyp <> AValue then
  begin
    FreeData;
    FTyp := AValue;
    if FTyp = vaList then
      FData := TJclDFMProperties.Create
    else
    if FTyp in [vaExtended, vaSingle, vaCurrency, vaDate] then
    begin
      GetMem(ExtendedPtr, SizeOf(Extended));
      ExtendedPtr^ := 0;
      FData := ExtendedPtr;
    end
    else
    if FTyp in [vaWString {$IFDEF DELPHI6_UP}, vaUTF8String {$ENDIF}] then
    begin
      New(WideStringPtr);
      WideStringPtr^ := '';
      FData := WideStringPtr;
    end
    else
    if FTyp in [vaString, vaLString, vaIdent, vaFalse, vaTrue, vaNil, vaNull] then
    begin
      New(StringPtr);
      StringPtr^ := '';
      FData := StringPtr;
    end
    else
    if FTyp = vaBinary then
      FData := TMemoryStream.Create
    else
    if FTyp = vaSet then
      FData := TStringList.Create
    else
    if FTyp = vaCollection then
      FData := TJclDFMCollectionProperty.Create
    else
    if FTyp = vaInt64 then
    begin
      GetMem(Int64Ptr, SizeOf(Int64));
      Int64Ptr^ := 0;
      FData := Int64Ptr;
    end
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclDFMProperty.WriteProperty(AWriter: TJclDFMWriter);
begin
  AWriter.WriteIndent;
  AWriter.WriteStr(FName);
  AWriter.WriteStr(' = ');
  WriteValue(AWriter);
  AWriter.WriteStr(sLineBreak);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclDFMProperty.WriteValue(AWriter: TJclDFMWriter);
const
  LineLength = 64;
var
  I, J, K, L: Integer;
  S: string;
  W: WideString;
  LineBreak: Boolean;

  ListProperties: TJclDFMProperties;
  Collection: TJclDFMCollectionProperty;
  CollectionData: TJclDFMCollectionPropertyData;
begin
  case FTyp of
    vaList:
      begin
        ListProperties := AsDFMProperties;
        AWriter.WriteStr('(');
        AWriter.IncNestingLevel;
        for I := 0 to ListProperties.Count - 1 do
        begin
          AWriter.NewLine;
          ListProperties[I].WriteValue(AWriter);
        end;
        AWriter.DecNestingLevel;
        AWriter.WriteStr(')');
      end;
    vaInt8, vaInt16, vaInt32:
      AWriter.WriteStr(GetAsString);
    vaExtended:
      AWriter.WriteStr(GetAsString); //todo - writefloat
    vaSingle:
      AWriter.WriteStr(GetAsString + 's'); //todo - writesingle
    vaCurrency:
      AWriter.WriteStr(GetAsString + 'c'); //todo - writecurrency
    vaDate:
      AWriter.WriteStr(GetAsString + 'd'); //todo - writedate
    vaWString {$IFDEF DELPHI6_UP}, vaUTF8String {$ENDIF}:
      begin
        W := GetAsWideString;
        L := Length(W);
        if L = 0 then
          AWriter.WriteStr(SingleQuote + SingleQuote)
        else
        begin
          I := 1;
          AWriter.IncNestingLevel;
          try
            if L > LineLength then
              AWriter.NewLine;
            K := I;
            repeat
              LineBreak := False;
              if (W[I] >= ' ') and (W[I] <> SingleQuote) and (Ord(W[i]) <= 127) then
              begin
                J := I;
                repeat
                  Inc(I)
                until (I > L) or (W[I] < ' ') or (W[I] = SingleQuote) or
                  ((I - K) >= LineLength) or (Ord(W[i]) > 127);
                if ((I - K) >= LineLength) then
                  LineBreak := True;
                AWriter.WriteStr(SingleQuote);
                while J < I do
                begin
                  AWriter.WriteStr(Char(W[J]));
                  Inc(J);
                end;
                AWriter.WriteStr(SingleQuote);
              end
              else
              begin
                AWriter.WriteStr('#');
                if (Ord(W[I]) > 255) and AWriter.SkipUnicode then
                  AWriter.WriteStr('32')
                else
                  AWriter.WriteStr(IntToStr(Ord(W[I])));
                Inc(I);
                if ((I - K) >= LineLength) then
                  LineBreak := True;
              end;
              if LineBreak and (I <= L) then
              begin
                AWriter.WriteStr(' +');
                AWriter.NewLine;
                K := I;
              end;
            until I > L;
          finally
            AWriter.DecNestingLevel;
          end;
        end;
      end;
    vaString, vaLString:
      begin
        S := GetAsString;
        L := Length(S);
        if L = 0 then
          AWriter.WriteStr(SingleQuote + SingleQuote)
        else
        begin
          I := 1;
          AWriter.IncNestingLevel;
          try
            if L > LineLength then
              AWriter.NewLine;
            K := I;
            repeat
              LineBreak := False;
              if (S[I] >= ' ') and (S[I] <> SingleQuote) then
              begin
                J := I;
                repeat
                  Inc(I)
                until (I > L) or (S[I] < ' ') or (S[I] = SingleQuote) or
                  ((I - K) >= LineLength);
                if ((I - K) >= LineLength) then
                begin
                  LIneBreak := True;
                  if ByteType(S, I) = mbTrailByte then
                    Dec(I);
                end;
                AWriter.WriteStr(SingleQuote);
                AWriter.Write(S[J], I - J);
                AWriter.WriteStr(SingleQuote);
              end
              else
              begin
                AWriter.WriteStr('#');
                AWriter.WriteStr(IntToStr(Ord(S[I])));
                Inc(I);
                if ((I - K) >= LineLength) then
                  LineBreak := True;
              end;
              if LineBreak and (I <= L) then
              begin
                AWriter.WriteStr(' +');
                AWriter.NewLine;
                K := I;
              end;
            until I > L;
          finally
            AWriter.DecNestingLevel;
          end;
        end;
      end;
    vaIdent, vaFalse, vaTrue, vaNil, vaNull:
      AWriter.WriteStr(GetAsString);
    vaBinary:
      AWriter.WriteBinary(AsStream);
    vaSet:
      AWriter.WriteStr(GetAsString);
    vaCollection:
      begin
        AWriter.WriteStr('<');
        AWriter.IncNestingLevel;
        Collection := AsCollectionProperty;
        for i := 0 to Collection.Count - 1 do
        begin
          CollectionData := Collection[i];

          AWriter.NewLine;
          AWriter.WriteStr('item');

          if CollectionData.HasIndex then
          begin
            AWriter.WriteStr(' [');
            AWriter.WriteStr(IntToStr(CollectionData.Index));
            AWriter.WriteStr(']');
          end;

          AWriter.WriteStr(sLineBreak);
          AWriter.IncNestingLevel;

          CollectionData.Properties.WriteProperties(AWriter);
          AWriter.DecNestingLevel;
          AWriter.WriteIndent;
          AWriter.WriteStr('end');
        end;
        AWriter.DecNestingLevel;
        AWriter.WriteStr('>');
      end;
    vaInt64:
      AWriter.WriteStr(GetAsString);
  end;
end;

//==================================================================================================
// TJclDFMProperties
//==================================================================================================

constructor TJclDFMProperties.Create;
begin
  inherited Create;

  FPropertyList := TObjectList.Create;
end;

//--------------------------------------------------------------------------------------------------

destructor TJclDFMProperties.Destroy;
begin
  Clear;
  FPropertyList.Free;

  inherited Destroy;
end;

//--------------------------------------------------------------------------------------------------

function TJclDFMProperties.Add: TJclDFMProperty;
begin
  Result := TJclDFMProperty.Create;
  FPropertyList.Add(Result);
end;

//--------------------------------------------------------------------------------------------------

function TJclDFMProperties.AddProperties: TJclDFMProperties;
begin
  Result := TJclDFMProperties.Create;
  FPropertyList.Add(Result);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclDFMProperties.Clear;
begin
  FPropertyList.Clear;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclDFMProperties.Delete(AIndex: Integer);
begin
  FPropertyList.Delete(AIndex);
end;

//--------------------------------------------------------------------------------------------------

function TJclDFMProperties.GetCount: Integer;
begin
  Result := FPropertyList.Count;
end;

//--------------------------------------------------------------------------------------------------

function TJclDFMProperties.GetItem(AIndex: Integer): TJclDFMProperty;
begin
  Result := TJclDFMProperty(FPropertyList[AIndex]);
end;

//--------------------------------------------------------------------------------------------------

function TJclDFMProperties.Insert(AIndex: Integer): TJclDFMProperty;
begin
  Result := TJclDFMProperty.Create;
  FPropertyList.Insert(AIndex, Result);
end;

//--------------------------------------------------------------------------------------------------

function TJclDFMProperties.InsertProperties(AIndex: Integer): TJclDFMProperties;
begin
  Result := TJclDFMProperties.Create;
  FPropertyList.Insert(AIndex, Result);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclDFMProperties.ReadProperties(AReader: TJclDFMReader);
var
  DFMProperty: TJclDFMProperty;
begin
  Clear;
  while not AReader.EndOfList do
  begin
    //todo - could be reduced to Add.ReadProperty(AReader);
    DFMProperty := TJclDFMProperty.Create;
    DFMProperty.ReadProperty(AReader);
    FPropertyList.Add(DFMProperty);
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclDFMProperties.WriteProperties(AWriter: TJclDFMWriter);
var
  I: Integer;
begin //todo - perhaps use internal vars
  for I := 0 to Count - 1 do
    Items[I].WriteProperty(AWriter);
end;

//==================================================================================================
// TJclDFMCollectionPropertyData
//==================================================================================================

constructor TJclDFMCollectionPropertyData.Create;
begin
  inherited Create;

  FHasIndex := False;
  FIndex := 0;
  FProperties := TJclDFMProperties.Create;
end;

//--------------------------------------------------------------------------------------------------

destructor TJclDFMCollectionPropertyData.Destroy;
begin
  FProperties.Free;

  inherited Destroy;
end;

//==================================================================================================
// TJclDFMCollectionProperty
//==================================================================================================

constructor TJclDFMCollectionProperty.Create;
begin
  inherited Create;
  FCollectionPropertyDataList := TObjectList.Create;
end;

//--------------------------------------------------------------------------------------------------

destructor TJclDFMCollectionProperty.Destroy;
begin
  Clear;
  FCollectionPropertyDataList.Free;

  inherited Destroy;
end;

//--------------------------------------------------------------------------------------------------

function TJclDFMCollectionProperty.Add: TJclDFMCollectionPropertyData;
begin
  Result := TJclDFMCollectionPropertyData.Create;
  FCollectionPropertyDataList.Add(Result);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclDFMCollectionProperty.Clear;
begin
  FCollectionPropertyDataList.Clear;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclDFMCollectionProperty.Delete(AIndex: Integer);
begin
  FCollectionPropertyDataList.Delete(AIndex);
end;

//--------------------------------------------------------------------------------------------------

function TJclDFMCollectionProperty.GetCount: Integer;
begin
  Result := FCollectionPropertyDataList.Count;
end;

//--------------------------------------------------------------------------------------------------

function TJclDFMCollectionProperty.GetItem(
  AIndex: Integer): TJclDFMCollectionPropertyData;
begin
  Result := TJclDFMCollectionPropertyData(FCollectionPropertyDataList[AIndex]);
end;

//--------------------------------------------------------------------------------------------------

function TJclDFMCollectionProperty.Insert(AIndex: Integer): TJclDFMCollectionPropertyData;
begin
  Result := TJclDFMCollectionPropertyData.Create;
  FCollectionPropertyDataList.Insert(AIndex, Result);
end;

//==================================================================================================
// TJclDFMComponent
//==================================================================================================

constructor TJclDFMComponent.Create;
begin
  inherited Create;

  FComponentClassName := '';
  FComponentName := '';
  FFilerFlags := [];
  FFilerPosition := 0;

  FProperties := TJclDFMProperties.Create;
  FSubComponents := TJclDFMComponents.Create;
end;

//--------------------------------------------------------------------------------------------------

destructor TJclDFMComponent.Destroy;
begin
  FProperties.Free;
  FSubComponents.Free;

  inherited Destroy;
end;

//--------------------------------------------------------------------------------------------------

function TJclDFMComponent.FindComponent(AComponentName: string): TJclDFMComponent;
begin
  Result := nil;
  if AComponentName <> '' then
    Result := InternalFindComponent(Self, AComponentName);
end;

//--------------------------------------------------------------------------------------------------

function TJclDFMComponent.FindComponentsByClass(AComponentClassName: string;
  AResultList: TList): Integer;
begin
  Result := 0;
  if (AComponentClassName <> '') and Assigned(AResultList) then
  begin
    AResultList.Clear;
    InternalFindComponentsByClass(Self, AComponentClassName, AResultList);
    Result := AResultList.Count;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclDFMComponent.GetObjectBinary(AStream: TStream; AWithChilds: Boolean = True);
var
  TextStream: TMemoryStream;
begin
  TextStream := TMemoryStream.Create;
  try
    GetObjectText(TextStream, AWithChilds);
    TextStream.Position := 0;
    ObjectTextToBinary(TextStream, AStream);
  finally
    TextStream.Free;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclDFMComponent.GetObjectText(AStream: TStream; AWithChilds: Boolean = True);
var
  SaveSeparator: Char;
  Writer: TJclDFMWriter;
begin
  SaveSeparator := DecimalSeparator;
  DecimalSeparator := '.';
  try
    Writer := TJclDFMWriter.Create(AStream, FilerBufferSize);
    try
      WriteComponent(Writer, AWithChilds);
    finally
      Writer.Free;
    end;
  finally
    DecimalSeparator := SaveSeparator;
  end;
end;

//--------------------------------------------------------------------------------------------------

function TJclDFMComponent.InternalFindComponent(ADFMComponent: TJclDFMComponent;
  AComponentName: string): TJclDFMComponent;
var
  I: Integer;
begin
  Result := nil;
  if Assigned(ADFMComponent) then
  begin
    // (rom) are you sure to handle ComponentName case INsensitive?
    if SameText(ADFMComponent.ComponentName, AComponentName) then
      Result := ADFMComponent;
    if not Assigned(Result) then
      for I := 0 to ADFMComponent.SubComponents.Count - 1 do
      begin
        Result := InternalFindComponent(ADFMComponent.SubComponents[I], AComponentName);
        if Assigned(Result) then
          Break;
      end;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclDFMComponent.InternalFindComponentsByClass(ADFMComponent: TJclDFMComponent;
  AComponentClassName: string; AResultList: TList);
var
  I: Integer;
begin
  if Assigned(ADFMComponent) then
  begin
    if SameText(ADFMComponent.ComponentClassName, AComponentClassName) then
      AResultList.Add(ADFMComponent);
    for I := 0 to ADFMComponent.SubComponents.Count - 1 do
      InternalFindComponentsByClass(ADFMComponent.SubComponents[I],
        AComponentClassName, AResultList);
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclDFMComponent.ReadComponent(AReader: TJclDFMReader);
begin
  ReadHeader(AReader);
  FProperties.ReadProperties(AReader);
  AReader.ReadListEnd;
  SubComponents.ReadComponents(AReader);
  AReader.ReadListEnd;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclDFMComponent.ReadHeader(AReader: TReader);
begin
  AReader.ReadPrefix(FFilerFlags, FFilerPosition);
  FComponentClassName := AReader.ReadStr;
  FComponentName := AReader.ReadStr;

  //todo - the componentname shouldn't be empty -> exception ?
  if FComponentName = '' then
    FComponentName := FComponentClassName;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclDFMComponent.WriteComponent(AWriter: TJclDFMWriter;
  AWithChilds: Boolean = True);
begin
  WriteHeader(AWriter);
  AWriter.IncNestingLevel;

  Properties.WriteProperties(AWriter);
  if AWithChilds then
    SubComponents.WriteComponents(AWriter);

  AWriter.DecNestingLevel;

  AWriter.WriteIndent;
  AWriter.WriteStr('end' + sLineBreak);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclDFMComponent.WriteHeader(AWriter: TJclDFMWriter);
begin
  AWriter.WriteIndent;
  if ffInherited in FFilerFlags then
    AWriter.WriteStr('inherited ')
  else
  if ffInline in FFilerFlags then
    AWriter.WriteStr('inline ')
  else
    AWriter.WriteStr('object ');
  if FComponentName <> '' then
  begin
    AWriter.WriteStr(FComponentName);
    AWriter.WriteStr(': ');
  end;
  AWriter.WriteStr(FComponentClassName);
  if ffChildPos in FFilerFlags then
  begin
    AWriter.WriteStr(' [');
    AWriter.WriteStr(IntToStr(FFilerPosition));
    AWriter.WriteStr(']');
  end;
  AWriter.WriteStr(sLineBreak);
end;

//==================================================================================================
// TJclDFMRootComponent
//==================================================================================================

procedure TJclDFMRootComponent.LoadFromFile(AFileName: string);
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(AFileName, fmOpenRead); //todo -> sharemode
  try
    LoadFromStream(fs);
  finally
    fs.Free;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclDFMRootComponent.LoadFromStream(AInput: TStream);
var
  SaveSeparator: Char;
  Reader: TJclDFMReader;
  tmpStream: TMemoryStream;
begin
  FProperties.Clear;
  FSubComponents.Clear;
  tmpStream := TMemoryStream.Create;
  try
    if not IsBinDFM(AInput) then
    begin
      ObjectTextToResource(AInput, tmpStream);
      tmpStream.Seek(0, soFromBeginning);
      AInput := tmpStream;
    end;

    AInput.ReadResHeader;
    Reader := TJclDFMReader.Create(AInput, FilerBufferSize);
    SaveSeparator := DecimalSeparator;
    DecimalSeparator := '.';
    try
      Reader.ReadSignature;
      ReadComponent(Reader);
    finally
      DecimalSeparator := SaveSeparator;
      Reader.Free;
    end;
  finally
    tmpStream.Free;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclDFMRootComponent.SaveToFile(AFileName: string);
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(AFileName, fmCreate);
  try
    SaveToStream(fs);
  finally
    fs.Free;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclDFMRootComponent.SaveToStream(AOutput: TStream);
begin
  GetObjectText(AOutput);
end;

//==================================================================================================
// TJclDFMComponents
//==================================================================================================

constructor TJclDFMComponents.Create;
begin
  inherited Create;

  FComponentList := TObjectList.Create;
end;

//--------------------------------------------------------------------------------------------------

destructor TJclDFMComponents.Destroy;
begin
  Clear;
  FComponentList.Free;

  inherited Destroy;
end;

//--------------------------------------------------------------------------------------------------

function TJclDFMComponents.Add: TJclDFMComponent;
begin
  Result := TJclDFMComponent.Create;
  FComponentList.Add(Result);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclDFMComponents.Clear;
begin
  FComponentList.Clear;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclDFMComponents.Delete(AIndex: Integer);
begin
  FComponentList.Delete(AIndex);
end;

//--------------------------------------------------------------------------------------------------

function TJclDFMComponents.GetCount: Integer;
begin
  Result := FComponentList.Count;
end;

//--------------------------------------------------------------------------------------------------

function TJclDFMComponents.GetItem(AIndex: Integer): TJclDFMComponent;
begin
  Result := TJclDFMComponent(FComponentList[AIndex]);
end;

//--------------------------------------------------------------------------------------------------

function TJclDFMComponents.Insert(AIndex: Integer): TJclDFMComponent;
begin
  Result := TJclDFMComponent.Create;
  FComponentList.Insert(AIndex, Result);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclDFMComponents.ReadComponents(AReader: TJclDFMReader);
var
  DFMComponent: TJclDFMComponent;
begin
  Clear;
  while not AReader.EndOfList do
  begin
    //todo - could be reduced to Add.ReadComponent(AReader);
    DFMComponent := TJclDFMComponent.Create;
    DFMComponent.ReadComponent(AReader);
    FComponentList.Add(DFMComponent);
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclDFMComponents.WriteComponents(AWriter: TJclDFMWriter);
var
  I: Integer;
begin //todo - perhaps use internal vars
  for I := 0 to Count - 1 do
    Items[I].WriteComponent(AWriter);
end;

//==================================================================================================
// miscellaneous routines
//==================================================================================================

function ValueTypeToString(const AValueType: TValueType): string;
begin
  Result := GetEnumName(TypeInfo(TValueType), Integer(AValueType));
end;

//--------------------------------------------------------------------------------------------------

function IsUnwantedComponent(const AClassName: string;
  AComponentSkipList: TStrings): Boolean;
var
  I: Integer;
begin
  Result := False;
  if Assigned(AComponentSkipList) then
    for I := 0 to AComponentSkipList.Count - 1 do
      if SameText(AClassName, AComponentSkipList[I]) then
      begin
        Result := True;
        Break;
      end;
end;

//--------------------------------------------------------------------------------------------------

function IsUnwantedProperty(const AClassName, APropName: string;
  APropertySkipList: TStrings): Boolean;
var
  I: Integer;
begin
  Result := False;
  if Assigned(APropertySkipList) then
    for I := 0 to APropertySkipList.Count - 1 do
      if SameText(AClassName + '.' + APropName, APropertySkipList[I]) or
        SameText('*.' + APropName, APropertySkipList[I]) then
      begin
        Result := True;
        Break;
      end;
end;

//--------------------------------------------------------------------------------------------------

procedure DFMRemoveUnwantedComponentsAndProps(ADFMComponent: TJclDFMComponent;
  AComponentSkipList, APropertySkipList: TStrings);
var
  I: Integer;
begin
  with ADFMComponent do
  begin
    if Assigned(APropertySkipList) and (APropertySkipList.Count > 0) then
      for I := Properties.Count - 1 downto 0 do
        if IsUnwantedProperty(ComponentClassName, Properties[I].Name,
          APropertySkipList) then
          Properties.Delete(I);
    if (Assigned(APropertySkipList) and (APropertySkipList.Count > 0)) or
      (Assigned(AComponentSkipList) and (AComponentSkipList.Count > 0)) then
      for I := SubComponents.Count - 1 downto 0 do
        if IsUnwantedComponent(ComponentClassName, AComponentSkipList) then
          SubComponents.Delete(I)
        else
          DFMRemoveUnwantedComponentsAndProps(SubComponents[I], AComponentSkipList,
            APropertySkipList);
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure DFMGetAllComponentTypes(ADFMComponent: TJclDFMComponent;
  AComponentList: TStrings);
var
  I: Integer;
begin
  if AComponentList.IndexOf(ADFMComponent.ComponentClassName) = -1 then
    AComponentList.Add(ADFMComponent.ComponentClassName);
  for I := 0 to ADFMComponent.SubComponents.Count - 1 do
    DFMGetAllComponentTypes(ADFMComponent.SubComponents[I], AComponentList);
end;

//--------------------------------------------------------------------------------------------------

procedure DFMGetAllComponentTypes(AFileName: string; AComponentList: TStrings);
var
  RComp: TJclDFMRootComponent;
begin
  RComp := TJclDFMRootComponent.Create;
  try
    RComp.LoadFromFile(AFileName);
    AComponentList.Clear;
    DFMGetAllComponentTypes(RComp, AComponentList);
  finally
    RComp.Free;
  end;
end;

//--------------------------------------------------------------------------------------------------

function GetNewPropertyValue(const APropertyValue: string;
  APropertyReplaceList: TStrings): string;
begin
  Result := '';
  if Assigned(APropertyReplaceList) then
    Result := APropertyReplaceList.Values[APropertyValue];
end;

//--------------------------------------------------------------------------------------------------

procedure DFMReplacePropertyValues(ADFMComponent: TJclDFMComponent;
  APropertyReplaceList: TStrings);
var
  I: Integer;
  NewPropertyValue: string;
begin
  if Assigned(APropertyReplaceList) and (APropertyReplaceList.Count > 0) then
  with ADFMComponent do
  begin
    for I := Properties.Count - 1 downto 0 do
    begin
      NewPropertyValue := GetNewPropertyValue(Properties[I].AsString, APropertyReplaceList);
      if NewPropertyValue <> '' then
        Properties[I].AsString := NewPropertyValue;
    end;
    for I := SubComponents.Count - 1 downto 0 do
      DFMReplacePropertyValues(SubComponents[I], APropertyReplaceList);
  end;
end;

end.
