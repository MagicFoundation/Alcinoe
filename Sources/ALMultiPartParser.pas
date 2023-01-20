{*******************************************************************************
MultiPart objects to encode or decode stream in mime multipart/xxx format.

Link :
http://msdn.microsoft.com/library/default.asp?url=/library/en-us/cdosys/html/7a18a98b-3a18-45b2-83a9-28a8f4099970.asp
http://www.ietf.org/rfc/rfc2646.txt
http://www.w3.org/TR/REC-html40/interact/forms.html#h-17.1
http://www.ietf.org/rfc/rfc1867.txt
http://www.ietf.org/rfc/rfc2388.txt
http://www.w3.org/MarkUp/html-spec/html-spec_8.html
*******************************************************************************}
unit ALMultiPartParser;

interface

Uses
  System.Classes,
  System.Contnrs,
  ALStringList;

type

  {--Single multipart Object-------------}
  TALMultiPartBaseContent = class(TObject)
  private
    FContentType: AnsiString;
    FContentTransferEncoding: AnsiString;
    FContentDisposition: AnsiString;
    FContentID: AnsiString;
    FContentDescription: AnsiString;
    FDataStream: TStream;
    FCustomHeaders: TALStrings;
    Function GetRawHeaderText: AnsiString;
    procedure SetRawHeaderText(const aRawHeaderText: AnsiString);
    function GetDataString: AnsiString;
    procedure SetDataString(const aValue: AnsiString);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear; virtual;
    procedure LoadDataFromFile(const aFileName: AnsiString); virtual;
    procedure LoadDataFromStream(aStream: TStream); virtual;
    procedure LoadDataFromFileBase64Encode(const aFileName: AnsiString); virtual;
    procedure LoadDataFromStreamBase64Encode(aStream: TStream); virtual;
    procedure SaveDataToFile(const aFileName: AnsiString); virtual;
    procedure SaveDataToStream(aStream: TStream); virtual;
    procedure SaveDataToFileBase64Decode(const aFileName: AnsiString); virtual;
    procedure SaveDataToStreamBase64Decode(aStream: TStream); virtual;
    property ContentType: AnsiString read FContentType write FContentType; //Content-Type: text/plain; charset="utf-8"  or  Content-Type: image/bmp; name="Blue Lace 16.bmp"
    property ContentTransferEncoding: AnsiString read FContentTransferEncoding write FContentTransferEncoding; //Content-Transfer-Encoding: base64
    property ContentDisposition: AnsiString read FContentDisposition write FContentDisposition; //Content-Disposition: attachment; filename="Blue Lace 16.bmp"
    property ContentID: AnsiString read FContentID write FContentID; //Content-ID: <foo4%25foo1@bar.net>
    property ContentDescription: AnsiString read FContentDescription write FContentDescription; //Content-Description: some text
    property DataStream: TStream read FDataStream;
    property DataString: AnsiString read GetDataString Write SetDataString;
    property CustomHeaders: TALStrings read FCustomHeaders;
    property RawHeaderText: AnsiString read GetRawHeaderText write setRawHeaderText;
  end;

  {--List Of multipart Objects----------------}
  TALMultiPartBaseContents = class(TObjectList)
  private
  protected
    function GetItem(Index: Integer): TALMultiPartBaseContent;
    procedure SetItem(Index: Integer; AObject: TALMultiPartBaseContent);
  public
    Function Add: TALMultiPartBaseContent; overload;
    function Add(AObject: TALMultiPartBaseContent): Integer; overload;
    function Remove(AObject: TALMultiPartBaseContent): Integer;
    function IndexOf(AObject: TALMultiPartBaseContent): Integer;
    procedure Insert(Index: Integer; AObject: TALMultiPartBaseContent);
    property Items[Index: Integer]: TALMultiPartBasecontent read GetItem write SetItem; default;
  end;

  {--TAlMultiPartBaseStream-------------------}
  TAlMultiPartBaseStream = class(TMemoryStream)
  private
    FBoundary: AnsiString;
    function GenerateUniqueBoundary: AnsiString;
  public
    constructor Create; virtual;
    procedure   AddContent(aContent: TALMultiPartBaseContent); virtual;
    procedure   CloseBoundary; virtual;
    property    Boundary: AnsiString read FBoundary write FBoundary;
  end;

  {--TALMultipartBaseEncoder-------------}
  TALMultipartBaseEncoder = class(TObject)
  private
    FDataStream: TAlMultiPartBaseStream;
  protected
    Function CreateDataStream: TAlMultiPartBaseStream; virtual; Abstract;
    function GetDataStream: TAlMultiPartBaseStream; virtual;
  public
    constructor	Create; virtual;
    destructor  Destroy; override;
    procedure   Encode(acontents: TALMultiPartBaseContents); overload;
    property    DataStream: TAlMultiPartBaseStream read GetDataStream;
  end;

  {--TALMultipartBaseDecoder-------------}
  TALMultipartBaseDecoder = class(TObject)
  private
    FContents: TALMultiPartBaseContents;
  protected
    function GetContents: TALMultiPartBaseContents; virtual;
    Function CreateContent: TALMultiPartBaseContent; virtual; Abstract;
    Function CreateContents: TALMultiPartBaseContents; virtual; Abstract;
  public
    constructor	Create; virtual;
    destructor  Destroy; override;
    procedure   Decode(aDataStream: Tstream; const aboundary: AnsiString); overload; Virtual;
    procedure   Decode(const aDataStr: AnsiString; const aboundary: AnsiString); overload; Virtual;
  end;

{Below a sample of multipart/alternative message :
********************************************
Mime-Version: 1.0
Content-Type: multipart/alternative;
	boundary="----=_NextPart_000_0189_01C81ED9.A3E50190"

This is a multi-part message in MIME format.

------=_NextPart_000_0189_01C81ED9.A3E50190
Content-Type: text/plain;
	charset="utf-8"
Content-Transfer-Encoding: quoted-printable

qsdqsd

qsdqsdqsd sqdqsds
------=_NextPart_000_0189_01C81ED9.A3E50190
Content-Type: text/html;
	charset="utf-8"
Content-Transfer-Encoding: quoted-printable

=EF=BB=BF<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN">
<HTML><HEAD>
<META http-equiv=3DContent-Type content=3D"text/html; charset=3Dutf-8">
<META content=3D"MSHTML 6.00.6000.16544" name=3DGENERATOR>
<STYLE></STYLE>
</HEAD>
<BODY>
<DIV><FONT face=3DArial size=3D2>qsdqsd</FONT></DIV>
<DIV><FONT face=3DArial size=3D2></FONT>&nbsp;</DIV>
<DIV><FONT face=3DArial size=3D2>qsdqsdqsd=20
<EM>sqdqsds</EM></FONT></DIV></BODY></HTML>

------=_NextPart_000_0189_01C81ED9.A3E50190--
********************************************}
type

  {--Single multipart Object------------------------------------}
  TALMultiPartAlternativeContent = class(TALMultiPartBaseContent)
  private
  public
  end;

  {--List Of multipart Objects------------------------------------}
  TALMultiPartAlternativeContents = class(TALMultiPartBaseContents)
  private
  protected
    function GetItem(Index: Integer): TALMultiPartAlternativeContent;
    procedure SetItem(Index: Integer; AObject: TALMultiPartAlternativeContent);
  public
    Function Add: TALMultiPartAlternativeContent; overload;
    function Add(AObject: TALMultiPartAlternativeContent): Integer; overload;
    function Remove(AObject: TALMultiPartAlternativeContent): Integer;
    function IndexOf(AObject: TALMultiPartAlternativeContent): Integer;
    procedure Insert(Index: Integer; AObject: TALMultiPartAlternativeContent);
    property Items[Index: Integer]: TALMultiPartAlternativecontent read GetItem write SetItem; default;
  end;

  {--TAlMultiPartAlternativeStream----------------------------}
  TAlMultiPartAlternativeStream = class(TAlMultiPartBaseStream)
  private
  public
    procedure AddContent(aContent: TALMultiPartAlternativeContent); reintroduce;
  end;

  {--TALMultipartAlternativeEncoder-----------------------------}
  TALMultipartAlternativeEncoder = class(TALMultipartBaseEncoder)
  private
  protected
    Function CreateDataStream: TAlMultiPartBaseStream; override;
    function GetDataStream: TAlMultiPartAlternativeStream; reintroduce;
  public
    property DataStream: TAlMultiPartAlternativeStream read GetDataStream;
  end;

  {--TALMultipartAlternativeDecoder-----------------------------}
  TALMultipartAlternativeDecoder = class(TALMultipartBaseDecoder)
  private
  protected
    function GetContents: TALMultiPartAlternativeContents; reintroduce;
    Function CreateContent: TALMultiPartBaseContent; override;
    Function CreateContents: TALMultiPartBaseContents; override;
  public
    property Contents: TALMultiPartAlternativeContents read GetContents;
  end;

{Below a sample of multipart/form-data message :
********************************************
Content-Type: multipart/form-data; boundary=---------------------------7d728842d0b36

-----------------------------7d728842d0b36
Content-Disposition: form-data; name="picture"; filename="C:\ud964D.tmp.jpg"
Content-Type: image/pjpeg

ÿØÿà ...
-----------------------------7d728842d0b36
Content-Disposition: form-data; name="field1"

avalue
-----------------------------7d728842d0b36
Content-Disposition: form-data; name="field2"

aValue
-----------------------------7d728842d0b36--
********************************************}

type

  {--Single multipart Object---------------------------------}
  TALMultiPartFormDataContent = class(TALMultiPartBaseContent)
  private
    function GetFieldName: AnsiString;
    function GetFileName: AnsiString;
    procedure SetfieldName(const aValue: AnsiString);
    procedure SetfileName(const aValue: AnsiString);
  public
    procedure LoadDataFromFile(const aFileName: AnsiString); override;
    procedure LoadDataFromStream(aStream: TStream); override;
    Property FieldName: AnsiString Read GetFieldName Write SetFieldName;
    Property FileName: AnsiString Read GetFileName Write SetfileName;
  end;

  {--List Of multipart Objects---------------------------------}
  TALMultiPartFormDataContents = class(TALMultiPartBaseContents)
  private
  protected
    function GetItem(Index: Integer): TALMultiPartFormDataContent;
    procedure SetItem(Index: Integer; AObject: TALMultiPartFormDataContent);
  public
    Function Add: TALMultiPartFormDataContent; overload;
    function Add(AObject: TALMultiPartFormDataContent): Integer; overload;
    function Remove(AObject: TALMultiPartFormDataContent): Integer;
    function IndexOf(AObject: TALMultiPartFormDataContent): Integer;
    procedure Insert(Index: Integer; AObject: TALMultiPartFormDataContent);
    property Items[Index: Integer]: TALMultiPartFormDatacontent read GetItem write SetItem; default;
  end;

  {--TAlMultiPartFormDataStream----------------------------}
  TAlMultiPartFormDataStream = class(TAlMultiPartBaseStream)
  private
  public
    procedure AddField(const aFieldName, aFieldValue: AnsiString);
    procedure AddFile(const aFieldName, aFileName, aContentType: AnsiString; aFileData: TStream); overload;
    procedure AddFile(const aFieldName, aFileName: AnsiString); overload;
    procedure AddContent(aContent: TALMultiPartFormDataContent); reintroduce;
  end;

  {--TALMultipartFormDataEncoder-----------------------------}
  TALMultipartFormDataEncoder = class(TALMultipartBaseEncoder)
  private
  protected
    Function CreateDataStream: TAlMultiPartBaseStream; override;
    function GetDataStream: TAlMultiPartFormDataStream; reintroduce;
  public
    procedure Encode(aContentFields: TALStrings; aContentFiles: TALMultiPartFormDataContents);
    property  DataStream: TAlMultiPartFormDataStream read GetDataStream;
  end;

  {--TALMultipartFormDataDecoder-----------------------------}
  TALMultipartFormDataDecoder = class(TALMultipartBaseDecoder)
  private
    FContentFiles: TALMultiPartFormDataContents;
    FContentFields: TALStrings;
    function GetContentFields: TALStrings;
    function GetContentFiles: TALMultiPartFormDataContents;
  protected
    function GetContents: TALMultiPartFormDataContents; reintroduce;
    Function CreateContent: TALMultiPartBaseContent; override;
    Function CreateContents: TALMultiPartBaseContents; override;
  public
    constructor Create; override;
    destructor  Destroy; override;
    procedure   Decode(const aDataStr, aboundary: AnsiString); overload; Override;
    procedure   Decode(const aDataStr, aboundary: AnsiString; aContentFields: TALStrings; aContentFiles: TALMultiPartFormDataContents); overload;
    property    ContentFiles: TALMultiPartFormDataContents read GetContentFiles;
    property    ContentFields: TALStrings read GetContentFields;
  end;

{Below a sample of multipart/mixed message :
********************************************
Mime-Version: 1.0
Content-Type: multipart/mixed;
	boundary="----=_NextPart_000_0145_01C81ECF.27E4F8C0"

This is a multi-part message in MIME format.

------=_NextPart_000_0145_01C81ECF.27E4F8C0
Content-Type: text/plain;
	charset="utf-8"
Content-Transfer-Encoding: base64

YyB1biB0ZXN0ICEhIQ==

------=_NextPart_000_0145_01C81ECF.27E4F8C0
Content-Type: image/bmp;
	name="Blue Lace 16.bmp"
Content-Transfer-Encoding: base64
Content-Disposition: attachment;
	filename="Blue Lace 16.bmp"

Qk34BAAAAAAAAHYAAAAoAAAAMAAAADAAAAABAAQAAAAAAAAAAADDDgAAww4AAAAAAAAAAAAAAAAA
AAAAgAAAgAAAAICAAIAAAACAAIAAgIAAAICAgADAwMAAAAD/AAD/AAAA//8A/wAAAP8A/wD//wAA
RMRERERExExEREREzMRERAAA

------=_NextPart_000_0145_01C81ECF.27E4F8C0--
********************************************}
type

  {--Single multipart Object------------------------------}
  TALMultiPartMixedContent = class(TALMultiPartBaseContent)
  private
    function GetAttachment: Boolean;
  public
    procedure LoadDataFromFileAsAttachmentBase64Encode(const aFileName: AnsiString); virtual;
    Property  IsAttachment: Boolean read GetAttachment;
  end;

  {--List Of multipart Objects------------------------------}
  TALMultiPartMixedContents = class(TALMultiPartBaseContents)
  private
  protected
    function GetItem(Index: Integer): TALMultiPartMixedContent;
    procedure SetItem(Index: Integer; AObject: TALMultiPartMixedContent);
  public
    Function Add: TALMultiPartMixedContent; overload;
    function Add(AObject: TALMultiPartMixedContent): Integer; overload;
    function Remove(AObject: TALMultiPartMixedContent): Integer;
    function IndexOf(AObject: TALMultiPartMixedContent): Integer;
    procedure Insert(Index: Integer; AObject: TALMultiPartMixedContent);
    property Items[Index: Integer]: TALMultiPartMixedcontent read GetItem write SetItem; default;
  end;

  {--TAlMultiPartMixedStream----------------------------}
  TAlMultiPartMixedStream = class(TAlMultiPartBaseStream)
  private
  public
    procedure AddInlineTextBase64Encode(const aContentType, aText: AnsiString);
    procedure AddAttachmentBase64Encode(const aFileName: AnsiString; const aContentType: AnsiString; aFileData: TStream); overload;
    procedure AddAttachmentBase64Encode(const aFileName: AnsiString); overload;
    procedure AddContent(aContent: TALMultiPartMixedContent); reintroduce;
  end;

  {--TALMultipartMixedEncoder-----------------------------}
  TALMultipartMixedEncoder = class(TALMultipartBaseEncoder)
  private
  protected
    Function CreateDataStream: TAlMultiPartBaseStream; override;
    function GetDataStream: TAlMultiPartMixedStream; reintroduce;
  public
    procedure Encode(const aInlineText: AnsiString;
                     const aInlineTextContentType: AnsiString;
                     aAttachments: TALMultiPartMixedContents); overload;
    property  DataStream: TAlMultiPartMixedStream read GetDataStream;
  end;

  {--TALMultipartMixedDecoder-----------------------------}
  TALMultipartMixedDecoder = class(TALMultipartBaseDecoder)
  private
  protected
    function GetContents: TALMultiPartMixedContents; reintroduce;
    Function CreateContent: TALMultiPartBaseContent; override;
    Function CreateContents: TALMultiPartBaseContents; override;
  public
    property Contents: TALMultiPartMixedContents read GetContents;
  end;

{---------------------------------------------------------------------------------------------}
Function ALMultipartExtractBoundaryFromContentType(const aContentType: AnsiString): AnsiString;
Function ALMultipartExtractValueFromHeaderLine(const aHeaderLine: AnsiString; const aName: AnsiString): AnsiString;
Function ALMultipartSetValueInHeaderLine(const aHeaderLine: AnsiString; const aName, AValue: AnsiString): AnsiString;

implementation

Uses
  System.SysUtils,
  System.Types, // to expand the inline function
  System.AnsiStrings,
  ALString,
  ALMime;

{*****************************************************************************************************************}
Function ALMultipartExtractValueFromHeaderLine(const aHeaderLine: AnsiString; const aName: AnsiString): AnsiString;

  {-----------------------------------------------------------}
  function _RemoveQuoteStr(const aStr: AnsiString): AnsiString;
  Begin
    Result := AStr;
    If (Length(result) > 0) and
       (result[1] in ['"','''']) and
       (result[1]=result[length(result)]) then result := AlCopyStr(Result,2,length(result)-2);
  end;

Var LLst: TALStringList;
    i: integer;

begin
  Result := '';
  LLst := TALStringList.Create;
  Try

    LLst.LineBreak := ';';
    LLst.Text := aHeaderLine;

    For i := 0 to LLst.Count - 1 do
      If ALSameText(ALTrim(LLst.Names[i]), aName) then begin
        Result := _RemoveQuoteStr(LLst.ValueFromIndex[i]);
        Break;
      end;

  finally
    LLst.Free;
  end;
end;

{*******************************************************************************************************************}
Function ALMultipartSetValueInHeaderLine(const aHeaderLine: AnsiString; const aName, AValue: AnsiString): AnsiString;
Var LLst: TALStringList;
    LFlag: Boolean;
    I: integer;
begin
  LLst := TALStringList.Create;
  Try

    LLst.LineBreak := ';';
    LLst.Text := aHeaderLine;

    LFlag := False;
    For I := 0 to LLst.Count - 1 do
      If AlSameText(ALTrim(LLst.Names[I]), aName) then begin
        LLst.ValueFromIndex[I] := '"' + AValue + '"';
        LFlag := True;
        Break;
      end;

    Result := '';
    For I := 0 to LLst.Count - 1 do begin
      if I = 0 then Result := Result + ALTrim(LLst[I])
      else Result := Result + '; ' + ALTrim(LLst[I]);
    end;

    if Not LFlag then begin
       if result <> '' then Result := Result + '; ';
       Result := Result + aName + '="' + aValue + '"'
    end;

  finally
    LLst.Free;
  end;
end;

{*********************************************************************************************}
Function ALMultipartExtractBoundaryFromContentType(const aContentType: AnsiString): AnsiString;
Begin
  Result := ALMultipartExtractValueFromHeaderLine(aContentType, 'boundary');
end;

{*****************************************}
constructor TALMultiPartBaseContent.Create;
begin
  inherited;
  FDataStream := TMemoryStream.Create;
  FCustomHeaders := TALStringList.create;
  FCustomHeaders.NameValueSeparator := ':';
  Clear;
end;

{*****************************************}
destructor TALMultiPartBaseContent.Destroy;
begin
  FDataStream.Free;
  FCustomHeaders.free;
  inherited;
end;

{**************************************}
procedure TALMultiPartBaseContent.clear;
begin
  FContentType := '';
  FContentTransferEncoding := '';
  FContentDisposition := '';
  FContentID := '';
  FContentDescription := '';
  FCustomHeaders.clear;
  TMemoryStream(FdataStream).Clear;
end;

{************************************************************}
function TALMultiPartBaseContent.GetRawHeaderText: AnsiString;
Var I : integer;
begin
  Result := '';
  If ALTrim(FContentType) <> '' then result := result + 'Content-Type: ' + ALTrim(FContentType) + #13#10;
  If ALTrim(FContentTransferEncoding) <> '' then result := result + 'Content-Transfer-Encoding: ' + ALTrim(FContentTransferEncoding) + #13#10;
  If ALTrim(fContentDisposition) <> '' then result := result + 'Content-Disposition: ' + ALTrim(fContentDisposition) + #13#10;
  If ALTrim(FContentID) <> '' then result := result + 'Content-ID: ' + ALTrim(FContentID) + #13#10;
  If ALTrim(FContentDescription) <> '' then result := result + 'Content-Description: ' + ALTrim(FContentDescription) + #13#10;
  For I := 0 to FCustomHeaders.count - 1 do
    if (ALTrim(FCustomHeaders.names[I]) <> '') and (ALTrim(FCustomHeaders.ValueFromIndex[I]) <> '') then
      result := result + FCustomHeaders.names[I] + ': ' + ALTrim(FCustomHeaders.ValueFromIndex[I]) + #13#10;
end;

{***********************************************************************************}
procedure TALMultiPartBaseContent.SetRawHeaderText(const aRawHeaderText: AnsiString);

Var LRawHeaderLst: TALStringList;

  {-------------------------------------------------------}
  Function _getHeader(const aName: AnsiString): AnsiString;
  Var I: Integer;
      Str: AnsiString;
  Begin
    I := LRawHeaderLst.IndexOfName(aName);
    If I >= 0 then Begin
      result := ALTrim(LRawHeaderLst.ValueFromIndex[I]);
      LRawHeaderLst.Delete(I);
      While True do begin
        If I >= LRawHeaderLst.Count then break;
        str := LRawHeaderLst[I];
        If (str = '') or
           (not (str[1] in [' ',#9])) then break; //(1) an empty line or (2) a line that does not start with a space, a tab, or a field name followed by a colon
        Result := ALTrim(result + ' ' + ALTrim(str));
        LRawHeaderLst.Delete(I);
      end;
    end
    else result := '';
  end;

Var Str1, Str2: AnsiString;
    J: integer;

begin
  Clear;
  LRawHeaderLst := TALStringList.create;
  try

    LRawHeaderLst.NameValueSeparator := ':';
    LRawHeaderLst.Text := aRawHeaderText;

    FContentType:= _getHeader('Content-Type');
    FContentTransferEncoding:= _getHeader('Content-Transfer-Encoding');
    fContentDisposition:= _getHeader('Content-Disposition');
    FContentID := _getHeader('Content-ID');
    FContentDescription := _getHeader('Content-Description');

    FCustomHeaders.clear;
    J := 0;
    while J <= LRawHeaderLst.count - 1 do begin
      Str1 := ALTrim(LRawHeaderLst.Names[J]);
      If (ALTrim(str1) <> '') and (not (str1[1] in [' ',#9])) then begin
        Str1 := ALTrim(Str1) + ': ' + ALTrim(LRawHeaderLst.ValueFromIndex[J]);
        inc(J);
        While True do begin
          If J >= LRawHeaderLst.Count then break;
          str2 := LRawHeaderLst[J];
          If (str2 = '') or
             (not (str2[1] in [' ',#9])) then break; //(1) an empty line or (2) a line that does not start with a space, a tab, or a field name followed by a colon
          Str1 := ALTrim(Str1 + ' ' + ALTrim(str2));
          inc(J);
        end;
        FCustomHeaders.Add(Str1);
      end
      else inc(J);
    end;

  finally
    LRawHeaderLst.Free;
  end;
end;

{*********************************************************}
function TALMultiPartBaseContent.GetDataString: AnsiString;
begin
  FdataStream.Position := 0;
  if FDataStream.Size = 0 then result := ''
  else begin
    SetLength(Result,FdataStream.size);
    FDataStream.ReadBuffer(pointer(Result)^,FdataStream.Size);
  end;
  FdataStream.Position := 0;
end;

{************************************************************************}
procedure TALMultiPartBaseContent.SetDataString(const aValue: AnsiString);
begin
  TmemoryStream(FdataStream).clear;
  if Length(aValue) > 0 then FDataStream.WriteBuffer(pointer(aValue)^,length(aValue));
  FdataStream.Position := 0;
end;

{******************************************************************************}
procedure TALMultiPartBaseContent.LoadDataFromFile(const aFileName: AnsiString);
begin
  TmemoryStream(FDataStream).LoadFromFile(String(aFileName));
  ContentType := ALGetDefaultMIMEContentTypeFromExt(ALExtractFileExt(aFileName));
  ContentTransferEncoding := 'binary';
end;

{******************************************************************************************}
procedure TALMultiPartBaseContent.LoadDataFromFileBase64Encode(const aFileName: AnsiString);
Var Buffer: AnsiString;
begin
  TMemoryStream(FDataStream).clear;
  Buffer := ALBase64EncodeStringMIME(AlGetStringFromFile(aFileName));
  FDataStream.WriteBuffer(pointer(Buffer)^, length(Buffer));
  FDataStream.Position := 0;
  ContentType := ALGetDefaultMIMEContentTypeFromExt(ALExtractFileExt(aFileName));
  ContentTransferEncoding := 'base64';
end;

{*********************************************************************}
procedure TALMultiPartBaseContent.LoadDataFromStream(aStream: TStream);
Begin
  TmemoryStream(FdataStream).LoadFromStream(aStream);
  ContentTransferEncoding := 'binary';
end;

{*********************************************************************************}
procedure TALMultiPartBaseContent.LoadDataFromStreamBase64Encode(aStream: TStream);
Var Buffer: AnsiString;
Begin
  aStream.Position := 0;
  SetLength(Buffer,aStream.size);
  aStream.ReadBuffer(pointer(Buffer)^,aStream.size);
  Buffer := ALBase64EncodeStringMIME(Buffer);
  TMemoryStream(FDataStream).clear;
  FDataStream.WriteBuffer(pointer(Buffer)^, length(Buffer));
  FDataStream.Position := 0;
  ContentTransferEncoding := 'base64';
end;

{****************************************************************************}
procedure TALMultiPartBaseContent.SaveDataToFile(const aFilename: AnsiString);
begin
  TMemoryStream(FDataStream).SaveToFile(String(aFileName));
end;

{*******************************************************************}
procedure TALMultiPartBaseContent.SaveDataToStream(aStream: TStream);
begin
  TMemoryStream(FDataStream).SaveToStream(aStream);
end;

{****************************************************************************************}
procedure TALMultiPartBaseContent.SaveDataToFileBase64Decode(const aFileName: AnsiString);
Var Buffer: AnsiString;
begin
  FDataStream.Position := 0;
  SetLength(Buffer,FdataStream.size);
  FDataStream.ReadBuffer(pointer(Buffer)^,FdataStream.size);
  AlSaveStringToFile(ALBase64DecodeStringMIME(Buffer),aFileName);
end;

{*******************************************************************************}
procedure TALMultiPartBaseContent.SaveDataToStreamBase64Decode(aStream: TStream);
Var Buffer: AnsiString;
begin
  FDataStream.Position := 0;
  SetLength(Buffer,FdataStream.size);
  FDataStream.ReadBuffer(pointer(Buffer)^,FdataStream.size);
  Buffer := ALBase64DecodeStringMIME(Buffer);
  aStream.WriteBuffer(pointer(Buffer)^, Length(Buffer));
end;

{*******************************************************************************}
function TALMultiPartBaseContents.Add(AObject: TALMultiPartBaseContent): Integer;
begin
  Result := inherited Add(AObject);
end;

{*************************************************************}
function TALMultiPartBaseContents.Add: TALMultiPartBaseContent;
begin
  Result := TALMultiPartBaseContent.Create;
  Try
    add(result);
  except
    Result.Free;
    raise;
  end;
end;

{*********************************************************************************}
function TALMultiPartBaseContents.GetItem(Index: Integer): TALMultiPartBaseContent;
begin
  Result := TALMultiPartBaseContent(inherited Items[Index]);
end;

{***********************************************************************************}
function TALMultiPartBaseContents.IndexOf(AObject: TALMultiPartBaseContent): Integer;
begin
  Result := inherited IndexOf(AObject);
end;

{******************************************************************************************}
procedure TALMultiPartBaseContents.Insert(Index: Integer; AObject: TALMultiPartBaseContent);
begin
  inherited Insert(Index, AObject);
end;

{**********************************************************************************}
function TALMultiPartBaseContents.Remove(AObject: TALMultiPartBaseContent): Integer;
begin
  Result := inherited Remove(AObject);
end;

{*******************************************************************************************}
procedure TALMultiPartBaseContents.SetItem(Index: Integer; AObject: TALMultiPartBaseContent);
begin
  inherited Items[Index] := AObject;
end;

{****************************************}
constructor TAlMultiPartBaseStream.Create;
begin
  inherited;
  FBoundary := GenerateUniqueBoundary;
end;

{*****************************************************************************}
procedure TAlMultiPartBaseStream.AddContent(aContent: TALMultiPartBaseContent);
var sFormFieldInfo: AnsiString;
begin
  sFormFieldInfo := #13#10 +
                    '--' + Boundary + #13#10 +
                    aContent.RawHeaderText + #13#10;

  Write(Pointer(sFormFieldInfo)^, Length(sFormFieldInfo));
  CopyFrom(aContent.DataStream,0);
end;

{*****************************************************************}
function TAlMultiPartBaseStream.GenerateUniqueBoundary: AnsiString;
begin
  Result := '---------------------------' + ALFormatDateTime('mmddyyhhnnsszzz', Now, ALDefaultFormatSettings);
end;

{*********************************************}
procedure TAlMultiPartBaseStream.CloseBoundary;
var sFormFieldInfo: AnsiString;
begin
  sFormFieldInfo := #13#10 +
                    '--' + Boundary + '--' + #13#10;
  Write(Pointer(sFormFieldInfo)^, Length(sFormFieldInfo));
end;

{*****************************************}
constructor TALMultipartBaseEncoder.Create;
begin
  inherited;
  FDataStream := CreateDataStream;
end;

{*****************************************}
destructor TALMultipartBaseEncoder.Destroy;
begin
  FDataStream.Free;
  inherited;
end;

{*********************************************************************}
function TALMultipartBaseEncoder.GetDataStream: TAlMultiPartBaseStream;
begin
  Result := FdataStream;
end;

{****************************************************************************}
procedure TALMultipartBaseEncoder.Encode(acontents: TALMultiPartBaseContents);
Var I: Integer;
begin
  with FDataStream do begin
    Clear;
    For I := 0 to acontents.Count - 1 do
      AddContent(acontents[I]);
    CloseBoundary;
  end;
end;

{*****************************************}
constructor TALMultipartBaseDecoder.Create;
begin
  Inherited;
  FContents:= CreateContents;
  Fcontents.OwnsObjects := True;
end;

{*****************************************}
destructor TALMultipartBaseDecoder.Destroy;
begin
  FContents.Free;
  inherited;
end;

{*********************************************************************}
function TALMultipartBaseDecoder.GetContents: TALMultiPartBaseContents;
begin
  Result := Fcontents;
end;

{******************************************************************************************}
procedure TALMultipartBaseDecoder.Decode(aDataStream: Tstream; const aboundary: AnsiString);
var sBuffer: AnsiString;
begin
  aDataStream.Position := 0;
  SetLength(sBuffer, aDataStream.Size);
  aDataStream.ReadBuffer(Pointer(sBuffer)^, aDataStream.Size);
  Decode(sBuffer, aboundary);
end;

{******************************************************************************}
procedure TALMultipartBaseDecoder.Decode(const aDataStr, aboundary: AnsiString);
var LLnBoundary: Integer;
    LContent: TALMultiPartBaseContent;
    LFlag: Boolean;
    P1, P2, P3: Integer;
begin

  {clear the fContent}
  FContents.Clear;

  {init LnBoundary}
  LLnBoundary := length(aBoundary);

  {Find the first Boundary}
  P1 := AlPos('--'+aBoundary+#13#10, aDataStr);
  LFlag := P1 > 0;
  Dec(P1,2);

  {start the loop on all Boundary}
  While LFlag Do begin
    LContent := CreateContent;
    With LContent do begin

      {Add the Content to the Contents}
      FContents.Add(LContent);

      {move P1 to the start of the header}
      P1 := P1 + LLnBoundary + 6;

      {Find the next Boundary}
      P3 := AlPosEx(#13#10+'--'+aBoundary+#13#10, aDataStr, P1);
      if P3 <= 0 then Begin
        LFlag := False;
        P3 := AlPosEx(#13#10+'--'+aBoundary+'--', aDataStr, P1);
        if P3 <= 0 then raise Exception.Create('Wrong MultiPart Content');
      end;

      {the the next 2 breakline that show the end of the header}
      P2 := AlPosEx(#13#10#13#10, aDataStr, P1);
      IF (P2 <= 0) or (P2 >= P3) then raise Exception.Create('Wrong MultiPart Content');
      RawHeaderText := alcopyStr(aDataStr, P1, P2 - 1);

      {write the body to the content}
      P2 := P2 + 4;
      DataStream.WriteBuffer(aDataStr[P2], P3-P2);
      DataStream.Position := 0;

      {move P1 to the newt boundary}
      P1 := P3;
    end;
  end;

end;

{*********************************************************************************************}
function TALMultiPartAlternativeContents.Add(AObject: TALMultiPartAlternativeContent): Integer;
begin
  Result := inherited Add(AObject);
end;

{***************************************************************************}
function TALMultiPartAlternativeContents.Add: TALMultiPartAlternativeContent;
begin
  Result := TALMultiPartAlternativeContent.Create;
  Try
    add(result);
  except
    Result.Free;
    raise;
  end;
end;

{***********************************************************************************************}
function TALMultiPartAlternativeContents.GetItem(Index: Integer): TALMultiPartAlternativeContent;
begin
  Result := TALMultiPartAlternativeContent(inherited Items[Index]);
end;

{*************************************************************************************************}
function TALMultiPartAlternativeContents.IndexOf(AObject: TALMultiPartAlternativeContent): Integer;
begin
  Result := inherited IndexOf(AObject);
end;

{********************************************************************************************************}
procedure TALMultiPartAlternativeContents.Insert(Index: Integer; AObject: TALMultiPartAlternativeContent);
begin
  inherited Insert(Index, AObject);
end;

{************************************************************************************************}
function TALMultiPartAlternativeContents.Remove(AObject: TALMultiPartAlternativeContent): Integer;
begin
  Result := inherited Remove(AObject);
end;

{*********************************************************************************************************}
procedure TALMultiPartAlternativeContents.SetItem(Index: Integer; AObject: TALMultiPartAlternativeContent);
begin
  inherited Items[Index] := AObject;
end;

{*******************************************************************************************}
procedure TAlMultiPartAlternativeStream.AddContent(aContent: TALMultiPartAlternativeContent);
Var sFormFieldInfo: AnsiString;
begin
  If Position = 0 then Begin
    sFormFieldInfo := #13#10 +
                      'This is a multi-part message in MIME format.'+#13#10;
    Write(Pointer(sFormFieldInfo)^, Length(sFormFieldInfo));
  end;

  Inherited AddContent(aContent);
end;

{*******************************************************************************}
function TALMultipartAlternativeEncoder.CreateDataStream: TAlMultiPartBaseStream;
begin
  Result := TAlMultiPartAlternativeStream.Create;
end;

{***********************************************************************************}
function TALMultipartAlternativeEncoder.GetDataStream: TAlMultiPartAlternativeStream;
begin
  Result := TAlMultiPartAlternativeStream(inherited GetDataStream);
end;

{*****************************************************************************}
function TALMultipartAlternativeDecoder.CreateContent: TALMultiPartBaseContent;
begin
  Result := TALMultiPartAlternativeContent.Create;
end;

{*******************************************************************************}
function TALMultipartAlternativeDecoder.CreateContents: TALMultiPartBaseContents;
begin
  Result := TALMultiPartAlternativeContents.Create(true);
end;

{***********************************************************************************}
function TALMultipartAlternativeDecoder.GetContents: TALMultiPartAlternativeContents;
begin
  Result := TALMultiPartAlternativeContents(inherited GetContents);
end;

{************************************************************}
function TALMultiPartFormDataContent.GetFieldName: AnsiString;
begin
  Result := ALMultipartExtractValueFromHeaderLine(ContentDisposition, 'name');
end;

{***********************************************************}
function TALMultiPartFormDataContent.GetFileName: AnsiString;
begin
  Result := ALMultipartExtractValueFromHeaderLine(ContentDisposition, 'filename');
end;

{**********************************************************************************}
procedure TALMultiPartFormDataContent.LoadDataFromFile(const aFileName: AnsiString);
begin
  inherited LoadDataFromFile(aFileName);
  ContentTransferEncoding := '';
end;

{*************************************************************************}
procedure TALMultiPartFormDataContent.LoadDataFromStream(aStream: TStream);
begin
  inherited LoadDataFromStream(aStream);
  ContentTransferEncoding := '';
end;

{**************************************************************************}
procedure TALMultiPartFormDataContent.SetfieldName(const aValue: AnsiString);
begin
  ContentDisposition := ALMultipartSetValueInHeaderLine(ContentDisposition, 'name', aValue);
end;

{**************************************************************************}
procedure TALMultiPartFormDataContent.SetfileName(const aValue: AnsiString);
begin
  ContentDisposition := ALMultipartSetValueInHeaderLine(ContentDisposition, 'filename', aValue);
end;

{***************************************************************************************}
function TALMultiPartFormDataContents.Add(AObject: TALMultiPartFormDataContent): Integer;
begin
  Result := inherited Add(AObject);
end;

{*********************************************************************}
function TALMultiPartFormDataContents.Add: TALMultiPartFormDataContent;
begin
  Result := TALMultiPartFormDataContent.Create;
  Try
    add(result);
  except
    Result.Free;
    raise;
  end;
end;

{*****************************************************************************************}
function TALMultiPartFormDataContents.GetItem(Index: Integer): TALMultiPartFormDataContent;
begin
  Result := TALMultiPartFormDataContent(inherited Items[Index]);
end;

{*******************************************************************************************}
function TALMultiPartFormDataContents.IndexOf(AObject: TALMultiPartFormDataContent): Integer;
begin
  Result := inherited IndexOf(AObject);
end;

{**************************************************************************************************}
procedure TALMultiPartFormDataContents.Insert(Index: Integer; AObject: TALMultiPartFormDataContent);
begin
  inherited Insert(Index, AObject);
end;

{******************************************************************************************}
function TALMultiPartFormDataContents.Remove(AObject: TALMultiPartFormDataContent): Integer;
begin
  Result := inherited Remove(AObject);
end;

{***************************************************************************************************}
procedure TALMultiPartFormDataContents.SetItem(Index: Integer; AObject: TALMultiPartFormDataContent);
begin
  inherited Items[Index] := AObject;
end;

{**********************************************************************************************************************}
procedure TAlMultiPartFormDataStream.AddFile(const aFieldName, aFileName, aContentType: AnsiString; aFileData: TStream);
Var LContent: TALMultiPartFormDataContent;
begin
  LContent := TALMultiPartFormDataContent.Create;
  Try
    LContent.LoadDataFromStream(aFileData);
    LContent.ContentType := aContentType;
    LContent.ContentDisposition := 'form-data; name="'+aFieldName+'"; filename="'+aFileName+'"';
    AddContent(LContent);
  Finally
    LContent.Free;
  end;
end;

{************************************************************************************}
procedure TAlMultiPartFormDataStream.AddFile(const aFieldName, aFileName: AnsiString);
Var LContent: TALMultiPartFormDataContent;
begin
  LContent := TALMultiPartFormDataContent.Create;
  Try
    LContent.LoadDataFromFile(aFileName);
    LContent.ContentDisposition := 'form-data; name="'+aFieldName+'"; filename="'+aFileName+'"';
    AddContent(LContent);
  Finally
    LContent.Free;
  end;
end;

{***************************************************************************************}
procedure TAlMultiPartFormDataStream.AddField(const aFieldName, aFieldValue: AnsiString);
Var LContent: TALMultiPartFormDataContent;
    LStringStream: TALStringStream;
begin
  LStringStream:= TALStringStream.Create(aFieldValue);
  LContent := TALMultiPartFormDataContent.Create;
  Try
    LContent.LoadDataFromStream(LStringStream);
    LContent.ContentDisposition := 'form-data; name="'+aFieldName+'"';
    AddContent(LContent);
  Finally
    LContent.Free;
    LStringStream.free;
  end;
end;

{*************************************************************************************}
procedure TAlMultiPartFormDataStream.AddContent(aContent: TALMultiPartFormDataContent);
begin
  Inherited AddContent(aContent);
end;

{****************************************************************************}
function TALMultipartFormDataEncoder.CreateDataStream: TAlMultiPartBaseStream;
begin
  Result := TAlMultiPartFormDataStream.Create;
end;

{*****************************************************************************}
function TALMultipartFormDataEncoder.GetDataStream: TAlMultiPartFormDataStream;
begin
  Result := TAlMultiPartFormDataStream(inherited GetDataStream);
end;

{********************************************************************************************************************}
procedure TALMultipartFormDataEncoder.Encode(aContentFields: TALStrings; aContentFiles: TALMultiPartFormDataContents);
Var I: Integer;
begin
  with TAlMultiPartFormDataStream(DataStream) do begin
    Clear;
    If assigned(aContentFiles) then
      For I := 0 to aContentFiles.Count - 1 do
        AddContent(aContentFiles[I]);
    If assigned(aContentFields) then
      With aContentFields do
        For I := 0 to Count - 1 do
          AddField(Names[I],ValueFromIndex[I]);
    CloseBoundary;
  end;
end;

{*********************************************}
constructor TALMultipartFormDataDecoder.Create;
begin
  inherited;
  FContentFiles := TALMultiPartFormDataContents.Create(False);
  FContentFields := TALStringList.Create;
end;

{*********************************************}
destructor TALMultipartFormDataDecoder.Destroy;
begin
  FContentFiles.Free;
  FContentFields.Free;
  inherited;
end;

{**********************************************************************************}
procedure TALMultipartFormDataDecoder.Decode(const aDataStr, aboundary: AnsiString);
Var LContents: TALMultiPartFormDataContents;
    I: integer;
begin
  //Update the Fcontent
  inherited Decode(aDataStr, aboundary);

  //clear the FContentFiles and FContentFields
  FContentFiles.Clear;
  FContentFields.Clear;

  //loop on all contents
  LContents := GetContents;
  For I := 0 to LContents.Count - 1 do begin
    If (LContents[I].FileName <> '') then FContentFiles.Add(LContents[I])             // if Filename or contentType set them assume its File
    else FContentFields.Add(LContents[I].FieldName + '=' + LContents[I].DataString);  // it's a field value
  end;
end;

{*********************************************************************************}
procedure TALMultipartFormDataDecoder.Decode(const aDataStr, aboundary: AnsiString;
                                             aContentFields: TALStrings;
                                             aContentFiles: TALMultiPartFormDataContents);
Var LContents: TALMultiPartFormDataContents;
begin
  //Update the Fcontent
  inherited Decode(aDataStr, aboundary);

  //clear the aContentFiles and aContentFields
  aContentFields.Clear;
  aContentFiles.Clear;

  //loop on all contents
  LContents := GetContents;
  While LContents.Count > 0 do begin
    If (LContents[0].FileName <> '') then aContentFiles.Add(LContents.Extract(LContents[0]))  // if Filename or contentType set them assume its File
    else begin
      aContentFields.Add(LContents[0].FieldName + '=' + LContents[0].DataString);             // it's a field value
      LContents.Delete(0);
    end;
  end;
end;

{**************************************************************************}
function TALMultipartFormDataDecoder.CreateContent: TALMultiPartBaseContent;
begin
  Result := TALMultiPartFormDataContent.Create;
end;

{****************************************************************************}
function TALMultipartFormDataDecoder.CreateContents: TALMultiPartBaseContents;
begin
  Result := TALMultiPartFormDataContents.Create(true);
end;

{*****************************************************************************}
function TALMultipartFormDataDecoder.GetContents: TALMultiPartFormDataContents;
begin
  Result := TALMultiPartFormDataContents(inherited GetContents);
end;

{****************************************************************}
function TALMultipartFormDataDecoder.GetContentFields: TALStrings;
begin
  Result := fContentFields;
end;

{*********************************************************************************}
function TALMultipartFormDataDecoder.GetContentFiles: TALMultiPartFormDataContents;
begin
  Result := fContentFiles;
end;

{*******************************************************}
function TALMultiPartMixedContent.GetAttachment: Boolean;
Var Lst: TALStringList;
    I: integer;
begin
  Result := False;
  Lst := TALStringList.Create;
  Try
    Lst.Text := AlStringReplace(ContentDisposition,';',#13#10,[RfReplaceAll]);
    For I := 0 to Lst.Count - 1 do
      If AlLowerCase(ALTrim(Lst[I])) = 'attachment' then begin
        Result := True;
        Break;
      end;
  finally
    Lst.Free;
  end;
end;

{*******************************************************************************************************}
procedure TALMultiPartMixedContent.LoadDataFromFileAsAttachmentBase64Encode(const aFileName: AnsiString);
var LFilenameWithoutPath: AnsiString;
begin
  LoadDataFromFileBase64Encode(aFileName);
  LFilenameWithoutPath := ALExtractFileName(aFileName);
  ContentType := ContentType + '; name="'+LFilenameWithoutPath+'"';
  ContentDisposition := 'attachment; filename="'+LFilenameWithoutPath+'"';
end;

{**********************************************************************************}
function TALMultiPartMixedContents.Add(AObject: TALMultiPartMixedContent): Integer;
begin
  Result := inherited Add(AObject);
end;

{***************************************************************}
function TALMultiPartMixedContents.Add: TALMultiPartMixedContent;
begin
  Result := TALMultiPartMixedContent.Create;
  Try
    add(result);
  except
    Result.Free;
    raise;
  end;
end;

{***********************************************************************************}
function TALMultiPartMixedContents.GetItem(Index: Integer): TALMultiPartMixedContent;
begin
  Result := TALMultiPartMixedContent(inherited Items[Index]);
end;

{*************************************************************************************}
function TALMultiPartMixedContents.IndexOf(AObject: TALMultiPartMixedContent): Integer;
begin
  Result := inherited IndexOf(AObject);
end;

{********************************************************************************************}
procedure TALMultiPartMixedContents.Insert(Index: Integer; AObject: TALMultiPartMixedContent);
begin
  inherited Insert(Index, AObject);
end;

{************************************************************************************}
function TALMultiPartMixedContents.Remove(AObject: TALMultiPartMixedContent): Integer;
begin
  Result := inherited Remove(AObject);
end;

{*********************************************************************************************}
procedure TALMultiPartMixedContents.SetItem(Index: Integer; AObject: TALMultiPartMixedContent);
begin
  inherited Items[Index] := AObject;
end;

{*******************************************************************************************************************************************}
procedure TAlMultiPartMixedStream.AddAttachmentBase64Encode(const aFileName: AnsiString; const aContentType: AnsiString; aFileData: TStream);
Var LContent: TALMultiPartMixedContent;
    LFilenameWithoutPath: AnsiString;
begin
  LContent := TALMultiPartMixedContent.Create;
  Try
    LContent.LoadDataFromStreamBase64Encode(aFileData);
    LFilenameWithoutPath := ALExtractFileName(aFileName);
    LContent.ContentType := aContentType + '; name="'+LFilenameWithoutPath+'"';
    LContent.ContentDisposition := 'attachment; filename="'+LFilenameWithoutPath+'"';
    AddContent(LContent);
  Finally
    LContent.Free;
  end;
end;

{***************************************************************************************}
procedure TAlMultiPartMixedStream.AddAttachmentBase64Encode(const aFileName: AnsiString);
Var LContent: TALMultiPartMixedContent;
begin
  LContent := TALMultiPartMixedContent.Create;
  Try
    LContent.LoadDataFromFileAsAttachmentBase64Encode(aFileName);
    AddContent(LContent);
  Finally
    LContent.Free;
  end;
end;

{*************************************************************************************************}
procedure TAlMultiPartMixedStream.AddInlineTextBase64Encode(const aContentType, aText: AnsiString);
Var LContent: TALMultiPartMixedContent;
    LStringStream: TALStringStream;
begin
  LContent := TALMultiPartMixedContent.Create;
  LStringStream := TALStringStream.Create(aText);
  Try
    LContent.LoadDataFromStreamBase64Encode(LStringStream);
    LContent.ContentType := aContentType;
    AddContent(LContent);
  Finally
    LContent.Free;
    LStringStream.Free;
  end;
end;

{*******************************************************************************}
procedure TAlMultiPartMixedStream.AddContent(aContent: TALMultiPartMixedContent);
Var sFormFieldInfo: AnsiString;
begin
  If Position = 0 then Begin
    sFormFieldInfo := #13#10'This is a multi-part message in MIME format.'#13#10;
    Write(Pointer(sFormFieldInfo)^, Length(sFormFieldInfo));
  end;
  Inherited AddContent(aContent);
end;

{*************************************************************************}
function TALMultipartMixedEncoder.CreateDataStream: TAlMultiPartBaseStream;
begin
  Result := TAlMultiPartMixedStream.Create;
end;

{***********************************************************************}
function TALMultipartMixedEncoder.GetDataStream: TAlMultiPartMixedStream;
begin
  Result := TAlMultiPartMixedStream(inherited GetDataStream);
end;

{**********************************************************************}
procedure TALMultipartMixedEncoder.Encode(const aInlineText: AnsiString;
                                          const aInlineTextContentType: AnsiString;
                                          aAttachments: TALMultiPartMixedContents);
Var I: Integer;
begin
  with TAlMultiPartMixedStream(DataStream) do begin
    Clear;
    AddInlineTextBase64Encode(aInlineTextContentType, aInlineText);
    If assigned(aAttachments) then
      For I := 0 to aAttachments.Count - 1 do
        AddContent(aAttachments[I]);
    CloseBoundary;
  end;
end;

{***********************************************************************}
function TALMultipartMixedDecoder.CreateContent: TALMultiPartBaseContent;
begin
  Result := TALMultiPartMixedContent.Create;
end;

{*************************************************************************}
function TALMultipartMixedDecoder.CreateContents: TALMultiPartBaseContents;
begin
  Result := TALMultiPartMixedContents.Create(true);
end;

{***********************************************************************}
function TALMultipartMixedDecoder.GetContents: TALMultiPartMixedContents;
begin
  Result := TALMultiPartMixedContents(inherited GetContents);
end;

end.
