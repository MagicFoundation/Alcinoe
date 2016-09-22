{*************************************************************
www:          http://sourceforge.net/projects/alcinoe/              
svn:          svn checkout svn://svn.code.sf.net/p/alcinoe/code/ alcinoe-code              
Author(s):    Stéphane Vander Clock (skype/email: svanderclock@yahoo.fr)
							
product:      ALMultiPartBaseParser
Version:      4.00

Description:  MultiPart objects to encode or decode stream
              in mime multipart/xxx format.

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

History :     26/06/2012: Add xe2 support

Link :        http://msdn.microsoft.com/library/default.asp?url=/library/en-us/cdosys/html/7a18a98b-3a18-45b2-83a9-28a8f4099970.asp
              http://www.ietf.org/rfc/rfc2646.txt
              http://www.w3.org/TR/REC-html40/interact/forms.html#h-17.1
              http://www.ietf.org/rfc/rfc1867.txt
              http://www.ietf.org/rfc/rfc2388.txt
              http://www.w3.org/MarkUp/html-spec/html-spec_8.html

**************************************************************}
unit ALMultiPartParser;

interface

{$IF CompilerVersion >= 25} {Delphi XE4}
  {$LEGACYIFEND ON} // http://docwiki.embarcadero.com/RADStudio/XE4/en/Legacy_IFEND_(Delphi)
{$IFEND}

Uses System.Classes,
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

Uses System.SysUtils,
     System.Types, // to expand the inline function
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

Var aLst: TALStringList;
    i: integer;

begin
  Result := '';
  aLst := TALStringList.Create;
  Try

    aLst.LineBreak := ';';
    aLst.Text := aHeaderLine;

    For i := 0 to aLst.Count - 1 do
      If ALSameText(ALTrim(aLst.Names[i]), aName) then begin
        Result := _RemoveQuoteStr(aLst.ValueFromIndex[i]);
        Break;
      end;

  finally
    aLst.Free;
  end;
end;

{*******************************************************************************************************************}
Function ALMultipartSetValueInHeaderLine(const aHeaderLine: AnsiString; const aName, AValue: AnsiString): AnsiString;
Var aLst: TALStringList;
    aFlag: Boolean;
    i: integer;
begin
  aLst := TALStringList.Create;
  Try

    aLst.LineBreak := ';';
    aLst.Text := aHeaderLine;

    aFlag := False;
    For i := 0 to aLst.Count - 1 do
      If AlSameText(ALTrim(aLst.Names[i]), aName) then begin
        aLst.ValueFromIndex[i] := '"' + AValue + '"';
        aFlag := True;
        Break;
      end;

    Result := '';
    For i := 0 to aLst.Count - 1 do begin
      if i = 0 then Result := Result + ALTrim(aLst[i])
      else Result := Result + '; ' + ALTrim(aLst[i]);
    end;

    if Not aFlag then begin
       if result <> '' then Result := Result + '; ';
       Result := Result + aName + '="' + aValue + '"'
    end;

  finally
    aLst.Free;
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
Var i : integer;
begin
  Result := '';
  If ALTrim(FContentType) <> '' then result := result + 'Content-Type: ' + ALTrim(FContentType) + #13#10;
  If ALTrim(FContentTransferEncoding) <> '' then result := result + 'Content-Transfer-Encoding: ' + ALTrim(FContentTransferEncoding) + #13#10;
  If ALTrim(fContentDisposition) <> '' then result := result + 'Content-Disposition: ' + ALTrim(fContentDisposition) + #13#10;
  If ALTrim(FContentID) <> '' then result := result + 'Content-ID: ' + ALTrim(FContentID) + #13#10;
  If ALTrim(FContentDescription) <> '' then result := result + 'Content-Description: ' + ALTrim(FContentDescription) + #13#10;
  For i := 0 to FCustomHeaders.count - 1 do
    if (ALTrim(FCustomHeaders.names[i]) <> '') and (ALTrim(FCustomHeaders.ValueFromIndex[i]) <> '') then
      result := result + FCustomHeaders.names[i] + ': ' + ALTrim(FCustomHeaders.ValueFromIndex[i]) + #13#10;
end;

{***********************************************************************************}
procedure TALMultiPartBaseContent.SetRawHeaderText(const aRawHeaderText: AnsiString);

Var aRawHeaderLst: TALStringList;

  {-------------------------------------------------------}
  Function _getHeader(const aName: AnsiString): AnsiString;
  Var i: Integer;
      Str: AnsiString;
  Begin
    I := aRawHeaderLst.IndexOfName(aName);
    If I >= 0 then Begin
      result := ALTrim(aRawHeaderLst.ValueFromIndex[i]);
      aRawHeaderLst.Delete(i);
      While True do begin
        If i >= aRawHeaderLst.Count then break;
        str := aRawHeaderLst[i];
        If (str = '') or
           (not (str[1] in [' ',#9])) then break; //(1) an empty line or (2) a line that does not start with a space, a tab, or a field name followed by a colon
        Result := ALTrim(result + ' ' + ALTrim(str));
        aRawHeaderLst.Delete(i);
      end;
    end
    else result := '';
  end;

Var Str1, Str2: AnsiString;
    j: integer;

begin
  Clear;
  aRawHeaderLst := TALStringList.create;
  try

    aRawHeaderLst.NameValueSeparator := ':';
    aRawHeaderLst.Text := aRawHeaderText;

    FContentType:= _getHeader('Content-Type');
    FContentTransferEncoding:= _getHeader('Content-Transfer-Encoding');
    fContentDisposition:= _getHeader('Content-Disposition');
    FContentID := _getHeader('Content-ID');
    FContentDescription := _getHeader('Content-Description');

    FCustomHeaders.clear;
    J := 0;
    while j <= aRawHeaderLst.count - 1 do begin
      Str1 := ALTrim(aRawHeaderLst.Names[j]);
      If (ALTrim(str1) <> '') and (not (str1[1] in [' ',#9])) then begin
        Str1 := ALTrim(Str1) + ': ' + ALTrim(aRawHeaderLst.ValueFromIndex[j]);
        inc(j);
        While True do begin
          If j >= aRawHeaderLst.Count then break;
          str2 := aRawHeaderLst[j];
          If (str2 = '') or
             (not (str2[1] in [' ',#9])) then break; //(1) an empty line or (2) a line that does not start with a space, a tab, or a field name followed by a colon
          Str1 := ALTrim(Str1 + ' ' + ALTrim(str2));
          inc(j);
        end;
        FCustomHeaders.Add(Str1);
      end
      else inc(j);
    end;

  finally
    aRawHeaderLst.Free;
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
  Buffer := ALMimeEncodeString(AlGetStringFromFile(aFileName));
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
  Buffer := ALMimeEncodeString(Buffer);
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
  AlSaveStringToFile(ALMimeDecodeString(Buffer),aFileName);
end;

{*******************************************************************************}
procedure TALMultiPartBaseContent.SaveDataToStreamBase64Decode(aStream: TStream);
Var Buffer: AnsiString;
begin
  FDataStream.Position := 0;
  SetLength(Buffer,FdataStream.size);
  FDataStream.ReadBuffer(pointer(Buffer)^,FdataStream.size);
  Buffer := ALMimeDecodeString(Buffer);
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
Var i: Integer;
begin
  with FDataStream do begin
    Clear;
    For i := 0 to acontents.Count - 1 do
      AddContent(acontents[i]);
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
var aLnBoundary: Integer;
    aContent: TALMultiPartBaseContent;
    aFlag: Boolean;
    P1, P2, P3: Integer;
begin

  {clear the fContent}
  FContents.Clear;

  {init LnBoundary}
  aLnBoundary := length(aBoundary);

  {Find the first Boundary}
  P1 := AlPos('--'+aBoundary+#13#10, aDataStr);
  aFlag := P1 > 0;
  Dec(P1,2);

  {start the loop on all Boundary}
  While aFlag Do begin
    aContent := CreateContent;
    With aContent do begin

      {Add the Content to the Contents}
      FContents.Add(aContent);

      {move P1 to the start of the header}
      P1 := P1 + aLnBoundary + 6;

      {Find the next Boundary}
      P3 := AlPosEx(#13#10+'--'+aBoundary+#13#10, aDataStr, P1);
      if P3 <= 0 then Begin
        aFlag := False;
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
Var aContent: TALMultiPartFormDataContent;
begin
  aContent := TALMultiPartFormDataContent.Create;
  Try
    aContent.LoadDataFromStream(aFileData);
    aContent.ContentType := aContentType;
    aContent.ContentDisposition := 'form-data; name="'+aFieldName+'"; filename="'+aFileName+'"';
    AddContent(aContent);
  Finally
    aContent.Free;
  end;
end;

{************************************************************************************}
procedure TAlMultiPartFormDataStream.AddFile(const aFieldName, aFileName: AnsiString);
Var aContent: TALMultiPartFormDataContent;
begin
  aContent := TALMultiPartFormDataContent.Create;
  Try
    aContent.LoadDataFromFile(aFileName);
    aContent.ContentDisposition := 'form-data; name="'+aFieldName+'"; filename="'+aFileName+'"';
    AddContent(aContent);
  Finally
    aContent.Free;
  end;
end;

{***************************************************************************************}
procedure TAlMultiPartFormDataStream.AddField(const aFieldName, aFieldValue: AnsiString);
Var aContent: TALMultiPartFormDataContent;
    aStringStream: TALStringStream;
begin
  aStringStream:= TALStringStream.Create(aFieldValue);
  aContent := TALMultiPartFormDataContent.Create;
  Try
    aContent.LoadDataFromStream(aStringStream);
    aContent.ContentDisposition := 'form-data; name="'+aFieldName+'"';
    AddContent(aContent);
  Finally
    aContent.Free;
    aStringStream.free;
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
Var i: Integer;
begin
  with TAlMultiPartFormDataStream(DataStream) do begin
    Clear;
    If assigned(aContentFiles) then
      For i := 0 to aContentFiles.Count - 1 do
        AddContent(aContentFiles[i]);
    If assigned(aContentFields) then
      With aContentFields do
        For i := 0 to Count - 1 do
          AddField(Names[i],ValueFromIndex[i]);
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
Var aContents: TALMultiPartFormDataContents;
    i: integer;
begin
  //Update the Fcontent
  inherited Decode(aDataStr, aboundary);

  //clear the FContentFiles and FContentFields
  FContentFiles.Clear;
  FContentFields.Clear;

  //loop on all contents
  aContents := GetContents;
  For i := 0 to aContents.Count - 1 do begin
    If (aContents[i].FileName <> '') then FContentFiles.Add(aContents[i])             // if Filename or contentType set them assume its File
    else FContentFields.Add(aContents[I].FieldName + '=' + aContents[I].DataString);  // it's a field value
  end;
end;

{*********************************************************************************}
procedure TALMultipartFormDataDecoder.Decode(const aDataStr, aboundary: AnsiString;
                                             aContentFields: TALStrings;
                                             aContentFiles: TALMultiPartFormDataContents);
Var aContents: TALMultiPartFormDataContents;
begin
  //Update the Fcontent
  inherited Decode(aDataStr, aboundary);

  //clear the aContentFiles and aContentFields
  aContentFields.Clear;
  aContentFiles.Clear;

  //loop on all contents
  aContents := GetContents;
  While aContents.Count > 0 do begin
    If (aContents[0].FileName <> '') then aContentFiles.Add(aContents.Extract(aContents[0]))  // if Filename or contentType set them assume its File
    else begin
      aContentFields.Add(aContents[0].FieldName + '=' + aContents[0].DataString);             // it's a field value
      aContents.Delete(0);
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
    i: integer;
begin
  Result := False;
  Lst := TALStringList.Create;
  Try
    Lst.Text := AlStringReplace(ContentDisposition,';',#13#10,[RfReplaceAll]);
    For i := 0 to Lst.Count - 1 do
      If AlLowerCase(ALTrim(Lst[i])) = 'attachment' then begin
        Result := True;
        Break;
      end;
  finally
    Lst.Free;
  end;
end;

{*******************************************************************************************************}
procedure TALMultiPartMixedContent.LoadDataFromFileAsAttachmentBase64Encode(const aFileName: AnsiString);
var aFilenameWithoutPath: AnsiString;
begin
  LoadDataFromFileBase64Encode(aFileName);
  aFilenameWithoutPath := ALExtractFileName(aFileName);
  ContentType := ContentType + '; name="'+aFilenameWithoutPath+'"';
  ContentDisposition := 'attachment; filename="'+aFilenameWithoutPath+'"';
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
Var aContent: TALMultiPartMixedContent;
    aFilenameWithoutPath: AnsiString;
begin
  aContent := TALMultiPartMixedContent.Create;
  Try
    aContent.LoadDataFromStreamBase64Encode(aFileData);
    aFilenameWithoutPath := ALExtractFileName(aFileName);
    aContent.ContentType := aContentType + '; name="'+aFilenameWithoutPath+'"';
    aContent.ContentDisposition := 'attachment; filename="'+aFilenameWithoutPath+'"';
    AddContent(aContent);
  Finally
    aContent.Free;
  end;
end;

{***************************************************************************************}
procedure TAlMultiPartMixedStream.AddAttachmentBase64Encode(const aFileName: AnsiString);
Var aContent: TALMultiPartMixedContent;
begin
  aContent := TALMultiPartMixedContent.Create;
  Try
    aContent.LoadDataFromFileAsAttachmentBase64Encode(aFileName);
    AddContent(aContent);
  Finally
    aContent.Free;
  end;
end;

{*************************************************************************************************}
procedure TAlMultiPartMixedStream.AddInlineTextBase64Encode(const aContentType, aText: AnsiString);
Var aContent: TALMultiPartMixedContent;
    aStringStream: TALStringStream;
begin
  aContent := TALMultiPartMixedContent.Create;
  aStringStream := TALStringStream.Create(aText);
  Try
    aContent.LoadDataFromStreamBase64Encode(aStringStream);
    aContent.ContentType := aContentType;
    AddContent(aContent);
  Finally
    aContent.Free;
    aStringStream.Free;
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
Var i: Integer;
begin
  with TAlMultiPartMixedStream(DataStream) do begin
    Clear;
    AddInlineTextBase64Encode(aInlineTextContentType, aInlineText);
    If assigned(aAttachments) then
      For i := 0 to aAttachments.Count - 1 do
        AddContent(aAttachments[i]);
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
