{*************************************************************
www:          http://sourceforge.net/projects/alcinoe/              
svn:          svn checkout svn://svn.code.sf.net/p/alcinoe/code/ alcinoe-code              
Author(s):    Stéphane Vander Clock (alcinoe@arkadia.com)
Sponsor(s):   Arkadia SA (http://www.arkadia.com)
							
product:      ALMultiPartFormDataParser
Version:      4.00

Description:  MultiPart Form Data function to encode or decode
              stream in multipart/form-data mime format. this format
              is use to send some file by HTTP request.

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

History :     08/09/2007: Fix bug (empty field returned when data
                          is present)
              06/11/2007: REbuild the architecture to add a base class
              13/01/2008: Move EALHttpClientConnectionDropped in ALHttpCommon
              26/06/2012: Add xe2 support

Link :        http://www.w3.org/TR/REC-html40/interact/forms.html#h-17.1
              http://www.ietf.org/rfc/rfc1867.txt
              http://www.ietf.org/rfc/rfc2388.txt
              http://www.w3.org/MarkUp/html-spec/html-spec_8.html

* Please send all your feedback to alcinoe@arkadia.com
* If you have downloaded this source from a website different from 
  sourceforge.net, please get the last version on http://sourceforge.net/projects/alcinoe/
* Please, help us to keep the development of these components free by 
  promoting the sponsor on http://static.arkadia.com/html/alcinoe_like.html
**************************************************************}
unit ALMultiPartFormDataParser;

interface

uses Classes,
     AlStringList,
     AlMultiPartBaseParser;

{Below a sample of multipart/mixed message :
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

  {-Single multipart Object----------------------------------}
  TALMultiPartFormDataContent = class(TALMultiPartBaseContent)
  private
    function GetFieldName: AnsiString;
    function GetFileName: AnsiString;
    procedure SetfieldName(const aValue: AnsiString);
    procedure SetfileName(const aValue: AnsiString);
  public
    procedure LoadDataFromFile(aFileName: AnsiString); override;
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
    procedure   Decode(aDataStr, aboundary: AnsiString); overload; Override;
    procedure   Decode(aDataStr, aboundary: AnsiString; aContentFields: TALStrings; aContentFiles: TALMultiPartFormDataContents); overload;
    property    ContentFiles: TALMultiPartFormDataContents read GetContentFiles;
    property    ContentFields: TALStrings read GetContentFields;
  end;

implementation

uses ALFcnString;

{************************************************************}
function TALMultiPartFormDataContent.GetFieldName: AnsiString;
begin
  Result := ALMultipartExtractSubValueFromHeaderLine(ContentDisposition, 'name');
end;

{***********************************************************}
function TALMultiPartFormDataContent.GetFileName: AnsiString;
begin
  Result := ALMultipartExtractSubValueFromHeaderLine(ContentDisposition, 'filename');
end;

{****************************************************************************}
procedure TALMultiPartFormDataContent.LoadDataFromFile(aFileName: AnsiString);
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
  ContentDisposition := ALMultipartSetSubValueInHeaderLine(ContentDisposition, 'name', aValue);
end;

{**************************************************************************}
procedure TALMultiPartFormDataContent.SetfileName(const aValue: AnsiString);
begin
  ContentDisposition := ALMultipartSetSubValueInHeaderLine(ContentDisposition, 'filename', aValue);
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

{************************************************************}
procedure TAlMultiPartFormDataStream.AddFile(const aFieldName,
                                                   aFileName,
                                                   aContentType: AnsiString;
                                             aFileData: TStream);
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

{************************************************************}
procedure TAlMultiPartFormDataStream.AddFile(const aFieldName,
                                                   aFileName: AnsiString);
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

{****************************************************************************}
procedure TALMultipartFormDataDecoder.Decode(aDataStr, aboundary: AnsiString);
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

{***************************************************************************}
procedure TALMultipartFormDataDecoder.Decode(aDataStr, aboundary: AnsiString;
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

end.
