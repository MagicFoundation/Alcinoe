{*************************************************************
www:          http://sourceforge.net/projects/alcinoe/              
Author(s):    Stéphane Vander Clock (svanderclock@arkadia.com)
Sponsor(s):   Arkadia SA (http://www.arkadia.com)
							
product:      ALMultiPartFormDataParser
Version:      3.50

Description:  MultiPart Form Data function to encode or decode
              stream in multipart/form-data mime format. this format
              is use to send some file by HTTP request.

Legal issues: Copyright (C) 1999-2009 by Arkadia Software Engineering

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

Link :        http://www.w3.org/TR/REC-html40/interact/forms.html#h-17.1
              http://www.ietf.org/rfc/rfc1867.txt
              http://www.ietf.org/rfc/rfc2388.txt
              http://www.w3.org/MarkUp/html-spec/html-spec_8.html

Please send all your feedback to svanderclock@arkadia.com
**************************************************************}
unit ALMultiPartFormDataParser;

interface

uses Classes,
     SysUtils,
     HTTPApp,
     ALHttpCommon,
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
    function GetFieldName: String;
    function GetFileName: String;
    procedure SetfieldName(const aValue: String);
    procedure SetfileName(const aValue: String);
  public
    procedure LoadDataFromFile(aFileName: string); override;
    procedure LoadDataFromStream(aStream: TStream); override;
    Property FieldName: String Read GetFieldName Write SetFieldName;
    Property FileName: String Read GetFileName Write SetfileName;
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
    procedure AddField(const aFieldName, aFieldValue: string);
    procedure AddFile(const aFieldName, aFileName, aContentType: string; aFileData: TStream); overload;
    procedure AddFile(const aFieldName, aFileName: string); overload;
    procedure AddContent(aContent: TALMultiPartFormDataContent); reintroduce;
  end;

  {--TALMultipartFormDataEncoder-----------------------------}
  TALMultipartFormDataEncoder = class(TALMultipartBaseEncoder)
  private
  protected
    Function CreateDataStream: TAlMultiPartBaseStream; override;
    function GetDataStream: TAlMultiPartFormDataStream; reintroduce;
  public
    procedure Encode(aContentFields: TStrings; aContentFiles: TALMultiPartFormDataContents);
    property  DataStream: TAlMultiPartFormDataStream read GetDataStream;
  end;

  {--TALMultipartFormDataDecoder-----------------------------}
  TALMultipartFormDataDecoder = class(TALMultipartBaseDecoder)
  private
    FContentFiles: TALMultiPartFormDataContents;
    FContentFields: TStrings;
    FRemoveDuplicateField: Boolean;
    function GetContentFields: Tstrings;
    function GetContentFiles: TALMultiPartFormDataContents;
  protected
    function GetContents: TALMultiPartFormDataContents; reintroduce;
    Function CreateContent: TALMultiPartBaseContent; override;
    Function CreateContents: TALMultiPartBaseContents; override;
  public
    constructor Create; override;
    destructor  Destroy; override;
    procedure   Decode(aDataStr, aboundary: String); overload; Override;
    procedure   Decode(aRequest: TWebRequest; aboundary: String); overload;
    property    ContentFiles: TALMultiPartFormDataContents read GetContentFiles;
    property    ContentFields: Tstrings read GetContentFields;
    property    RemoveDuplicateField: Boolean read FRemoveDuplicateField write FRemoveDuplicateField Default True;
  end;

implementation

//////////////////////////////////////////////////////////////////
//////////  TALMultiPartFormDataContent  /////////////////////////
//////////////////////////////////////////////////////////////////

{********************************************************}
function TALMultiPartFormDataContent.GetFieldName: String;
begin
  Result := ALMultipartExtractSubValueFromHeaderLine(ContentDisposition, 'name');
end;

{*******************************************************}
function TALMultiPartFormDataContent.GetFileName: String;
begin
  Result := ALMultipartExtractSubValueFromHeaderLine(ContentDisposition, 'filename');
end;

{************************************************************************}
procedure TALMultiPartFormDataContent.LoadDataFromFile(aFileName: string);
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

{***********************************************************************}
procedure TALMultiPartFormDataContent.SetfieldName(const aValue: String);
begin
  ContentDisposition := ALMultipartSetSubValueInHeaderLine(ContentDisposition, 'name', aValue);
end;

{**********************************************************************}
procedure TALMultiPartFormDataContent.SetfileName(const aValue: String);
begin
  ContentDisposition := ALMultipartSetSubValueInHeaderLine(ContentDisposition, 'filename', aValue);
end;



//////////////////////////////////////////////////////////////////
//////////  TALMultiPartFormDataContents  ////////////////////////
//////////////////////////////////////////////////////////////////

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




////////////////////////////////////////////////////////////////////////
////////// TAlMultiPartFormDataStream //////////////////////////////////
////////////////////////////////////////////////////////////////////////

{************************************************************}
procedure TAlMultiPartFormDataStream.AddFile(const aFieldName,
                                                   aFileName,
                                                   aContentType: string;
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
                                                   aFileName: string);
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

{***********************************************************************************}
procedure TAlMultiPartFormDataStream.AddField(const aFieldName, aFieldValue: string);
Var aContent: TALMultiPartFormDataContent;
    aStringStream: TStringStream;
begin
  aStringStream:= TStringStream.Create(aFieldValue);
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



///////////////////////////////////////////////////////////////////////////////////////
////////// TALMultipartFormDataEncoder ////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////

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

{******************************************************************************************************************}
procedure TALMultipartFormDataEncoder.Encode(aContentFields: TStrings; aContentFiles: TALMultiPartFormDataContents);
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



//////////////////////////////////////////////////////////////////
//////////  TALMultipartFormDataDecoder  /////////////////////////
//////////////////////////////////////////////////////////////////

{*********************************************}
constructor TALMultipartFormDataDecoder.Create;
begin
  inherited;
  FContentFiles := TALMultiPartFormDataContents.Create(False);
  FContentFields := TStringList.Create;
  FRemoveDuplicateField := True;
end;

{*********************************************}
destructor TALMultipartFormDataDecoder.Destroy;
begin
  FContentFiles.Free;
  FContentFields.Free;
  inherited;
end;

{*************************************************************************************}
procedure TALMultipartFormDataDecoder.Decode(aRequest: TWebRequest; aboundary: String);
var aContentStream: TMemoryStream;
    aTotalBytes: LongInt;
    aBytesRead: Longint;
    aChunkSize: Longint;
    aBuffer: array of Byte;
begin

  {Full Extract of the request}
  aContentStream := TMemoryStream.Create;
  try
    aBytesRead := Length(aRequest.Content);
    aContentStream.Write(aRequest.Content[1], aBytesRead);
    aTotalBytes := aRequest.ContentLength;
    if aBytesRead < aTotalBytes then begin
      SetLength(aBuffer, aTotalBytes);
      repeat
        aChunkSize := aRequest.ReadClient(aBuffer[0], aTotalBytes - aBytesRead);
        if aChunkSize <= 0 then Break;
        aContentStream.Write(aBuffer[0], aChunkSize);
        Inc(aBytesRead, aChunkSize);
      until (aTotalBytes = aBytesRead);
    end;
    if aRequest.ContentLength - aBytesRead > 0 then
      raise EALHttpClientConnectionDropped.Create('Client Dropped Connection.'#13#10 +
        'Total Bytes indicated by Header: ' + IntToStr(aTotalBytes) + #13#10 +
        'Total Bytes Read: ' + IntToStr(aBytesRead));

    {parse the request now}
    Decode(aContentStream, aBoundary);
  finally
    aContentStream.Free;
  end;

end;

{************************************************************************}
procedure TALMultipartFormDataDecoder.Decode(aDataStr, aboundary: String);
Var i: integer;
    aContents: TALMultiPartFormDataContents;
    CanInsertFile: Boolean;
    P1: integer;
begin
  inherited Decode(aDataStr, aboundary);

  {clear the FContentFiles and FContentFields}
  FContentFiles.Clear;
  FContentFields.Clear;

  {loop on all contents}
  aContents := GetContents;
  For i := 0 to aContents.Count - 1 do begin

    {if Filename or contentType set them assume its File}
    If (aContents[i].FileName <> '') then begin

        CanInsertFile := True;
        IF FRemoveDuplicateField then
          For P1 := 0 to fContentFiles.Count - 1 do
            If sametext(fContentfiles[P1].FieldName, aContents[i].fieldName) then begin
              CanInsertFile := False;
              {More bigger the file is, more lucky
               we are that it contain the full data}
              If (fContentfiles[P1].DataStream.Size < aContents[i].DataStream.size) then FContentFiles[P1] := aContents[i];
              Break;
            end;

        If CanInsertFile then FContentFiles.Add(aContents[i])

    end

    {it's a field value}
    else Begin
      P1 := contentFields.IndexOfName(aContents[I].FieldName);
      If (not FRemoveDuplicateField) or (P1=-1) then ContentFields.Add(aContents[I].FieldName + '=' + aContents[I].DataString)
      Else If length(ContentFields.ValueFromIndex[P1])<length(aContents[I].DataString) then ContentFields.ValueFromIndex[P1] := aContents[I].DataString;
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

{**************************************************************}
function TALMultipartFormDataDecoder.GetContentFields: Tstrings;
begin
  Result := fContentFields;
end;

{*********************************************************************************}
function TALMultipartFormDataDecoder.GetContentFiles: TALMultiPartFormDataContents;
begin
  Result := fContentFiles;
end;

end.
