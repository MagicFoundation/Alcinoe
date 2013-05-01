{*************************************************************
www:          http://sourceforge.net/projects/alcinoe/              
svn:          svn checkout svn://svn.code.sf.net/p/alcinoe/code/ alcinoe-code              
Author(s):    Stéphane Vander Clock (alcinoe@arkadia.com)
Sponsor(s):   Arkadia SA (http://www.arkadia.com)
							
product:      ALMultiPartBaseParser
Version:      4.00

Description:  MultiPart Base objects to encode or decode stream
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

* Please send all your feedback to alcinoe@arkadia.com
* If you have downloaded this source from a website different from 
  sourceforge.net, please get the last version on http://sourceforge.net/projects/alcinoe/
* Please, help us to keep the development of these components free by 
  promoting the sponsor on http://static.arkadia.com/html/alcinoe_like.html
**************************************************************}
unit ALMultiPartBaseParser;

interface

uses Classes,
     Contnrs,
     ALStringList;

type

  {-Single multipart Object--------------}
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
    procedure LoadDataFromFile(aFileName: AnsiString); virtual;
    procedure LoadDataFromStream(aStream: TStream); virtual;
    procedure LoadDataFromFileBase64Encode(aFileName: AnsiString); virtual;
    procedure LoadDataFromStreamBase64Encode(aStream: TStream); virtual;
    procedure SaveDataToFile(aFileName: AnsiString); virtual;
    procedure SaveDataToStream(aStream: TStream); virtual;
    procedure SaveDataToFileBase64Decode(aFileName: AnsiString); virtual;
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
    procedure   Decode(aDataStream: Tstream; aboundary: AnsiString); overload; Virtual;
    procedure   Decode(aDataStr: AnsiString; aboundary: AnsiString); overload; Virtual;
  end;

{---------------------------------------------------------------------------------------}
Function ALMultipartExtractBoundaryFromContentType(aContentType: AnsiString): AnsiString;
Function ALMultipartExtractSubValueFromHeaderLine(aHeaderLine: AnsiString; aName: AnsiString): AnsiString;
Function ALMultipartSetSubValueInHeaderLine(aHeaderLine: AnsiString; aName, AValue: AnsiString): AnsiString;

implementation

uses SysUtils,
     AlFcnString,
     AlFcnMime;

{********************************************************************************************************}
Function ALMultipartExtractSubValueFromHeaderLine(aHeaderLine: AnsiString; aName: AnsiString): AnsiString;

    {------------------------------------------------------------}
    function InternalRemoveQuoteStr(aStr: AnsiString): AnsiString;
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
  aName := AlLowerCase(aName);
  aLst := TALStringList.Create;
  Try
    aLst.Text := AlStringReplace(aHeaderLine,';',#13#10,[RfReplaceAll]);
    For i := 0 to aLst.Count - 1 do
      If AlLowerCase(ALTrim(aLst.Names[i])) = aName then begin
        Result := InternalRemoveQuoteStr(aLst.ValueFromIndex[i]);
        Break;
      end;
  finally
    aLst.Free;
  end;
end;

{**********************************************************************************************************}
Function ALMultipartSetSubValueInHeaderLine(aHeaderLine: AnsiString; aName, AValue: AnsiString): AnsiString;
Var aLst: TALStringList;
    aLowerCaseName: AnsiString;
    aFlag: Boolean;
    i: integer;
begin
  Result := '';
  aLowerCaseName := AlLowerCase(aName);
  aHeaderLine := AlStringReplace(aHeaderLine, #13#10, ' ', [RfReplaceAll]);
  aLst := TALStringList.Create;
  Try
    aFlag := False;
    aLst.Text := AlStringReplace(aHeaderLine,';',#13#10,[RfReplaceAll]);
    For i := 0 to aLst.Count - 1 do
      If AlLowerCase(ALTrim(aLst.Names[i])) = aLowerCaseName then begin
        aLst.ValueFromIndex[i] := '"' + AValue + '"';
        aFlag := True;
        Break;
      end;

    For i := 0 to aLst.Count - 1 do
      Result := Result + '; ' + ALTrim(aLst[i]);

    if Not aFlag then
       Result := Result + '; ' + aName + '=' + '"' + aValue + '"';

    Delete(Result,1,2);
  finally
    aLst.Free;
  end;
end;

{***************************************************************************************}
Function ALMultipartExtractBoundaryFromContentType(aContentType: AnsiString): AnsiString;
Begin
  Result := ALMultipartExtractSubValueFromHeaderLine(aContentType, 'boundary');
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

  {---------------------------------------------}
  Function AlG001(aName: AnsiString): AnsiString;
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

    FContentType:= Alg001('Content-Type');
    FContentTransferEncoding:= Alg001('Content-Transfer-Encoding');
    fContentDisposition:= Alg001('Content-Disposition');
    FContentID := Alg001('Content-ID');
    FContentDescription := Alg001('Content-Description');

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
    FDataStream.ReadBuffer(Result[1],FdataStream.Size);
  end;
  FdataStream.Position := 0;
end;

{************************************************************************}
procedure TALMultiPartBaseContent.SetDataString(const aValue: AnsiString);
begin
  TmemoryStream(FdataStream).clear;
  if Length(aValue) > 0 then FDataStream.WriteBuffer(aValue[1],length(aValue));
  FdataStream.Position := 0;
end;

{************************************************************************}
procedure TALMultiPartBaseContent.LoadDataFromFile(aFileName: AnsiString);
begin
  TmemoryStream(FDataStream).LoadFromFile(String(aFileName));
  ContentType := ALGetDefaultMIMEContentTypeFromExt(ALExtractFileExt(aFileName));
  ContentTransferEncoding := 'binary';
end;

{************************************************************************************}
procedure TALMultiPartBaseContent.LoadDataFromFileBase64Encode(aFileName: AnsiString);
Var Buffer: AnsiString;
begin
  TMemoryStream(FDataStream).clear;
  Buffer := ALMimeBase64EncodeString(AlGetStringFromFile(aFileName));
  FDataStream.WriteBuffer(Buffer[1], length(Buffer));
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
  aStream.ReadBuffer(Buffer[1],aStream.size);
  Buffer := ALMimeBase64EncodeString(Buffer);
  TMemoryStream(FDataStream).clear;
  FDataStream.WriteBuffer(Buffer[1], length(Buffer));
  FDataStream.Position := 0;
  ContentTransferEncoding := 'base64';
end;

{**********************************************************************}
procedure TALMultiPartBaseContent.SaveDataToFile(aFilename: AnsiString);
begin
  TMemoryStream(FDataStream).SaveToFile(String(aFileName));
end;

{*******************************************************************}
procedure TALMultiPartBaseContent.SaveDataToStream(aStream: TStream);
begin
  TMemoryStream(FDataStream).SaveToStream(aStream);
end;

{**********************************************************************************}
procedure TALMultiPartBaseContent.SaveDataToFileBase64Decode(aFileName: AnsiString);
Var Buffer: AnsiString;
begin
  FDataStream.Position := 0;
  SetLength(Buffer,FdataStream.size);
  FDataStream.ReadBuffer(Buffer[1],FdataStream.size);
  AlSaveStringToFile(ALMimeBase64DecodeString(Buffer),aFileName);
end;

{*******************************************************************************}
procedure TALMultiPartBaseContent.SaveDataToStreamBase64Decode(aStream: TStream);
Var Buffer: AnsiString;
begin
  FDataStream.Position := 0;
  SetLength(Buffer,FdataStream.size);
  FDataStream.ReadBuffer(Buffer[1],FdataStream.size);
  Buffer := ALMimeBase64DecodeString(Buffer);
  aStream.WriteBuffer(Buffer[1], Length(Buffer));
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
    For i := 0 to acontents.Count - 1 do AddContent(acontents[i]);
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

{************************************************************************************}
procedure TALMultipartBaseDecoder.Decode(aDataStream: Tstream; aboundary: AnsiString);
var sBuffer: AnsiString;
begin
  aDataStream.Position := 0;
  SetLength(sBuffer, aDataStream.Size);
  aDataStream.Read(Pointer(sBuffer)^, aDataStream.Size);
  Decode(sBuffer, aboundary);
end;

{************************************************************************}
procedure TALMultipartBaseDecoder.Decode(aDataStr, aboundary: AnsiString);
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
      DataStream.Write(aDataStr[P2], P3-P2);
      DataStream.Position := 0;

      {move P1 to the newt boundary}
      P1 := P3;
    end;
  end;

end;

end.
