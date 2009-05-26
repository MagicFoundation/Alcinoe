{*************************************************************
www:          http://sourceforge.net/projects/alcinoe/              
Author(s):    Stéphane Vander Clock (svanderclock@arkadia.com)
Sponsor(s):   Arkadia SA (http://www.arkadia.com)
							
product:      ALMultiPartBaseParser
Version:      3.50

Description:  MultiPart Base objects to encode or decode stream
              in mime multipart/xxx format.

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

History :

Link :        http://msdn.microsoft.com/library/default.asp?url=/library/en-us/cdosys/html/7a18a98b-3a18-45b2-83a9-28a8f4099970.asp
              http://www.ietf.org/rfc/rfc2646.txt

Please send all your feedback to svanderclock@arkadia.com
**************************************************************}
unit ALMultiPartBaseParser;

interface

uses Classes,
     SysUtils,
     Contnrs,
     HTTPApp;

type

  {-Single multipart Object--------------}
  TALMultiPartBaseContent = class(TObject)
  private
    FContentType: string;
    FContentTransferEncoding: String;
    FContentDisposition: string;
    FContentID: String;
    FContentDescription: String;
    FDataStream: TStream;
    FCustomHeaders: Tstrings;
    Function GetRawHeaderText: String;
    procedure SetRawHeaderText(const aRawHeaderText: string);
    function GetDataString: String;
    procedure SetDataString(const aValue: String);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear; virtual;
    procedure LoadDataFromFile(aFileName: string); virtual;
    procedure LoadDataFromStream(aStream: TStream); virtual;
    procedure LoadDataFromFileBase64Encode(aFileName: string); virtual;
    procedure LoadDataFromStreamBase64Encode(aStream: TStream); virtual;
    procedure SaveDataToFile(aFileName: string); virtual;
    procedure SaveDataToStream(aStream: TStream); virtual;
    procedure SaveDataToFileBase64Decode(aFileName: string); virtual;
    procedure SaveDataToStreamBase64Decode(aStream: TStream); virtual;
    property ContentType: string read FContentType write FContentType; //Content-Type: text/plain; charset="utf-8"  or  Content-Type: image/bmp; name="Blue Lace 16.bmp"
    property ContentTransferEncoding: string read FContentTransferEncoding write FContentTransferEncoding; //Content-Transfer-Encoding: base64
    property ContentDisposition: string read FContentDisposition write FContentDisposition; //Content-Disposition: attachment; filename="Blue Lace 16.bmp"
    property ContentID: string read FContentID write FContentID; //Content-ID: <foo4%25foo1@bar.net>
    property ContentDescription: string read FContentDescription write FContentDescription; //Content-Description: some text
    property DataStream: TStream read FDataStream;
    property DataString: String read GetDataString Write SetDataString;
    property CustomHeaders: Tstrings read FCustomHeaders;
    property RawHeaderText: String read GetRawHeaderText write setRawHeaderText;
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
    FBoundary: string;
    function GenerateUniqueBoundary: string;
  public
    constructor Create; virtual;
    procedure   AddContent(aContent: TALMultiPartBaseContent); virtual;
    procedure   CloseBoundary; virtual;
    property    Boundary: string read FBoundary write FBoundary;
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
    procedure   Decode(aDataStream: Tstream; aboundary: String); overload; Virtual;
    procedure   Decode(aDataStr: String; aboundary: String); overload; Virtual;
  end;

{-------------------------------------------------------------------------------}
Function ALMultipartExtractBoundaryFromContentType(aContentType: String): String;
Function ALMultipartExtractSubValueFromHeaderLine(aHeaderLine: String; aName: String): String;
Function ALMultipartSetSubValueInHeaderLine(aHeaderLine: String; aName, AValue: String): String;

implementation

uses AlFcnString,
     AlFcnMime;

{********************************************************************************************}
Function ALMultipartExtractSubValueFromHeaderLine(aHeaderLine: String; aName: String): String;

    {----------------------------------------------------}
    function InternalRemoveQuoteStr(aStr: String): String;
    Begin
      Result := AStr;
      If (Length(result) > 0) and
         (result[1] in ['"','''']) and
         (result[1]=result[length(result)]) then result := AlCopyStr(Result,2,length(result)-2);
    end;

Var Lst: TstringList;
    i: integer;
begin
  Result := '';
  aName := AlLowerCase(aName);
  Lst := TstringList.Create;
  Try
    Lst.Text := AlStringReplace(aHeaderLine,';',#13#10,[RfReplaceAll]);
    For i := 0 to Lst.Count - 1 do
      If AlLowerCase(Trim(Lst.Names[i])) = aName then begin
        Result := InternalRemoveQuoteStr(Lst.ValueFromIndex[i]);
        Break;
      end;
  finally
    Lst.Free;
  end;
end;

{**********************************************************************************************}
Function ALMultipartSetSubValueInHeaderLine(aHeaderLine: String; aName, AValue: String): String;
Var Lst: TstringList;
    aLowerCaseName: String;
    i: integer;
    Flag1: Boolean;
begin
  Result := '';
  aLowerCaseName := AlLowerCase(aName);
  aHeaderLine := AlStringReplace(aHeaderLine, #13#10, ' ', [RfReplaceAll]);
  Lst := TstringList.Create;
  Try
    Flag1 := False;
    Lst.Text := AlStringReplace(aHeaderLine,';',#13#10,[RfReplaceAll]);
    For i := 0 to Lst.Count - 1 do
      If AlLowerCase(Trim(Lst.Names[i])) = aLowerCaseName then begin
        Lst.ValueFromIndex[i] := '"' + AValue + '"';
        Flag1 := True;
        Break;
      end;

    For i := 0 to Lst.Count - 1 do
      Result := Result + '; ' + trim(Lst[i]);

    if Not Flag1 then
       Result := Result + '; ' + aName + '=' + '"' + aValue + '"';

    Delete(Result,1,2);
  finally
    Lst.Free;
  end;
end;


{*******************************************************************************}
Function ALMultipartExtractBoundaryFromContentType(aContentType: String): String;
Begin
  Result := ALMultipartExtractSubValueFromHeaderLine(aContentType, 'boundary');
end;

//////////////////////////////////////////////////////////////
//////////  TALMultiPartBaseContent //////////////////////////
//////////////////////////////////////////////////////////////

{*****************************************}
constructor TALMultiPartBaseContent.Create;
begin
  inherited;
  FDataStream := TMemoryStream.Create;
  FCustomHeaders := TstringList.create;
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

{********************************************************}
function TALMultiPartBaseContent.GetRawHeaderText: String;
Var i : integer;
begin
  Result := '';
  If Trim(FContentType) <> '' then result := result + 'Content-Type: ' + trim(FContentType) + #13#10;
  If Trim(FContentTransferEncoding) <> '' then result := result + 'Content-Transfer-Encoding: ' + trim(FContentTransferEncoding) + #13#10;
  If Trim(fContentDisposition) <> '' then result := result + 'Content-Disposition: ' + trim(fContentDisposition) + #13#10;
  If Trim(FContentID) <> '' then result := result + 'Content-ID: ' + trim(FContentID) + #13#10;
  If Trim(FContentDescription) <> '' then result := result + 'Content-Description: ' + trim(FContentDescription) + #13#10;
  For i := 0 to FCustomHeaders.count - 1 do
    if (trim(FCustomHeaders.names[i]) <> '') and (trim(FCustomHeaders.ValueFromIndex[i]) <> '') then
      result := result + FCustomHeaders.names[i] + ': ' + trim(FCustomHeaders.ValueFromIndex[i]) + #13#10;
end;

{*******************************************************************************}
procedure TALMultiPartBaseContent.SetRawHeaderText(const aRawHeaderText: string);
Var aRawHeaderLst: TstringList;

  {-------------------------------------}
  Function AlG001(aName: String): String;
  Var i: Integer;
      Str: String;
  Begin
    I := aRawHeaderLst.IndexOfName(aName);
    If I >= 0 then Begin
      result := Trim(aRawHeaderLst.ValueFromIndex[i]);
      aRawHeaderLst.Delete(i);
      While True do begin
        If i >= aRawHeaderLst.Count then break;
        str := aRawHeaderLst[i];
        If (str = '') or
           (not (str[1] in [' ',#9])) then break; //(1) an empty line or (2) a line that does not start with a space, a tab, or a field name followed by a colon
        Result := trim(result + ' ' + trim(str));
        aRawHeaderLst.Delete(i);
      end;
    end
    else result := '';
  end;

Var Str1, Str2: String;
    j: integer;
begin
  Clear;
  aRawHeaderLst := TstringList.create;
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
      Str1 := trim(aRawHeaderLst.Names[j]);
      If (trim(str1) <> '') and (not (str1[1] in [' ',#9])) then begin
        Str1 := trim(Str1) + ': ' + trim(aRawHeaderLst.ValueFromIndex[j]);
        inc(j);
        While True do begin
          If j >= aRawHeaderLst.Count then break;
          str2 := aRawHeaderLst[j];
          If (str2 = '') or
             (not (str2[1] in [' ',#9])) then break; //(1) an empty line or (2) a line that does not start with a space, a tab, or a field name followed by a colon
          Str1 := trim(Str1 + ' ' + trim(str2));
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

{*****************************************************}
function TALMultiPartBaseContent.GetDataString: String;
begin
  FdataStream.Position := 0;
  SetLength(Result,FdataStream.size);
  FDataStream.ReadBuffer(Result[1],FdataStream.Size);
  FdataStream.Position := 0;
end;

{********************************************************************}
procedure TALMultiPartBaseContent.SetDataString(const aValue: String);
begin
  TmemoryStream(FdataStream).clear;
  FDataStream.WriteBuffer(aValue[1],length(aValue));
  FdataStream.Position := 0;
end;

{********************************************************************}
procedure TALMultiPartBaseContent.LoadDataFromFile(aFileName: string);
begin
  TmemoryStream(FDataStream).LoadFromFile(aFileName);
  ContentType := ALGetDefaultMIMEContentTypeFromExt(ExtractfileExt(aFileName));
  ContentTransferEncoding := 'binary';
end;

{********************************************************************************}
procedure TALMultiPartBaseContent.LoadDataFromFileBase64Encode(aFileName: string);
Var Buffer: String;
begin
  TMemoryStream(FDataStream).clear;
  Buffer := ALMimeBase64EncodeString(AlGetStringFromFile(aFileName));
  FDataStream.WriteBuffer(Buffer[1], length(Buffer));
  FDataStream.Position := 0;
  ContentType := ALGetDefaultMIMEContentTypeFromExt(ExtractfileExt(aFileName));
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
Var Buffer: String;
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

{******************************************************************}
procedure TALMultiPartBaseContent.SaveDataToFile(aFilename: string);
begin
  TMemoryStream(FDataStream).SaveToFile(aFileName);
end;

{*******************************************************************}
procedure TALMultiPartBaseContent.SaveDataToStream(aStream: TStream);
begin
  TMemoryStream(FDataStream).SaveToStream(aStream);
end;

{******************************************************************************}
procedure TALMultiPartBaseContent.SaveDataToFileBase64Decode(aFileName: string);
Var Buffer: String;
begin
  FDataStream.Position := 0;
  SetLength(Buffer,FdataStream.size);
  FDataStream.ReadBuffer(Buffer[1],FdataStream.size);
  AlSaveStringToFile(ALMimeBase64DecodeString(Buffer),aFileName);
end;

{*******************************************************************************}
procedure TALMultiPartBaseContent.SaveDataToStreamBase64Decode(aStream: TStream);
Var Buffer: String;
begin
  FDataStream.Position := 0;
  SetLength(Buffer,FdataStream.size);
  FDataStream.ReadBuffer(Buffer[1],FdataStream.size);
  Buffer := ALMimeBase64DecodeString(Buffer);
  aStream.WriteBuffer(Buffer[1], Length(Buffer));
end;




///////////////////////////////////////////////////////////////
//////////  TALMultiPartBaseContents  ////////////////////////
///////////////////////////////////////////////////////////////

{*******************************************************************************}
function TALMultiPartBaseContents.Add(AObject: TALMultiPartBaseContent): Integer;
begin
  Result := inherited Add(AObject);
end;

{***************************************************************}
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

{***********************************************************************************}
function TALMultiPartBaseContents.GetItem(Index: Integer): TALMultiPartBaseContent;
begin
  Result := TALMultiPartBaseContent(inherited Items[Index]);
end;

{*************************************************************************************}
function TALMultiPartBaseContents.IndexOf(AObject: TALMultiPartBaseContent): Integer;
begin
  Result := inherited IndexOf(AObject);
end;

{********************************************************************************************}
procedure TALMultiPartBaseContents.Insert(Index: Integer; AObject: TALMultiPartBaseContent);
begin
  inherited Insert(Index, AObject);
end;

{************************************************************************************}
function TALMultiPartBaseContents.Remove(AObject: TALMultiPartBaseContent): Integer;
begin
  Result := inherited Remove(AObject);
end;

{*********************************************************************************************}
procedure TALMultiPartBaseContents.SetItem(Index: Integer; AObject: TALMultiPartBaseContent);
begin
  inherited Items[Index] := AObject;
end;




//////////////////////////////////////////////
////////// TAlMultiPartBaseStream ///////////
//////////////////////////////////////////////

{****************************************}
constructor TAlMultiPartBaseStream.Create;
begin
  inherited;
  FBoundary := GenerateUniqueBoundary;
end;

{*****************************************************************************}
procedure TAlMultiPartBaseStream.AddContent(aContent: TALMultiPartBaseContent);
var sFormFieldInfo: string;
begin
  sFormFieldInfo := #13#10 +
                    '--' + Boundary + #13#10 +
                    aContent.RawHeaderText + #13#10;

  Write(Pointer(sFormFieldInfo)^, Length(sFormFieldInfo));
  CopyFrom(aContent.DataStream,0);
end;

{*************************************************************}
function TAlMultiPartBaseStream.GenerateUniqueBoundary: string;
begin
  Result := '---------------------------' + FormatDateTime('mmddyyhhnnsszzz', Now);
end;

{*********************************************}
procedure TAlMultiPartBaseStream.CloseBoundary;
var sFormFieldInfo: string;
begin
  sFormFieldInfo := #13#10 +
                    '--' + Boundary + '--' + #13#10;
  Write(Pointer(sFormFieldInfo)^, Length(sFormFieldInfo));
end;




////////////////////////////////////////////////
////////// TALMultipartBaseEncoder ////////////
////////////////////////////////////////////////

{******************************************}
constructor TALMultipartBaseEncoder.Create;
begin
  inherited;
  FDataStream := CreateDataStream;
end;

{******************************************}
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




////////////////////////////////////////////////////////
////////// TALMultipartBaseDecoder ////////////////////
////////////////////////////////////////////////////////


{******************************************}
constructor TALMultipartBaseDecoder.Create;
begin
  Inherited;
  FContents:= CreateContents;
  Fcontents.OwnsObjects := True;
end;

{******************************************}
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

{********************************************************************************}
procedure TALMultipartBaseDecoder.Decode(aDataStream: Tstream; aboundary: String);
var sBuffer: string;
begin
  {read the data from the Data}
  aDataStream.Position := 0;
  SetLength(sBuffer, aDataStream.Size);
  aDataStream.Read(Pointer(sBuffer)^, aDataStream.Size);
  Decode(sBuffer, aboundary);
end;

{********************************************************************}
procedure TALMultipartBaseDecoder.Decode(aDataStr, aboundary: String);
var P1, P2, P3: Integer;
    LnBoundary: Integer;
    Flag1: Boolean;
    aContent: TALMultiPartBaseContent;
begin

  {clear the fContent}
  FContents.Clear;

  {init LnBoundary}
  LnBoundary := length(aBoundary);

  {Find the first Boundary}
  P1 := AlPos('--'+aBoundary+#13#10, aDataStr);
  Flag1 := P1 > 0;
  Dec(P1,2);

  {start the loop on all Boundary}
  While Flag1 Do begin
    aContent := CreateContent;
    With aContent do begin

      {Add the Content to the Contents}
      FContents.Add(aContent);

      {move P1 to the start of the header}
      P1 := P1 + LnBoundary + 6;

      {Find the next Boundary}
      P3 := AlPosEx(#13#10+'--'+aBoundary+#13#10, aDataStr, P1);
      if P3 <= 0 then Begin
        Flag1 := False;
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
