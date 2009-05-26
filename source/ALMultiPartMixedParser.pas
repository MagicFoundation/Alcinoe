{*************************************************************
www:          http://sourceforge.net/projects/alcinoe/              
Author(s):    Stéphane Vander Clock (svanderclock@arkadia.com)
Sponsor(s):   Arkadia SA (http://www.arkadia.com)
							
product:      ALMultiPartMixedParser
Version:      3.50

Description:  MultiPart Mixed objects to encode or decode stream in
              mime multipart/mixed format. the best way to add some
              Attachments to any email or news content.

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

History :			14/06/2007: fixe TAlMultiPartMixedStream.AddInlineText %s in text bug by Daniel Bauten
              06/11/2007: REbuild the architecture to add a base class

Link :        http://msdn.microsoft.com/library/default.asp?url=/library/en-us/cdosys/html/7a18a98b-3a18-45b2-83a9-28a8f4099970.asp
              http://www.ietf.org/rfc/rfc2646.txt

Please send all your feedback to svanderclock@arkadia.com
**************************************************************}
unit ALMultiPartMixedParser;

interface

uses Classes,
     SysUtils,
     ALMultiPartBaseParser;

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

  {-Single multipart Object-------------------------------}
  TALMultiPartMixedContent = class(TALMultiPartBaseContent)
  private
    function GetAttachment: Boolean;
  public
    procedure LoadDataFromFileAsAttachmentBase64Encode(aFileName: string); virtual;
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
    procedure AddInlineTextBase64Encode(aContentType, aText: string);
    procedure AddAttachmentBase64Encode(aFileName, aContentType: string; aFileData: TStream); overload;
    procedure AddAttachmentBase64Encode(aFileName: String); overload;
    procedure AddContent(aContent: TALMultiPartMixedContent); reintroduce;
  end;

  {--TALMultipartMixedEncoder-----------------------------}
  TALMultipartMixedEncoder = class(TALMultipartBaseEncoder)
  private
  protected
    Function CreateDataStream: TAlMultiPartBaseStream; override;
    function GetDataStream: TAlMultiPartMixedStream; reintroduce;
  public
    procedure Encode(aInlineText, aInlineTextContentType: String; aAttachments: TALMultiPartMixedContents); overload;
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

implementation

uses AlFcnString;

///////////////////////////////////////////////////////////////
//////////  TALMultiPartMixedContent //////////////////////////
///////////////////////////////////////////////////////////////

{*******************************************************}
function TALMultiPartMixedContent.GetAttachment: Boolean;
Var Lst: TstringList;
    i: integer;
begin
  Result := False;
  Lst := TstringList.Create;
  Try
    Lst.Text := AlStringReplace(ContentDisposition,';',#13#10,[RfReplaceAll]);
    For i := 0 to Lst.Count - 1 do
      If AlLowerCase(Trim(Lst[i])) = 'attachment' then begin
        Result := True;
        Break;
      end;
  finally
    Lst.Free;
  end;
end;

{*********************************************************************************************}
procedure TALMultiPartMixedContent.LoadDataFromFileAsAttachmentBase64Encode(aFileName: string);
begin
  LoadDataFromFileBase64Encode(aFileName);
  aFileName := ExtractFileName(aFileName);
  ContentType := ContentType + '; name="'+aFileName+'"';
  ContentDisposition := 'attachment; filename="'+aFileName+'"';
end;




///////////////////////////////////////////////////////////////
//////////  TALMultiPartMixedContents  ////////////////////////
///////////////////////////////////////////////////////////////

{*********************************************************************************}
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




//////////////////////////////////////////////
////////// TAlMultiPartMixedStream ///////////
//////////////////////////////////////////////

{***************************************************************************************************************}
procedure TAlMultiPartMixedStream.AddAttachmentBase64Encode(aFileName, aContentType: string; aFileData: TStream);
Var aContent: TALMultiPartMixedContent;
begin
  aContent := TALMultiPartMixedContent.Create;
  Try
    aContent.LoadDataFromStreamBase64Encode(aFileData);
    aFileName := ExtractFileName(aFileName);
    aContent.ContentType := aContentType + '; name="'+aFileName+'"';
    aContent.ContentDisposition := 'attachment; filename="'+aFileName+'"';
    AddContent(aContent);
  Finally
    aContent.Free;
  end;
end;

{*****************************************************************************}
procedure TAlMultiPartMixedStream.AddAttachmentBase64Encode(aFileName: string);
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

{***************************************************************************************}
procedure TAlMultiPartMixedStream.AddInlineTextBase64Encode(aContentType, aText: string);
Var aContent: TALMultiPartMixedContent;
    aStringStream: TstringStream;
begin
  aContent := TALMultiPartMixedContent.Create;
  aStringStream := TstringStream.Create(aText);
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
Var sFormFieldInfo: string;
begin
  If Position = 0 then Begin
    sFormFieldInfo := #13#10 +
                      'This is a multi-part message in MIME format.'+#13#10;
    Write(Pointer(sFormFieldInfo)^, Length(sFormFieldInfo));
  end;

  Inherited AddContent(aContent);
end;



////////////////////////////////////////////////
////////// TALMultipartMixedEncoder ////////////
////////////////////////////////////////////////

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

{****************************************************}
procedure TALMultipartMixedEncoder.Encode(aInlineText,
                                          aInlineTextContentType: String;
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



////////////////////////////////////////////////////////
////////// TALMultipartMixedDecoder ////////////////////
////////////////////////////////////////////////////////

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
