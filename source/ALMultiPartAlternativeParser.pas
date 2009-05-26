{*************************************************************
www:          http://sourceforge.net/projects/alcinoe/              
Author(s):    Stéphane Vander Clock (svanderclock@arkadia.com)
Sponsor(s):   Arkadia SA (http://www.arkadia.com)
							
product:      ALMultiPartAlternativeParser
Version:      3.50

Description:  MultiPart Alternative objects to encode or Decode stream
              in mime multipart/Alternative format.

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
unit ALMultiPartAlternativeParser;

interface

uses ALMultiPartBaseParser;

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

  {-Single multipart Object-------------------------------------}
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

implementation

///////////////////////////////////////////////////////////////
//////////  TALMultiPartAlternativeContents  //////////////////
///////////////////////////////////////////////////////////////

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




//////////////////////////////////////////////
////////// TAlMultiPartAlternativeStream /////
//////////////////////////////////////////////

{*******************************************************************************************}
procedure TAlMultiPartAlternativeStream.AddContent(aContent: TALMultiPartAlternativeContent);
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
////////// TALMultipartAlternativeEncoder //////
////////////////////////////////////////////////

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




////////////////////////////////////////////////////////
////////// TALMultipartAlternativeDecoder //////////////
////////////////////////////////////////////////////////

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

end.
