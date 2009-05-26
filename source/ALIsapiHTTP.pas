{*************************************************************
www:          http://sourceforge.net/projects/alcinoe/              
Author(s):    Stéphane Vander Clock (svanderclock@arkadia.com)
Sponsor(s):   Arkadia SA (http://www.arkadia.com)
							
product:      Alcinoe ISAPI Http WebRequest and WebResponse Objects
Version:      3.50

Description:  Inherited TWebRequest and TWebResponse for better handling
              of ISAPI Request

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

Link :

Please send all your feedback to svanderclock@arkadia.com
**************************************************************}
unit ALIsapiHTTP;

interface

uses Windows,
     Classes,
     sysutils,
     IsapiHTTP,
     Isapi2;

type

  {--------------------------------------------------}
  EALIsapiRequestContentSizeTooBig = Class(Exception);

  {------------------------------------}
  TALISAPIRequest = class(TISAPIRequest)
  private
    FMoreDataAvailableOnReadClient: Boolean;
    FContentStream: TStream;
    FMaxContentSize: integer;
    Procedure InitContentStream;
    function  GetContentStream: Tstream;
  protected
  public
    constructor Create(AECB: PEXTENSION_CONTROL_BLOCK);
    destructor Destroy; override;
    function ReadClient(var Buffer; Count: Integer): Integer; override;
    Property ContentStream: Tstream Read GetContentStream;
    Property MaxContentSize: Integer Read FMaxContentSize Write FMaxContentSize default -1;
  end;

  {--------------------------------------}
  TALISAPIResponse = class(TISAPIResponse)
  private
  protected
  public
  end;

implementation

uses AlFcnString;

{*****************************************************************}
constructor TALISAPIRequest.Create(AECB: PEXTENSION_CONTROL_BLOCK);
begin
  FMoreDataAvailableOnReadClient := True;
  FContentStream := nil;
  FMaxContentSize := -1;
  inherited create(AECB);
end;

{*********************************}
destructor TALISAPIRequest.Destroy;
begin
  If assigned(FContentStream) then FContentStream.Free;
  inherited;
end;

{******************************************}
procedure TALISAPIRequest.InitContentStream;
var aBytesRead: Longint;
begin
  {init the FcontentStream}
  If not assigned(FContentStream) then begin
    FContentStream := TmemoryStream.create;
    aBytesRead := Length(Content);
    FContentStream.Write(Content[1], aBytesRead);
    If aBytesRead >= ContentLength then FMoreDataAvailableOnReadClient := False;
  end;
end;

{*************************************************}
function TALISAPIRequest.GetContentStream: Tstream;
var aBuffer: array of Byte;
begin
  {init the contentstream}
  InitContentStream;

  {FullFile the FcontentStream}
  IF FMoreDataAvailableOnReadClient then begin
    SetLength(aBuffer, ContentLength - FContentStream.size);
    While FMoreDataAvailableOnReadClient do ReadClient(aBuffer[0], ContentLength - FContentStream.size);
  end;

  {give the ContentStream}
  Result := FContentStream;
end;

{***********************************************************************}
function TALISAPIRequest.ReadClient(var Buffer; Count: Integer): Integer;
begin
  Result := Inherited readClient(Buffer, Count);
  If Result <= 0 then FMoreDataAvailableOnReadClient := False
  else begin
    InitContentStream;
    FcontentStream.Position := FcontentStream.Size;
    FContentStream.Write(Buffer, Result);
    FMoreDataAvailableOnReadClient := FcontentStream.Size < ContentLength;
    If (FMaxContentSize > -1) and (FcontentStream.Size > FMaxContentSize) then
      Raise EALIsapiRequestContentSizeTooBig.create('Content size is bigger than the maximum allowed size ('+inttostr(FMaxContentSize)+')');
  end;
end;

end.






