{*************************************************************
www:          http://sourceforge.net/projects/alcinoe/              
svn:          svn checkout svn://svn.code.sf.net/p/alcinoe/code/ alcinoe-code              
Author(s):    Stéphane Vander Clock (alcinoe@arkadia.com)
Sponsor(s):   Arkadia SA (http://www.arkadia.com)

product:      ALHttpClient Base Class
Version:      4.00

Description:  TALHttpClient is a ancestor of class like
              TALWinInetHttpClient or TALWinHttpClient

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

History :     28/11/2005: move public procedure to published
                          in TALHttpClientProxyParams
              26/06/2012: Add xe2 support

Link :        http://www.w3.org/TR/REC-html40/interact/forms.html#h-17.1
              http://www.ietf.org/rfc/rfc1867.txt
              http://www.ietf.org/rfc/rfc2388.txt
              http://www.w3.org/MarkUp/html-spec/html-spec_8.html
              http://www.cs.tut.fi/~jkorpela/forms/methods.html

* Please send all your feedback to alcinoe@arkadia.com
* If you have downloaded this source from a website different from 
  sourceforge.net, please get the last version on http://sourceforge.net/projects/alcinoe/
* Please, help us to keep the development of these components free by 
  promoting the sponsor on http://static.arkadia.com/html/alcinoe_like.html
**************************************************************}
unit ALHttpClient;

interface

uses SysUtils,
     Classes,
     ALHttpCommon,
     ALStringList,
     ALMultiPartFormDataParser;

type

  {---------------------------------------}
  EALHTTPClientException = class(Exception)
  private
    FStatusCode: Integer;
  public
    constructor Create(const Msg: AnsiString; SCode: Integer = 0);
    constructor CreateFmt(const Msg: AnsiString; const Args: array of const; SCode: Integer = 0);
    property StatusCode: Integer read FStatusCode write FStatusCode;
  end;

  {---------------------------------------}
  TALHTTPClientProxyParams = Class(Tobject)
  Private
    FProxyBypass: AnsiString;
    FproxyServer: AnsiString;
    FProxyUserName: AnsiString;
    FProxyPassword: AnsiString;
    FproxyPort: integer;
    FOnChange: TALHTTPPropertyChangeEvent;
    procedure SetProxyBypass(const Value: AnsiString);
    procedure SetProxyPassword(const Value: AnsiString);
    procedure SetProxyPort(const Value: integer);
    procedure SetProxyServer(const Value: AnsiString);
    procedure SetProxyUserName(const Value: AnsiString);
    Procedure DoChange(propertyIndex: Integer);
  public
    constructor Create; virtual;
    procedure Clear;
    Property ProxyBypass: AnsiString read FProxyBypass write SetProxyBypass; //index 0
    property ProxyServer: AnsiString read FProxyServer write SetProxyServer; //index 1
    property ProxyPort: integer read FProxyPort write SetProxyPort default 0; //index 2
    property ProxyUserName: AnsiString read FProxyUserName write SetProxyUserName; //index 3
    property ProxyPassword: AnsiString read FProxyPassword write SetProxyPassword; //index 4
    property OnChange: TALHTTPPropertyChangeEvent read FOnChange write FOnChange;
  end;

  {--------------------------------------------------------------------------------------------------}
  TAlHTTPClientRedirectEvent         = procedure(sender: Tobject; const NewURL: AnsiString) of object;
  TALHTTPClientUploadProgressEvent   = procedure(sender: Tobject; Sent: Integer; Total: Integer) of object;
  TALHTTPClientDownloadProgressEvent = procedure(sender: Tobject; Read: Integer; Total: Integer) of object;

  {----------------------------}
  TALHTTPClient = class(TObject)
  private
    FProxyParams: TALHTTPClientProxyParams;
    FRequestHeader: TALHTTPRequestHeader;
    FProtocolVersion: TALHTTPProtocolVersion;
    FRequestMethod: TALHTTPMethod;
    FURL: AnsiString;
    FUserName: AnsiString;
    FPassword: AnsiString;
    FConnectTimeout: Integer;
    FSendTimeout: Integer;
    FReceiveTimeout: Integer;
    FOnUploadProgress: TALHTTPClientUploadProgressEvent;
    FOnDownloadProgress: TALHTTPClientDownloadProgressEvent;
    FOnRedirect: TAlHTTPClientRedirectEvent;
    FUploadBufferSize: Integer;
  protected
    procedure SetURL(const Value: AnsiString); virtual;
    procedure SetUsername(const NameValue: AnsiString); virtual;
    procedure SetPassword(const PasswordValue: AnsiString); virtual;
    procedure OnProxyParamsChange(sender: Tobject; Const PropertyIndex: Integer); virtual;
    procedure OnRequestHeaderChange(sender: Tobject; Const PropertyIndex: Integer); virtual;
    procedure SetUploadBufferSize(const Value: Integer); virtual;
    procedure SetOnRedirect(const Value: TAlHTTPClientRedirectEvent); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Execute(aRequestDataStream: TStream;
                      aResponseContentStream: TStream;
                      aResponseContentHeader: TALHTTPResponseHeader); virtual;
    Procedure Get(const aUrl:AnsiString;
                  aResponseContentStream: TStream;
                  aResponseContentHeader: TALHTTPResponseHeader); overload;
    Procedure Post(const aUrl:AnsiString;
                   aResponseContentStream: TStream;
                   aResponseContentHeader: TALHTTPResponseHeader); overload;
    Procedure Post(const aUrl:AnsiString;
                   aPostDataStream: TStream;
                   aResponseContentStream: TStream;
                   aResponseContentHeader: TALHTTPResponseHeader); overload;
    Procedure PostUrlEncoded(const aUrl:AnsiString;
                             aPostDataStrings: TALStrings;
                             aResponseContentStream: TStream;
                             aResponseContentHeader: TALHTTPResponseHeader;
                             Const EncodeParams: Boolean=True); overload;
    Procedure PostMultipartFormData(const aUrl:AnsiString;
                                    aPostDataStrings: TALStrings;
                                    aPostDataFiles: TALMultiPartFormDataContents;
                                    aResponseContentStream: TStream;
                                    aResponseContentHeader: TALHTTPResponseHeader); overload;
    Procedure Head(const aUrl:AnsiString;
                   aResponseContentStream: TStream;
                   aResponseContentHeader: TALHTTPResponseHeader); overload;
    Procedure Trace(const aUrl:AnsiString;
                    aResponseContentStream: TStream;
                    aResponseContentHeader: TALHTTPResponseHeader); overload;
    Procedure Put(const aUrl:AnsiString;
                  aPutDataStream: TStream;
                  aResponseContentStream: TStream;
                  aResponseContentHeader: TALHTTPResponseHeader); overload;
    procedure Delete(const aUrl:AnsiString;
                     aResponseContentStream: TStream;
                     aResponseContentHeader: TALHTTPResponseHeader); overload;
    Function  Get(const aUrl:AnsiString): AnsiString; overload;
    Function  Get(const aUrl:AnsiString;
                  aParams: TALStrings;
                  Const EncodeParams: Boolean=True): AnsiString; overload;
    Procedure Get(const aUrl:AnsiString;
                  aParams: TALStrings;
                  aResponseContentStream: TStream;
                  aResponseContentHeader: TALHTTPResponseHeader;
                  Const EncodeParams: Boolean=True); overload;
    Function  Post(const aUrl:AnsiString): AnsiString; overload;
    Function  Post(const aUrl:AnsiString; aPostDataStream: TStream): AnsiString; overload;
    Function  PostUrlEncoded(const aUrl:AnsiString;
                             aPostDataStrings: TALStrings;
                             Const EncodeParams: Boolean=True): AnsiString; overload;
    Function  PostMultiPartFormData(const aUrl:AnsiString;
                                    aPostDataStrings: TALStrings;
                                    aPostDataFiles: TALMultiPartFormDataContents): AnsiString; overload;
    Function  Head(const aUrl:AnsiString): AnsiString; overload;
    Function  trace(const aUrl:AnsiString): AnsiString; overload;
    function  Put(const aURL: Ansistring; aPutDataStream: TStream): AnsiString; overload;
    function  Delete(const aURL: Ansistring): AnsiString; overload;
    property  URL: AnsiString read FURL write SetURL;
    property  ConnectTimeout: Integer read FConnectTimeout write FConnectTimeout default 0;
    property  SendTimeout: Integer read FSendTimeout write FSendTimeout default 0;
    property  ReceiveTimeout: Integer read FReceiveTimeout write FReceiveTimeout default 0;
    property  UploadBufferSize: Integer read FUploadBufferSize write SetUploadBufferSize default $8000;
    property  ProxyParams: TALHTTPClientProxyParams read FProxyParams;
    property  RequestHeader: TALHTTPRequestHeader read FRequestHeader;
    Property  ProtocolVersion: TALHTTPProtocolVersion read FProtocolVersion write FProtocolVersion default HTTPpv_1_1;
    Property  RequestMethod: TALHTTPMethod read FRequestMethod write fRequestMethod default HTTPmt_get;
    property  UserName: AnsiString read FUserName write SetUserName;
    property  Password: AnsiString read FPassword write SetPassword;
    property  OnUploadProgress: TALHTTPClientUploadProgressEvent read FOnUploadProgress write FOnUploadProgress;
    property  OnDownloadProgress: TALHTTPClientDownloadProgressEvent read FonDownloadProgress write FonDownloadProgress;
    property  OnRedirect: TAlHTTPClientRedirectEvent read FOnRedirect write SetOnRedirect;
  end;

Const
  cALHTTPCLient_MsgInvalidURL         = 'Invalid url ''%s'' - only supports ''http'' and ''https'' schemes';
  cALHTTPCLient_MsgInvalidHTTPRequest = 'Invalid HTTP Request: Length is 0';
  cALHTTPCLient_MsgEmptyURL           = 'Empty URL';

implementation

uses HttpApp,
     AlFcnString;

{***********************************************************************************}
constructor EALHTTPClientException.Create(const Msg: AnsiString; SCode: Integer = 0);
begin
  inherited Create(String(Msg));
  FStatusCode := SCode;
end;

{******************************************************************************************************************}
constructor EALHTTPClientException.CreateFmt(const Msg: AnsiString; const Args: array of const; SCode: Integer = 0);
begin
  inherited CreateFmt(String(Msg), Args);
  FStatusCode := SCode;
end;

{*******************************}
constructor TALHTTPClient.Create;
begin
  inherited;
  FUploadBufferSize := $8000;
  FConnectTimeout := 0;
  FSendTimeout := 0;
  FReceiveTimeout := 0;
  FURL:= '';
  FUserName := '';
  FPassword := '';
  FOnUploadProgress := nil;
  FOnDownloadProgress := nil;
  FOnRedirect := nil;
  FProxyParams := TALHTTPClientProxyParams.Create;
  FRequestHeader := TALHTTPRequestHeader.Create;
  FRequestHeader.UserAgent := 'Mozilla/3.0 (compatible; TALHTTPClient)';
  FProtocolVersion := HTTPpv_1_1;
  FRequestMethod := HTTPmt_get;
  FrequestHeader.OnChange := OnRequestHeaderChange;
  FProxyParams.OnChange := OnProxyParamsChange;
end;

{*******************************}
destructor TALHTTPClient.Destroy;
begin
  FProxyParams.free;
  FRequestHeader.free;
  inherited;
end;

{******************************************************}
procedure TALHTTPClient.SetURL(const Value: AnsiString);
begin
  Furl := Value;
end;

{***************************************************************}
procedure TALHTTPClient.SetUsername(const NameValue: AnsiString);
begin
  FUserName := NameValue;
end;

{*****************************************************************************}
procedure TALHTTPClient.SetOnRedirect(const Value: TAlHTTPClientRedirectEvent);
begin
  FOnRedirect := Value;
end;

{*******************************************************************}
procedure TALHTTPClient.SetPassword(const PasswordValue: AnsiString);
begin
  FPassword := PasswordValue;
end;

{**********************************************************}
procedure TALHTTPClient.Execute(aRequestDataStream: TStream;
                                aResponseContentStream: TStream;
                                aResponseContentHeader: TALHTTPResponseHeader);
begin
// virtual;
end;

{*************************************************}
procedure TALHTTPClient.Get(const aUrl: AnsiString;
                            aResponseContentStream: TStream;
                            aResponseContentHeader: TALHTTPResponseHeader);
begin
  Url := aURL;
  RequestMethod := HTTPmt_get;
  Execute(nil,
          aResponseContentStream,
          aResponseContentHeader);
end;

{**************************************************}
procedure TALHTTPClient.Post(const aUrl: AnsiString;
                             aPostDataStream: TStream;
                             aResponseContentStream: TStream;
                             aResponseContentHeader: TALHTTPResponseHeader);
Var OldContentLengthValue: AnsiString;
begin
  Url := aURL;
  RequestMethod := HTTPmt_Post;
  OldContentLengthValue := FrequestHeader.ContentLength;
  try
    If assigned(aPostDataStream) then FrequestHeader.ContentLength := ALIntToStr(aPostDataStream.Size)
    else FrequestHeader.ContentLength := '0';
    Execute(aPostDataStream,
            aResponseContentStream,
            aResponseContentHeader);
  finally
    FrequestHeader.ContentLength := OldContentLengthValue;
  end;
end;

{**************************************************}
procedure TALHTTPClient.Post(const aUrl: AnsiString;
                             aResponseContentStream: TStream;
                             aResponseContentHeader: TALHTTPResponseHeader);
begin
  Post(aUrl,
       nil,
       aResponseContentStream,
       aResponseContentHeader);
end;

{*******************************************************************}
procedure TALHTTPClient.PostMultiPartFormData(const aUrl: AnsiString;
                                              aPostDataStrings: TALStrings;
                                              aPostDataFiles: TALMultiPartFormDataContents;
                                              aResponseContentStream: TStream;
                                              aResponseContentHeader: TALHTTPResponseHeader);
Var aMultipartFormDataEncoder: TALMultipartFormDataEncoder;
    OldRequestContentType: AnsiString;
begin
  aMultipartFormDataEncoder := TALMultipartFormDataEncoder.create;
  OldRequestContentType := FrequestHeader.ContentType;
  try
    aMultipartFormDataEncoder.Encode(aPostDataStrings, aPostDataFiles);
    FrequestHeader.ContentType := 'multipart/form-data; boundary='+aMultipartFormDataEncoder.DataStream.Boundary;
    post(aUrl,
         aMultipartFormDataEncoder.DataStream,
         aResponseContentStream,
         aResponseContentHeader);
  finally
    aMultipartFormDataEncoder.free;
    FrequestHeader.ContentType := OldRequestContentType;
  end;
end;

{************************************************************}
procedure TALHTTPClient.PostUrlEncoded(const aUrl: AnsiString;
                                       aPostDataStrings: TALStrings;
                                       aResponseContentStream: TStream;
                                       aResponseContentHeader: TALHTTPResponseHeader;
                                       Const EncodeParams: Boolean=True);
Var aURLEncodedContentStream: TALStringStream;
    OldRequestContentType: AnsiString;
    Str: AnsiString;
    I, P: Integer;
begin
  aURLEncodedContentStream := TALStringStream.create('');
  OldRequestContentType := FrequestHeader.ContentType;
  try

    if EncodeParams then begin
      for i := 0 to aPostDataStrings.Count - 1 do begin
        Str := aPostDataStrings[i];
        P := AlPos(aPostDataStrings.NameValueSeparator, Str);
        if P > 0 then Str := HTTPEncode(AlCopyStr(Str, 1, P-1)) + '=' + HTTPEncode(AlCopyStr(Str, P+1, MAXINT))
        else Str := HTTPEncode(Str);
        If i < aPostDataStrings.Count - 1 then aURLEncodedContentStream.WriteString(Str + '&')
        else aURLEncodedContentStream.WriteString(Str);
      end;
    end
    else begin
      for i := 0 to aPostDataStrings.Count - 1 do begin
        If i < aPostDataStrings.Count - 1 then aURLEncodedContentStream.WriteString(aPostDataStrings[i] + '&')
        else aURLEncodedContentStream.WriteString(aPostDataStrings[i]);
      end;
    end;

    FrequestHeader.ContentType := 'application/x-www-form-urlencoded';
    post(aUrl,
         aURLEncodedContentStream,
         aResponseContentStream,
         aResponseContentHeader);
  finally
    aURLEncodedContentStream.free;
    FrequestHeader.ContentType := OldRequestContentType;
  end;
end;

{**************************************************}
procedure TALHTTPClient.Head(const aUrl: AnsiString;
                             aResponseContentStream: TStream;
                             aResponseContentHeader: TALHTTPResponseHeader);
begin
  Url := aURL;
  RequestMethod := HTTPmt_head;
  Execute(nil,
          aResponseContentStream,
          aResponseContentHeader);
end;

{***************************************************}
procedure TALHTTPClient.Trace(const aUrl: AnsiString;
                              aResponseContentStream: TStream;
                              aResponseContentHeader: TALHTTPResponseHeader);
begin
  Url := aURL;
  RequestMethod := HTTPmt_Trace;
  Execute(nil,
          aResponseContentStream,
          aResponseContentHeader);
end;

{************************************************}
procedure TALHTTPClient.Put(const aUrl:AnsiString;
                            aPutDataStream: TStream;
                            aResponseContentStream: TStream;
                            aResponseContentHeader: TALHTTPResponseHeader);
Var OldContentLengthValue: AnsiString;
begin
  Url := aURL;
  RequestMethod := HTTPmt_Put;
  OldContentLengthValue := FrequestHeader.ContentLength;
  try
    If assigned(aPutDataStream) then FrequestHeader.ContentLength := ALIntToStr(aPutDataStream.Size)
    else FrequestHeader.ContentLength := '0';
    Execute(aPutDataStream,
            aResponseContentStream,
            aResponseContentHeader);
  finally
    FrequestHeader.ContentLength := OldContentLengthValue;
  end;
end;

{****************************************************}
procedure TALHTTPClient.Delete(const aUrl: AnsiString;
                               aResponseContentStream: TStream;
                               aResponseContentHeader: TALHTTPResponseHeader);
begin
  Url := aURL;
  RequestMethod := HTTPmt_Delete;
  Execute(nil,
          aResponseContentStream,
          aResponseContentHeader);
end;

{*************************************************************}
function TALHTTPClient.Get(const aUrl: AnsiString): AnsiString;
var aResponseContentStream: TALStringStream;
begin
  aResponseContentStream := TALStringStream.Create('');
  try
    Get(aUrl,
        aResponseContentStream,
        nil);
    result := aResponseContentStream.DataString;
  finally
    aResponseContentStream.Free;
  end;
end;

{************************************************}
Procedure TALHTTPClient.Get(const aUrl:AnsiString;
                            aParams: TALStrings;
                            aResponseContentStream: TStream;
                            aResponseContentHeader: TALHTTPResponseHeader;
                            Const EncodeParams: Boolean=True);
Var Query: AnsiString;
    Str: AnsiString;
    I, P: Integer;
begin

  Query := '';
  for i := 0 to aParams.Count - 1 do begin
    Str := aParams[i];
    P := AlPos(aParams.NameValueSeparator, Str);
    if EncodeParams then begin
      if P > 0 then Query := Query + HTTPEncode(AlCopyStr(Str, 1, P-1)) + '=' + HTTPEncode(AlCopyStr(Str, P+1, MAXINT)) + ALIfThen(i < aParams.Count - 1, '&')
      else Query := Query + HTTPEncode(Str) + ALIfThen(i < aParams.Count - 1, '&')
    end
    else begin
      if P > 0 then Query := Query + AlCopyStr(Str, 1, P-1) + '=' + AlCopyStr(Str, P+1, MAXINT) + ALIfThen(i < aParams.Count - 1, '&')
      else Query := Query + Str + ALIfThen(i < aParams.Count - 1, '&')
    end;
  end;
  if Query <> '' then begin
    P := ALpos('?', aUrl);
    if P <= 0 then Query := '?' + Query
    else if P <> length(aUrl) then Query := '&' + Query;
  end;

  Get(aUrl + Query,
      aResponseContentStream,
      aResponseContentHeader);

end;

{***********************************************}
Function TALHTTPClient.Get(const aUrl:AnsiString;
                           aParams: TALStrings;
                           Const EncodeParams: Boolean=True): AnsiString;
var aResponseContentStream: TALStringStream;
begin
  aResponseContentStream := TALStringStream.Create('');
  try
    Get(aUrl,
        aParams,
        aResponseContentStream,
        nil,
        EncodeParams);
    result := aResponseContentStream.DataString;
  finally
    aResponseContentStream.Free;
  end;
end;

{****************************************************************************************}
function TALHTTPClient.Post(const aUrl: AnsiString; aPostDataStream: TStream): AnsiString;
var aResponseContentStream: TALStringStream;
begin
  aResponseContentStream := TALStringStream.Create('');
  try
    post(aUrl,
         aPostDataStream,
         aResponseContentStream,
         nil);
    result := aResponseContentStream.DataString;
  finally
    aResponseContentStream.Free;
  end;
end;

{**************************************************************}
function TALHTTPClient.Post(const aUrl: AnsiString): AnsiString;
begin
  Result := Post(aUrl, nil);
end;

{******************************************************************}
function TALHTTPClient.PostMultiPartFormData(const aUrl: AnsiString;
                                             aPostDataStrings: TALStrings;
                                             aPostDataFiles: TALMultiPartFormDataContents): AnsiString;
Var aMultipartFormDataEncoder: TALMultipartFormDataEncoder;
    OldRequestContentType: AnsiString;
begin
  aMultipartFormDataEncoder := TALMultipartFormDataEncoder.create;
  OldRequestContentType := FrequestHeader.ContentType;
  try
    aMultipartFormDataEncoder.Encode(aPostDataStrings, aPostDataFiles);
    FrequestHeader.ContentType := 'multipart/form-data; boundary='+aMultipartFormDataEncoder.DataStream.Boundary;
    Result := post(aUrl, aMultipartFormDataEncoder.DataStream);
  finally
    aMultipartFormDataEncoder.free;
    FrequestHeader.ContentType := OldRequestContentType;
  end;
end;

{***********************************************************}
function TALHTTPClient.PostUrlEncoded(const aUrl: AnsiString;
                                      aPostDataStrings: TALStrings;
                                      Const EncodeParams: Boolean=True): AnsiString;
Var aURLEncodedContentStream: TALStringStream;
    OldRequestContentType: AnsiString;
    Str: AnsiString;
    I, P: Integer;
begin
  aURLEncodedContentStream := TALStringStream.create('');
  OldRequestContentType := FrequestHeader.ContentType;
  try

    if EncodeParams then begin
      for i := 0 to aPostDataStrings.Count - 1 do begin
        Str := aPostDataStrings[i];
        P := AlPos(aPostDataStrings.NameValueSeparator, Str);
        if P > 0 then Str := HTTPEncode(AlCopyStr(Str, 1, P-1)) + '=' + HTTPEncode(AlCopyStr(Str, P+1, MAXINT))
        else Str := HTTPEncode(Str);
        If i < aPostDataStrings.Count - 1 then aURLEncodedContentStream.WriteString(Str + '&')
        else aURLEncodedContentStream.WriteString(Str);
      end;
    end
    else begin
      for i := 0 to aPostDataStrings.Count - 1 do begin
        If i < aPostDataStrings.Count - 1 then aURLEncodedContentStream.WriteString(aPostDataStrings[i] + '&')
        else aURLEncodedContentStream.WriteString(aPostDataStrings[i]);
      end;
    end;

    FrequestHeader.ContentType := 'application/x-www-form-urlencoded';
    Result := post(aUrl, aURLEncodedContentStream);
  finally
    aURLEncodedContentStream.free;
    FrequestHeader.ContentType := OldRequestContentType;
  end;
end;

{**************************************************************}
function TALHTTPClient.Head(const aUrl:AnsiString) : AnsiString;
var aResponseContentStream: TALStringStream;
begin
  aResponseContentStream := TALStringStream.Create('');
  try
    Head(aUrl,
         aResponseContentStream,
         nil);
    result := aResponseContentStream.DataString;
  finally
    aResponseContentStream.Free;
  end;
end;

{**************************************************************}
function TALHTTPClient.Trace(const aUrl:AnsiString): AnsiString;
var aResponseContentStream: TALStringStream;
begin
  aResponseContentStream := TALStringStream.Create('');
  try
    Trace(aUrl,
          aResponseContentStream,
          nil);
    result := aResponseContentStream.DataString;
  finally
    aResponseContentStream.Free;
  end;
end;

{**************************************************************************************}
function TALHTTPClient.Put(const aURL: Ansistring; aPutDataStream: TStream): AnsiString;
var aResponseContentStream: TALStringStream;
begin
  aResponseContentStream := TALStringStream.Create('');
  try
    put(aUrl,
        aPutDataStream,
        aResponseContentStream,
        nil);
    result := aResponseContentStream.DataString;
  finally
    aResponseContentStream.Free;
  end;
end;

{****************************************************************}
function TALHTTPClient.Delete(const aURL: Ansistring): AnsiString;
var aResponseContentStream: TALStringStream;
begin
  aResponseContentStream := TALStringStream.Create('');
  try
    Delete(aUrl,
           aResponseContentStream,
           nil);
    result := aResponseContentStream.DataString;
  finally
    aResponseContentStream.Free;
  end;
end;

{*****************************************************************************************}
procedure TALHTTPClient.OnProxyParamsChange(sender: Tobject; Const PropertyIndex: Integer);
begin
 //virtual
end;

{*******************************************************************************************}
procedure TALHTTPClient.OnRequestHeaderChange(sender: Tobject; Const PropertyIndex: Integer);
begin
 //virtual
end;

{****************************************************************}
procedure TALHTTPClient.SetUploadBufferSize(const Value: Integer);
begin
  If Value >= 0 then FUploadBufferSize := Value;
end;

{***************************************}
procedure TALHTTPClientProxyParams.Clear;
begin
  FProxyBypass := '';
  FproxyServer := '';
  FProxyUserName := '';
  FProxyPassword := '';
  FproxyPort := 0;
  DoChange(-1);
end;

{******************************************}
constructor TALHTTPClientProxyParams.Create;
Begin
  inherited create;
  FProxyBypass := '';
  FproxyServer := '';
  FProxyUserName := '';
  FProxyPassword := '';
  FproxyPort := 0;
  FOnchange := nil;
end;

{******************************************************************}
procedure TALHTTPClientProxyParams.DoChange(propertyIndex: Integer);
begin
  if assigned(FonChange) then FonChange(Self,propertyIndex);
end;

{*************************************************************************}
procedure TALHTTPClientProxyParams.SetProxyBypass(const Value: AnsiString);
begin
  If (Value <> FProxyBypass) then begin
    FProxyBypass := Value;
    DoChange(0);
  end;
end;

{***************************************************************************}
procedure TALHTTPClientProxyParams.SetProxyPassword(const Value: AnsiString);
begin
  If (Value <> FProxyPassword) then begin
    FProxyPassword := Value;
    DoChange(4);
  end;
end;

{********************************************************************}
procedure TALHTTPClientProxyParams.SetProxyPort(const Value: integer);
begin
  If (Value <> FProxyPort) then begin
    FProxyPort := Value;
    DoChange(2);
  end;
end;

{*************************************************************************}
procedure TALHTTPClientProxyParams.SetProxyServer(const Value: AnsiString);
begin
  If (Value <> FProxyServer) then begin
    FProxyServer := Value;
    DoChange(1);
  end;
end;

{***************************************************************************}
procedure TALHTTPClientProxyParams.SetProxyUserName(const Value: AnsiString);
begin
  If (Value <> FProxyUserName) then begin
    FProxyUserName := Value;
    DoChange(3);
  end;
end;

end.

