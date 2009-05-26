{*************************************************************
www:          http://sourceforge.net/projects/alcinoe/              
Author(s):    Stéphane Vander Clock (svanderclock@arkadia.com)
Sponsor(s):   Arkadia SA (http://www.arkadia.com)

product:      ALHttpClient Base Class
Version:      3.50

Description:  TALHttpClient is a ancestor of class like
              TALWinInetHttpClient or TALWinHttpClient

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

History :     28/11/2005: move public procedure to published
                          in TALHttpClientProxyParams

Link :        http://www.w3.org/TR/REC-html40/interact/forms.html#h-17.1
              http://www.ietf.org/rfc/rfc1867.txt
              http://www.ietf.org/rfc/rfc2388.txt
              http://www.w3.org/MarkUp/html-spec/html-spec_8.html
              http://www.cs.tut.fi/~jkorpela/forms/methods.html

Please send all your feedback to svanderclock@arkadia.com
**************************************************************}
unit ALHttpClient;

interface

uses SysUtils,
     Classes,
     ALHttpCommon,
     ALMultiPartFormDataParser;

type

  {---------------------------------------}
  EALHTTPClientException = class(Exception)
  private
    FStatusCode: Integer;
  public
    constructor Create(const Msg: string; SCode: Integer = 0);
    constructor CreateFmt(const Msg: string; const Args: array of const; SCode: Integer = 0);
    property StatusCode: Integer read FStatusCode write FStatusCode;
  end;

  {-------------------------------------------}
  TALHTTPClientProxyParams = Class(TPersistent)
  Private
    FProxyBypass: String;
    FproxyServer: String;
    FProxyUserName: String;
    FProxyPassword: String;
    FproxyPort: integer;
    FOnChange: TALHTTPPropertyChangeEvent;
    procedure SetProxyBypass(const Value: String);
    procedure SetProxyPassword(const Value: String);
    procedure SetProxyPort(const Value: integer);
    procedure SetProxyServer(const Value: String);
    procedure SetProxyUserName(const Value: String);
    Procedure DoChange(propertyIndex: Integer);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; virtual;
    procedure Clear;
  published
    Property ProxyBypass: String read FProxyBypass write SetProxyBypass; //index 0
    property ProxyServer: String read FProxyServer write SetProxyServer; //index 1
    property ProxyPort: integer read FProxyPort write SetProxyPort default 0; //index 2
    property ProxyUserName: String read FProxyUserName write SetProxyUserName; //index 3
    property ProxyPassword: String read FProxyPassword write SetProxyPassword; //index 4
    property OnChange: TALHTTPPropertyChangeEvent read FOnChange write FOnChange;
  end;

  {----------------------------------------------------------------------------------------------}
  TAlHTTPClientRedirectEvent         = procedure(sender: Tobject; const NewURL: String) of object;
  TALHTTPClientUploadProgressEvent   = procedure(sender: Tobject; Sent: Integer; Total: Integer) of object;
  TALHTTPClientDownloadProgressEvent = procedure(sender: Tobject; Read: Integer; Total: Integer) of object;

  {-------------------------------}
  TALHTTPClient = class(TComponent)
  private
    FProxyParams: TALHTTPClientProxyParams;
    FRequestHeader: TALHTTPRequestHeader;
    FProtocolVersion: TALHTTPProtocolVersion;
    FRequestMethod: TALHTTPRequestMethod;
    FURL: string;
    FUserName: string;
    FPassword: string;
    FConnectTimeout: Integer;
    FSendTimeout: Integer;
    FReceiveTimeout: Integer;
    FOnUploadProgress: TALHTTPClientUploadProgressEvent;
    FOnDownloadProgress: TALHTTPClientDownloadProgressEvent;
    FOnRedirect: TAlHTTPClientRedirectEvent;
    FUploadBufferSize: Integer;
  protected
    procedure SetURL(const Value: string); virtual;
    procedure SetUsername(const NameValue: string); virtual;
    procedure SetPassword(const PasswordValue: string); virtual;
    procedure OnProxyParamsChange(sender: Tobject; Const PropertyIndex: Integer); virtual;
    procedure OnRequestHeaderChange(sender: Tobject; Const PropertyIndex: Integer); virtual;
    procedure SetUploadBufferSize(const Value: Integer); virtual;
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    procedure Execute(const aRequestDataStream: TStream; aResponseContentStream: TStream; aResponseContentHeader: TALHTTPResponseHeader); virtual;
    Procedure Get(aUrl:String; aResponseContentStream: TStream; aResponseContentHeader: TALHTTPResponseHeader); overload;
    Procedure Post(aUrl:String; aResponseContentStream: TStream; aResponseContentHeader: TALHTTPResponseHeader); overload;
    Procedure Post(aUrl:String; aPostDataStream: TStream; aResponseContentStream: TStream; aResponseContentHeader: TALHTTPResponseHeader); overload;
    Procedure PostUrlEncoded(aUrl:String; aPostDataStrings: TStrings; aResponseContentStream: TStream; aResponseContentHeader: TALHTTPResponseHeader; Const EncodeParams: Boolean=True); overload;
    Procedure PostMultipartFormData(aUrl:String; aPostDataStrings: TStrings; aPostDataFiles: TALMultiPartFormDataContents; aResponseContentStream: TStream; aResponseContentHeader: TALHTTPResponseHeader); overload;
    Function  Get(aUrl:String): String; overload;
    Function  Post(aUrl:String): String; overload;
    Function  Post(aUrl:String; aPostDataStream: TStream): String; overload;
    Function  PostUrlEncoded(aUrl:String; aPostDataStrings: TStrings; Const EncodeParams: Boolean=True): String; overload;
    Function  PostMultiPartFormData(aUrl:String; aPostDataStrings: TStrings; aPostDataFiles: TALMultiPartFormDataContents): String; overload;
  published
    property  URL: string read FURL write SetURL;
    property  ConnectTimeout: Integer read FConnectTimeout write FConnectTimeout default 0;
    property  SendTimeout: Integer read FSendTimeout write FSendTimeout default 0;
    property  ReceiveTimeout: Integer read FReceiveTimeout write FReceiveTimeout default 0;
    property  UploadBufferSize: Integer read FUploadBufferSize write SetUploadBufferSize default $8000;
    property  ProxyParams: TALHTTPClientProxyParams read FProxyParams;
    property  RequestHeader: TALHTTPRequestHeader read FRequestHeader;
    Property  ProtocolVersion: TALHTTPProtocolVersion read FProtocolVersion write FProtocolVersion default HTTPpv_1_1;
    Property  RequestMethod: TALHTTPRequestMethod read FRequestMethod write fRequestMethod default HTTPrm_get;
    property  UserName: string read FUserName write SetUserName;
    property  Password: string read FPassword write SetPassword;
    property  OnUploadProgress: TALHTTPClientUploadProgressEvent read FOnUploadProgress write FOnUploadProgress;
    property  OnDownloadProgress: TALHTTPClientDownloadProgressEvent read FonDownloadProgress write FonDownloadProgress;
    property  OnRedirect: TAlHTTPClientRedirectEvent read FOnRedirect write FOnRedirect;
  end;

ResourceString
  CALHTTPCLient_MsgInvalidURL         = 'Invalid url ''%s'' - only supports ''http'' and ''https'' schemes';
  CALHTTPCLient_MsgInvalidHTTPRequest = 'Invalid HTTP Request: Length is 0';
  CALHTTPCLient_MsgEmptyURL           = 'Empty URL';

implementation

////////////////////////////////////////////////////////////////////////////////
////////// EALHTTPClientException //////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

{*******************************************************************************}
constructor EALHTTPClientException.Create(const Msg: string; SCode: Integer = 0);
begin
  inherited Create(Msg);
  FStatusCode := SCode;
end;

{**************************************************************************************************************}
constructor EALHTTPClientException.CreateFmt(const Msg: string; const Args: array of const; SCode: Integer = 0);
begin
  inherited CreateFmt(Msg, Args);
  FStatusCode := SCode;
end;




////////////////////////////////////////////////////////////////////////////////
////////// TALWinInetHTTPClient ////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

{**************************************************}
constructor TALHTTPClient.Create(Owner: TComponent);
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
  FRequestHeader := TALHTTPRequestHeader.Create(self);
  FRequestHeader.UserAgent := 'Mozilla/3.0 (compatible; TALHTTPClient)';
  FProtocolVersion := HTTPpv_1_1;
  FRequestMethod := HTTPrm_get;
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

{**************************************************}
procedure TALHTTPClient.SetURL(const Value: string);
begin
  Furl := Value;
end;

{***********************************************************}
procedure TALHTTPClient.SetUsername(const NameValue: string);
begin
  FUserName := NameValue;
end;

{***************************************************************}
procedure TALHTTPClient.SetPassword(const PasswordValue: string);
begin
  FPassword := PasswordValue;
end;

{****************************************************************}
procedure TALHTTPClient.Execute(const aRequestDataStream: TStream;
                                aResponseContentStream: TStream;
                                aResponseContentHeader: TALHTTPResponseHeader);
begin
// virtual;
end;

{***************************************}
procedure TALHTTPClient.Get(aUrl: String;
                            aResponseContentStream: TStream;
                            aResponseContentHeader: TALHTTPResponseHeader);
begin
  Url := aURL;
  RequestMethod := HTTPrm_get;
  Execute(
          nil,
          aResponseContentStream,
          aResponseContentHeader
         );
end;

{****************************************}
procedure TALHTTPClient.Post(aUrl: String;
                             aPostDataStream: TStream;
                             aResponseContentStream: TStream;
                             aResponseContentHeader: TALHTTPResponseHeader);
Var OldContentLengthValue: String;
begin
  Url := aURL;
  RequestMethod := HTTPrm_Post;
  OldContentLengthValue := FrequestHeader.ContentLength;
  try
    If assigned(aPostDataStream) then FrequestHeader.ContentLength := inttostr(aPostDataStream.Size)
    else FrequestHeader.ContentLength := '0';
    Execute(
            aPostDataStream,
            aResponseContentStream,
            aResponseContentHeader
           );
  finally
    FrequestHeader.ContentLength := OldContentLengthValue;
  end;
end;

{****************************************}
procedure TALHTTPClient.Post(aUrl: String;
                             aResponseContentStream: TStream;
                             aResponseContentHeader: TALHTTPResponseHeader);
begin
  Post(
       aUrl,
       nil,
       aResponseContentStream,
       aResponseContentHeader
      );
end;

{*********************************************************}
procedure TALHTTPClient.PostMultiPartFormData(aUrl: String;
                                              aPostDataStrings: TStrings;
                                              aPostDataFiles: TALMultiPartFormDataContents;
                                              aResponseContentStream: TStream;
                                              aResponseContentHeader: TALHTTPResponseHeader);
Var aMultipartFormDataEncoder: TALMultipartFormDataEncoder;
    OldRequestContentType: String;
begin
  aMultipartFormDataEncoder := TALMultipartFormDataEncoder.create;
  OldRequestContentType := FrequestHeader.ContentType;
  try
    aMultipartFormDataEncoder.Encode(aPostDataStrings, aPostDataFiles);
    FrequestHeader.ContentType := 'multipart/form-data; boundary='+aMultipartFormDataEncoder.DataStream.Boundary;
    post(
         aUrl,
         aMultipartFormDataEncoder.DataStream,
         aResponseContentStream,
         aResponseContentHeader
        );
  finally
    aMultipartFormDataEncoder.free;
    FrequestHeader.ContentType := OldRequestContentType;
  end;
end;

{**************************************************}
procedure TALHTTPClient.PostUrlEncoded(aUrl: String;
                                       aPostDataStrings: TStrings;
                                       aResponseContentStream: TStream;
                                       aResponseContentHeader: TALHTTPResponseHeader;
                                       Const EncodeParams: Boolean=True);
Var aURLEncodedContentStream: TstringStream;
    OldRequestContentType: String;
    I: Integer;
begin
  aURLEncodedContentStream := TstringStream.create('');
  OldRequestContentType := FrequestHeader.ContentType;
  try

    if EncodeParams then ALHTTPEncodeParamNameValues(aPostDataStrings);
    With aPostDataStrings do
      for i := 0 to Count - 1 do
        If i < Count - 1 then aURLEncodedContentStream.WriteString(Strings[i] + '&')
        else aURLEncodedContentStream.WriteString(Strings[i]);

    FrequestHeader.ContentType := 'application/x-www-form-urlencoded';
    post(
         aUrl,
         aURLEncodedContentStream,
         aResponseContentStream,
         aResponseContentHeader
        );
  finally
    aURLEncodedContentStream.free;
    FrequestHeader.ContentType := OldRequestContentType;
  end;
end;

{***********************************************}
function TALHTTPClient.Get(aUrl: String): String;
var aResponseContentStream: TStringStream;
begin
  aResponseContentStream := TstringStream.Create('');
  try
    Get(
        aUrl,
        aResponseContentStream,
        nil
       );
    result := aResponseContentStream.DataString;
  finally
    aResponseContentStream.Free;
  end;
end;

{**************************************************************************}
function TALHTTPClient.Post(aUrl: String; aPostDataStream: TStream): String;
var aResponseContentStream: TStringStream;
begin
  aResponseContentStream := TstringStream.Create('');
  try
    post(
         aUrl,
         aPostDataStream,
         aResponseContentStream,
         nil
        );
    result := aResponseContentStream.DataString;
  finally
    aResponseContentStream.Free;
  end;
end;

{************************************************}
function TALHTTPClient.Post(aUrl: String): String;
begin
  Result := Post(aUrl, nil);
end;

{********************************************************}
function TALHTTPClient.PostMultiPartFormData(aUrl: String;
                                             aPostDataStrings: TStrings;
                                             aPostDataFiles: TALMultiPartFormDataContents): String;
Var aMultipartFormDataEncoder: TALMultipartFormDataEncoder;
    OldRequestContentType: String;
begin
  aMultipartFormDataEncoder := TALMultipartFormDataEncoder.create;
  OldRequestContentType := FrequestHeader.ContentType;
  try
    aMultipartFormDataEncoder.Encode(aPostDataStrings, aPostDataFiles);
    FrequestHeader.ContentType := 'multipart/form-data; boundary='+aMultipartFormDataEncoder.DataStream.Boundary;
    Result := post(
                   aUrl,
                   aMultipartFormDataEncoder.DataStream
                  );
  finally
    aMultipartFormDataEncoder.free;
    FrequestHeader.ContentType := OldRequestContentType;
  end;
end;

{*************************************************}
function TALHTTPClient.PostUrlEncoded(aUrl: String;
                                      aPostDataStrings: TStrings;
                                      Const EncodeParams: Boolean=True): String;
Var aURLEncodedContentStream: TstringStream;
    OldRequestContentType: String;
    I: Integer;
begin
  aURLEncodedContentStream := TstringStream.create('');
  OldRequestContentType := FrequestHeader.ContentType;
  try

    if EncodeParams then ALHTTPEncodeParamNameValues(aPostDataStrings);
    With aPostDataStrings do
      for i := 0 to Count - 1 do
        If i < Count - 1 then aURLEncodedContentStream.WriteString(Strings[i] + '&')
        else aURLEncodedContentStream.WriteString(Strings[i]);

    FrequestHeader.ContentType := 'application/x-www-form-urlencoded';
    Result := post(
                   aUrl,
                   aURLEncodedContentStream
                  );
  finally
    aURLEncodedContentStream.free;
    FrequestHeader.ContentType := OldRequestContentType;
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




///////////////////////////////////////////////////////////////////////////////////////
////////// TALHTTPClientProxyParams ///////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////

{*************************************************************}
procedure TALHTTPClientProxyParams.AssignTo(Dest: TPersistent);
begin
  if Dest is TALHTTPClientProxyParams then begin
    with Dest as TALHTTPClientProxyParams do begin
      FProxyBypass := self.FProxyBypass;
      FproxyServer := self.FproxyServer;
      FProxyUserName := self.FProxyUserName;
      FProxyPassword := self.FProxyPassword;
      FproxyPort := self.FproxyPort;
    end;
  end
  else inherited AssignTo(Dest);
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

{*********************************************************************}
procedure TALHTTPClientProxyParams.SetProxyBypass(const Value: String);
begin
  If (Value <> FProxyBypass) then begin
    FProxyBypass := Value;
    DoChange(0);
  end;
end;

{***********************************************************************}
procedure TALHTTPClientProxyParams.SetProxyPassword(const Value: String);
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

{*********************************************************************}
procedure TALHTTPClientProxyParams.SetProxyServer(const Value: String);
begin
  If (Value <> FProxyServer) then begin
    FProxyServer := Value;
    DoChange(1);
  end;
end;

{***********************************************************************}
procedure TALHTTPClientProxyParams.SetProxyUserName(const Value: String);
begin
  If (Value <> FProxyUserName) then begin
    FProxyUserName := Value;
    DoChange(3);
  end;
end;

end.
