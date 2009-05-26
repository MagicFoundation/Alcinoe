{*************************************************************
www:          http://sourceforge.net/projects/alcinoe/              
Author(s):    Stéphane Vander Clock (svanderclock@arkadia.com)
Sponsor(s):   Arkadia SA (http://www.arkadia.com)

product:      ALFTPClient Base Classe
Version:      3.50

Description:  TALFTPClient is a ancestor base class of
              TALWinInetFTPClient

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
unit ALFTPClient;

interface

uses Windows,
     SysUtils,
     Classes;

type

  {--------------------------------------}
  EALFTPClientException = class(Exception)
  private
  public
  end;

  {-- onchange Event that specify the property index that is just changed --}
  TALFTPPropertyChangeEvent = procedure(sender: Tobject; Const PropertyIndex: Integer) of object;

  {------------------------------------------}
  TALFTPClientProxyParams = Class(TPersistent)
  Private
    FProxyBypass: String;
    FproxyServer: String;
    FProxyUserName: String;
    FProxyPassword: String;
    FproxyPort: integer;
    FOnChange: TALFTPPropertyChangeEvent;
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
    property OnChange: TALFTPPropertyChangeEvent read FOnChange write FOnChange;
  end;

  {------------------------------------------------------------------------------------------------------}
  TALFTPClientUploadProgressEvent   = procedure(sender: Tobject; Sent: Integer; Total: Integer) of object;
  TALFTPClientDownloadProgressEvent = procedure(sender: Tobject; Read: Integer; Total: Integer) of object;

  {----------------------------}
  TALFtpclientSearchRec = record
    Time: Integer;
    Size: Integer;
    Attr: Integer;
    Name: TFileName;
    ExcludeAttr: Integer;
    FindHandle: Pointer;
    FindData: TWin32FindData;
  end;

  {-------------------------------}
  TALFTPClient = class(TComponent)
  private
    FProxyParams: TALFTPClientProxyParams;
    FServerName: string;
    FServerPort: Integer;
    FUserName: string;
    FPassword: string;
    FConnectTimeout: Integer;
    FSendTimeout: Integer;
    FReceiveTimeout: Integer;
    FOnUploadProgress: TALFTPClientUploadProgressEvent;
    FOnDownloadProgress: TALFTPClientDownloadProgressEvent;
    FUploadBufferSize: Integer;
  protected
    procedure SetServerName(const Value: string); virtual;
    procedure SetServerPort(const Value: Integer); virtual;
    procedure SetUsername(const NameValue: string); virtual;
    procedure SetPassword(const PasswordValue: string); virtual;
    function  GetConnected: Boolean; virtual;
    procedure SetConnected(const Value: Boolean); virtual;
    procedure OnProxyParamsChange(sender: Tobject; Const PropertyIndex: Integer); virtual;
    procedure SetUploadBufferSize(const Value: Integer); virtual;
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    procedure CreateDirectory(Directory: String); virtual;
    procedure DeleteFile(FileName: String); virtual;
    Function  FindFirst(const Path: string; Attr: Integer; var F: TALFtpclientSearchRec): Integer; virtual;
    Function  FindNext(var F: TALFtpclientSearchRec): Integer; virtual;
    procedure FindClose(var F: TALFtpclientSearchRec); virtual;
    Function  GetCurrentDirectory: String; virtual;
    Procedure GetFile(RemoteFile: String; LocalFile: String; FailIfExists: Boolean); overload; virtual;
    Procedure GetFile(RemoteFile: String; DataStream: Tstream); overload; virtual;
    Function  GetFileSize(filename: String): Longword; virtual;
    Procedure PutFile(LocalFile: String; Remotefile: String); overload; virtual;
    Procedure PutFile(DataStream: TStream; Remotefile: String); overload; virtual;
    Procedure RemoveDirectory(Directory: String); virtual;
    Procedure RenameFile(ExistingFile, NewFile: String); virtual;
    Procedure SetCurrentDirectory(Directory: String); virtual;
    procedure Connect; virtual;
    procedure Disconnect; virtual;
  published
    Property  connected: Boolean read GetConnected write SetConnected default False;
    property  ServerName: string read FServerName write SetServerName;
    property  ServerPort: integer read FServerPort write SetServerPort default 21;
    property  UserName: string read FUserName write SetUserName;
    property  Password: string read FPassword write SetPassword;
    property  ConnectTimeout: Integer read FConnectTimeout write FConnectTimeout default 0;
    property  SendTimeout: Integer read FSendTimeout write FSendTimeout default 0;
    property  ReceiveTimeout: Integer read FReceiveTimeout write FReceiveTimeout default 0;
    property  UploadBufferSize: Integer read FUploadBufferSize write SetUploadBufferSize default $8000;
    property  ProxyParams: TALFTPClientProxyParams read FProxyParams;
    property  OnUploadProgress: TALFTPClientUploadProgressEvent read FOnUploadProgress write FOnUploadProgress;
    property  OnDownloadProgress: TALFTPClientDownloadProgressEvent read FonDownloadProgress write FonDownloadProgress;
  end;

ResourceString
  CALFtpCLient_MsgNotConnected         = 'Not connected';
  CALFtpCLient_MsgInvalidFtpRequest    = 'Invalid Ftp Request';  

implementation

////////////////////////////////////////////////////////////////////////////////
////////// TALWinInetFTPClient ////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

{**************************************************}
constructor TALFTPClient.Create(Owner: TComponent);
begin
  inherited;
  FUploadBufferSize := $8000;
  FConnectTimeout := 0;
  FSendTimeout := 0;
  FReceiveTimeout := 0;
  FServerName := '';
  FServerPort := 21;
  FUserName := '';
  FPassword := '';
  FOnUploadProgress := nil;
  FOnDownloadProgress := nil;
  FProxyParams := TALFTPClientProxyParams.Create;
  FProxyParams.OnChange := OnProxyParamsChange;
end;

{*******************************}
destructor TALFTPClient.Destroy;
begin
  FProxyParams.free;
  inherited;
end;

{***********************************************************}
procedure TALFTPClient.SetUsername(const NameValue: string);
begin
  FUserName := NameValue;
end;

{***************************************************************}
procedure TALFTPClient.SetPassword(const PasswordValue: string);
begin
  FPassword := PasswordValue;
end;

{********************************************************}
procedure TALFTPClient.SetServerName(const Value: string);
begin
  FServerName := Value;
end;

{*********************************************************}
procedure TALFTPClient.SetServerPort(const Value: Integer);
begin
  FServerPort := Value;
end;

{*****************************************************************************************}
procedure TALFTPClient.OnProxyParamsChange(sender: Tobject; Const PropertyIndex: Integer);
begin
 //virtual
end;

{****************************************************************}
procedure TALFTPClient.SetUploadBufferSize(const Value: Integer);
begin
  If Value >= 0 then FUploadBufferSize := Value;
end;

{********************************************************}
procedure TALFTPClient.CreateDirectory(Directory: String);
begin
//virtual
end;

{**************************************************}
procedure TALFTPClient.DeleteFile(FileName: String);
begin
//virtual
end;

{********************************************************************************************************}
function TALFTPClient.FindFirst(const Path: string; Attr: Integer; var F: TALFtpclientSearchRec): Integer;
begin
  //virtual
  Result := 0;
end;

{********************************************************************}
function TALFTPClient.FindNext(var F: TALFtpclientSearchRec): Integer;
begin
  //virtual
  Result := 0;
end;

{*************************************************************}
procedure TALFTPClient.FindClose(var F: TALFtpclientSearchRec);
begin
//virtual
end;

{************************************************}
function TALFTPClient.GetCurrentDirectory: String;
begin
  //virtual
  Result := '';
end;

{**********************************************************************}
procedure TALFTPClient.GetFile(RemoteFile: String; DataStream: Tstream);
begin
  //virtual
end;

{***********************************************************************************}
procedure TALFTPClient.GetFile(RemoteFile, LocalFile: String; FailIfExists: Boolean);
begin
//virtual
end;

{************************************************************}
function TALFTPClient.GetFileSize(filename: String): Longword;
begin
  //virtual
  Result := 0;
end;

{**********************************************************************}
procedure TALFTPClient.PutFile(DataStream: TStream; Remotefile: String);
begin
//virtual
end;

{************************************************************}
procedure TALFTPClient.PutFile(LocalFile, Remotefile: String);
begin
//virtual
end;

{********************************************************}
procedure TALFTPClient.RemoveDirectory(Directory: String);
begin
//virtual
end;

{***************************************************************}
procedure TALFTPClient.RenameFile(ExistingFile, NewFile: String);
begin
//virtual
end;

{************************************************************}
procedure TALFTPClient.SetCurrentDirectory(Directory: String);
begin
//virtual
end;

{*****************************}
procedure TALFTPClient.Connect;
begin
//virtual
end;

{********************************}
procedure TALFTPClient.Disconnect;
begin
//virtual
end;




///////////////////////////////////////////////////////////////////////////////////////
////////// TALFTPClientProxyParams ///////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////

{*************************************************************}
procedure TALFTPClientProxyParams.AssignTo(Dest: TPersistent);
begin
  if Dest is TALFTPClientProxyParams then begin
    with Dest as TALFTPClientProxyParams do begin
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
procedure TALFTPClientProxyParams.Clear;
begin
  FProxyBypass := '';
  FproxyServer := '';
  FProxyUserName := '';
  FProxyPassword := '';
  FproxyPort := 0;
  DoChange(-1);
end;

{******************************************}
constructor TALFTPClientProxyParams.Create;
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
procedure TALFTPClientProxyParams.DoChange(propertyIndex: Integer);
begin
  if assigned(FonChange) then FonChange(Self,propertyIndex);
end;

{*********************************************************************}
procedure TALFTPClientProxyParams.SetProxyBypass(const Value: String);
begin
  If (Value <> FProxyBypass) then begin
    FProxyBypass := Value;
    DoChange(0);
  end;
end;

{***********************************************************************}
procedure TALFTPClientProxyParams.SetProxyPassword(const Value: String);
begin
  If (Value <> FProxyPassword) then begin
    FProxyPassword := Value;
    DoChange(4);
  end;
end;

{********************************************************************}
procedure TALFTPClientProxyParams.SetProxyPort(const Value: integer);
begin
  If (Value <> FProxyPort) then begin
    FProxyPort := Value;
    DoChange(2);
  end;
end;

{*********************************************************************}
procedure TALFTPClientProxyParams.SetProxyServer(const Value: String);
begin
  If (Value <> FProxyServer) then begin
    FProxyServer := Value;
    DoChange(1);
  end;
end;

{***********************************************************************}
procedure TALFTPClientProxyParams.SetProxyUserName(const Value: String);
begin
  If (Value <> FProxyUserName) then begin
    FProxyUserName := Value;
    DoChange(3);
  end;
end;

{******************************************}
function TALFTPClient.GetConnected: Boolean;
begin
  //virtual;
  result := false;
end;

{********************************************************}
procedure TALFTPClient.SetConnected(const Value: Boolean);
begin
  //virtual;
end;

end.
