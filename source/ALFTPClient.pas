{*************************************************************
www:          http://sourceforge.net/projects/alcinoe/              
svn:          svn checkout svn://svn.code.sf.net/p/alcinoe/code/ alcinoe-code              
Author(s):    Stéphane Vander Clock (skype/email: svanderclock@yahoo.fr)

product:      ALFTPClient Base Classe
Version:      4.00

Description:  TALFTPClient is a ancestor base class of
              TALWinInetFTPClient

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

Link :

**************************************************************}
unit ALFTPClient;

interface

{$IF CompilerVersion >= 25} {Delphi XE4}
  {$LEGACYIFEND ON} // http://docwiki.embarcadero.com/RADStudio/XE4/en/Legacy_IFEND_(Delphi)
{$IFEND}

uses {$IF CompilerVersion >= 23} {Delphi XE2}
     Winapi.Windows,
     System.SysUtils,
     System.Classes;
     {$ELSE}
     Windows,
     SysUtils,
     Classes;
     {$IFEND}

type

  {--------------------------------------}
  EALFTPClientException = class(Exception)
  private
  public
  end;

  {-- onchange Event that specify the property index that is just changed --}
  TALFTPPropertyChangeEvent = procedure(sender: Tobject; Const PropertyIndex: Integer) of object;

  {--------------------------------------}
  TALFTPClientProxyParams = Class(TObject)
  Private
    FProxyBypass: AnsiString;
    FproxyServer: AnsiString;
    FProxyUserName: AnsiString;
    FProxyPassword: AnsiString;
    FproxyPort: integer;
    FOnChange: TALFTPPropertyChangeEvent;
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
    Name: AnsiString;
    ExcludeAttr: Integer;
    FindHandle: Pointer;
    FindData: TWin32FindDataA;
  end;

  {---------------------------}
  TALFTPClient = class(TObject)
  private
    FProxyParams: TALFTPClientProxyParams;
    FServerName: AnsiString;
    FServerPort: Integer;
    FUserName: AnsiString;
    FPassword: AnsiString;
    FConnectTimeout: Integer;
    FSendTimeout: Integer;
    FReceiveTimeout: Integer;
    FOnUploadProgress: TALFTPClientUploadProgressEvent;
    FOnDownloadProgress: TALFTPClientDownloadProgressEvent;
    FUploadBufferSize: Integer;
  protected
    procedure SetServerName(const Value: AnsiString); virtual;
    procedure SetServerPort(const Value: Integer); virtual;
    procedure SetUsername(const NameValue: AnsiString); virtual;
    procedure SetPassword(const PasswordValue: AnsiString); virtual;
    function  GetConnected: Boolean; virtual;
    procedure SetConnected(const Value: Boolean); virtual;
    procedure OnProxyParamsChange(sender: Tobject; Const PropertyIndex: Integer); virtual;
    procedure SetUploadBufferSize(const Value: Integer); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure CreateDirectory(const Directory: AnsiString); virtual;
    procedure DeleteFile(const FileName: AnsiString); virtual;
    Function  FindFirst(const Path: AnsiString;
                        const Attr: Integer;
                        var F: TALFtpclientSearchRec): Integer; virtual;
    Function  FindNext(var F: TALFtpclientSearchRec): Integer; virtual;
    procedure FindClose(var F: TALFtpclientSearchRec); virtual;
    Function  GetCurrentDirectory: AnsiString; virtual;
    Procedure GetFile(const RemoteFile: AnsiString;
                      const LocalFile: AnsiString;
                      FailIfExists: Boolean); overload; virtual;
    Procedure GetFile(const RemoteFile: AnsiString;
                      DataStream: Tstream); overload; virtual;
    Function  GetFileSize(const filename: AnsiString): Longword; virtual;
    Procedure PutFile(const LocalFile: AnsiString;
                      const Remotefile: AnsiString); overload; virtual;
    Procedure PutFile(DataStream: TStream;
                      const Remotefile: AnsiString); overload; virtual;
    Procedure RemoveDirectory(const Directory: AnsiString); virtual;
    Procedure RenameFile(const ExistingFile: AnsiString;
                         const NewFile: AnsiString); virtual;
    Procedure SetCurrentDirectory(const Directory: AnsiString); virtual;
    procedure Connect; virtual;
    procedure Disconnect; virtual;
    Property  connected: Boolean read GetConnected write SetConnected default False;
    property  ServerName: AnsiString read FServerName write SetServerName;
    property  ServerPort: integer read FServerPort write SetServerPort default 21;
    property  UserName: AnsiString read FUserName write SetUserName;
    property  Password: AnsiString read FPassword write SetPassword;
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

{******************************}
constructor TALFTPClient.Create;
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

{**************************************************************}
procedure TALFTPClient.SetUsername(const NameValue: AnsiString);
begin
  FUserName := NameValue;
end;

{******************************************************************}
procedure TALFTPClient.SetPassword(const PasswordValue: AnsiString);
begin
  FPassword := PasswordValue;
end;

{************************************************************}
procedure TALFTPClient.SetServerName(const Value: AnsiString);
begin
  FServerName := Value;
end;

{*********************************************************}
procedure TALFTPClient.SetServerPort(const Value: Integer);
begin
  FServerPort := Value;
end;

{****************************************************************************************}
procedure TALFTPClient.OnProxyParamsChange(sender: Tobject; Const PropertyIndex: Integer);
begin
 //virtual
end;

{***************************************************************}
procedure TALFTPClient.SetUploadBufferSize(const Value: Integer);
begin
  If Value >= 0 then FUploadBufferSize := Value;
end;

{******************************************************************}
procedure TALFTPClient.CreateDirectory(const Directory: AnsiString);
begin
//virtual
end;

{************************************************************}
procedure TALFTPClient.DeleteFile(const FileName: AnsiString);
begin
//virtual
end;

{*****************************************************}
function TALFTPClient.FindFirst(const Path: AnsiString;
                                const Attr: Integer;
                                var F: TALFtpclientSearchRec): Integer;
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

{****************************************************}
function TALFTPClient.GetCurrentDirectory: AnsiString;
begin
  //virtual
  Result := '';
end;

{**********************************************************}
procedure TALFTPClient.GetFile(const RemoteFile: AnsiString;
                               DataStream: Tstream);
begin
  //virtual
end;

{**********************************************************}
procedure TALFTPClient.GetFile(const RemoteFile: AnsiString;
                               const LocalFile: AnsiString;
                               FailIfExists: Boolean);
begin
//virtual
end;

{**********************************************************************}
function TALFTPClient.GetFileSize(const filename: AnsiString): Longword;
begin
  //virtual
  Result := 0;
end;

{*************************************************}
procedure TALFTPClient.PutFile(DataStream: TStream;
                               const Remotefile: AnsiString);
begin
//virtual
end;

{*********************************************************}
procedure TALFTPClient.PutFile(const LocalFile: AnsiString;
                               const Remotefile: AnsiString);
begin
//virtual
end;

{******************************************************************}
procedure TALFTPClient.RemoveDirectory(const Directory: AnsiString);
begin
//virtual
end;

{***************************************************************}
procedure TALFTPClient.RenameFile(const ExistingFile: AnsiString;
                                  const NewFile: AnsiString);
begin
//virtual
end;

{**********************************************************************}
procedure TALFTPClient.SetCurrentDirectory(const Directory: AnsiString);
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

{**************************************}
procedure TALFTPClientProxyParams.Clear;
begin
  FProxyBypass := '';
  FproxyServer := '';
  FProxyUserName := '';
  FProxyPassword := '';
  FproxyPort := 0;
  DoChange(-1);
end;

{*****************************************}
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

{*****************************************************************}
procedure TALFTPClientProxyParams.DoChange(propertyIndex: Integer);
begin
  if assigned(FonChange) then FonChange(Self,propertyIndex);
end;

{************************************************************************}
procedure TALFTPClientProxyParams.SetProxyBypass(const Value: AnsiString);
begin
  If (Value <> FProxyBypass) then begin
    FProxyBypass := Value;
    DoChange(0);
  end;
end;

{**************************************************************************}
procedure TALFTPClientProxyParams.SetProxyPassword(const Value: AnsiString);
begin
  If (Value <> FProxyPassword) then begin
    FProxyPassword := Value;
    DoChange(4);
  end;
end;

{*******************************************************************}
procedure TALFTPClientProxyParams.SetProxyPort(const Value: integer);
begin
  If (Value <> FProxyPort) then begin
    FProxyPort := Value;
    DoChange(2);
  end;
end;

{************************************************************************}
procedure TALFTPClientProxyParams.SetProxyServer(const Value: AnsiString);
begin
  If (Value <> FProxyServer) then begin
    FProxyServer := Value;
    DoChange(1);
  end;
end;

{**************************************************************************}
procedure TALFTPClientProxyParams.SetProxyUserName(const Value: AnsiString);
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
