{*************************************************************
www:          http://sourceforge.net/projects/alcinoe/              
svn:          svn checkout svn://svn.code.sf.net/p/alcinoe/code/ alcinoe-code              
Author(s):    Stéphane Vander Clock (skype/email: svanderclock@yahoo.fr)
							
product:      ALSMTPClient
Version:      4.00

Description:  TALsmtpClient class implements the SMTP protocol (RFC-821)
              Support file attachement using MIME format (RFC-1521, RFC-2045)
              Support authentification (RFC-2104)

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

History :     02/05/2007: add function :
                           function AlSMTPClientExtractEmail(FriendlyEmail: String): String;
                           Function ALSMTPClientmakeFriendlyEmail(aRealName, aEmail: String): String;
                           Function ALSMTPClientEncodeRealName4FriendlyEmail(aRealName: String): String;
                          update function:
                           function AlSMTPClientParseEmail(FriendlyEmail: String; var RealName : String; Const decodeRealName: Boolean=True): String;
              16/10/2007: Replace TalSMTPClientHeader by TALEmailHeader
                          minor update;
              26/06/2012: Add xe2 support

Link :        http://linuxgazette.net/issue45/stumpel.html
              http://www.overbyte.be
              http://msdn.microsoft.com/library/default.asp?url=/library/en-us/winsock/winsock/socket_options.asp
              http://www.fehcom.de/qmail/smtpauth.html
              http://www.freesoft.org/CIE/RFC/821/
              http://www.expita.com/header1.html
              http://cr.yp.to/immhf.html

**************************************************************}
unit ALSMTPClient;

interface

{$IF CompilerVersion >= 25} {Delphi XE4}
  {$LEGACYIFEND ON} // http://docwiki.embarcadero.com/RADStudio/XE4/en/Legacy_IFEND_(Delphi)
{$IFEND}

Uses Winapi.WinSock2,
     ALStringList,
     ALInternetMessages,
     ALMultiPartParser;

type

    {--------------------------------------------}
    TAlSmtpClientAuthType = (AlsmtpClientAuthNone,
                             alsmtpClientAuthPlain,
                             AlsmtpClientAuthLogin,
                             AlsmtpClientAuthCramMD5,
                             AlsmtpClientAuthCramSha1,
                             AlsmtpClientAuthAutoSelect);

    {------------------------------------------------------}
    TAlSmtpClientAuthTypeSet = set of TAlSmtpClientAuthType;

    {----------------------------}
    TAlSmtpClient = class(TObject)
    Private
      Fconnected: Boolean;
      FSocketDescriptor: TSocket;
      FAuthTypesSupported: TAlSmtpClientAuthTypeSet;
      FSendTimeout: Integer;
      FReceiveTimeout: Integer;
      fKeepAlive: Boolean;
      fTCPNoDelay: Boolean;
      procedure SetSendTimeout(const Value: integer);
      procedure SetReceiveTimeout(const Value: integer);
      procedure SetKeepAlive(const Value: boolean);
      procedure SetTCPNoDelay(const Value: boolean);
    protected
      procedure CheckError(Error: Boolean);
      Function SendCmd(const aCmd:AnsiString; const OkResponses: array of Word): AnsiString; virtual;
      Function GetResponse(const OkResponses: array of Word): AnsiString;
      Function SocketWrite(const Buf; len: Integer): Integer; Virtual;
      Function SocketRead(var buf; len: Integer): Integer; Virtual;
    public
      constructor Create; virtual;
      destructor Destroy; override;
      Function Connect(const aHost: AnsiString; const APort: integer): AnsiString; virtual;
      Function Helo: AnsiString; virtual;
      Function Ehlo: AnsiString; virtual;
      Function Auth(const AUserName, APassword: AnsiString; aAuthType: TalSmtpClientAuthType): AnsiString; virtual;
      Function Vrfy(const aUserName: AnsiString): AnsiString; virtual;
      Function MailFrom(const aSenderEmail: AnsiString): AnsiString; virtual;
      Function RcptTo(aRcptNameLst: TALStrings): AnsiString; virtual;
      Function Data(const aMailData: AnsiString): AnsiString; overload; virtual;
      Function Data(const aHeader, aBody: AnsiString): AnsiString; overload; virtual;
      Function Data(aHeader:TALEmailHeader; const aBody: AnsiString): AnsiString; overload; virtual;
      Function DataMultipartMixed(aHeader: TALEmailHeader;
                                  const aInlineText, aInlineTextContentType: AnsiString;
                                  aAttachments: TALMultiPartMixedContents): AnsiString; virtual;
      Function Quit: AnsiString; virtual;
      Function Rset: AnsiString; virtual;
      procedure SendMail(const aHost: AnsiString;
                         APort: integer;
                         const aSenderEmail: AnsiString;
                         aRcptNameLst: TALStrings;
                         const AUserName, APassword: AnsiString;
                         aAuthType: TalSmtpClientAuthType;
                         const aMailData: AnsiString); overload; virtual;
      procedure SendMail(const aHost: AnsiString;
                         APort: integer;
                         const aSenderEmail: AnsiString;
                         aRcptNameLst: TALStrings;
                         const AUserName, APassword: AnsiString;
                         aAuthType: TalSmtpClientAuthType;
                         const aHeader, aBody: AnsiString); overload; virtual;
      procedure SendMailMultipartMixed(const aHost: AnsiString;
                                       APort: integer;
                                       const aSenderEmail: AnsiString;
                                       aRcptNameLst: TALStrings;
                                       const AUserName, APassword: AnsiString;
                                       aAuthType: TalSmtpClientAuthType;
                                       aHeader: TALEmailHeader;
                                       const aInlineText, aInlineTextContentType: AnsiString;
                                       aAttachments: TALMultiPartMixedContents); virtual;
      Procedure Disconnect; virtual;
      Function GetAuthTypeFromEhloResponse(const EhloResponse: AnsiString): TAlSmtpClientAuthTypeSet; virtual;
      property Connected: Boolean read FConnected;
      property SendTimeout: Integer read FSendTimeout write SetSendTimeout;
      property ReceiveTimeout: Integer read FReceiveTimeout write SetReceiveTimeout;
      property KeepAlive: Boolean read fKeepAlive write SetKeepAlive;
      property TcpNoDelay: Boolean read fTCPNoDelay write fTCPNoDelay;
    end;

implementation

Uses Winapi.Windows,
     System.Classes,
     System.SysUtils,
     ALMime,
     ALWinsock,
     ALString;

{*******************************}
constructor TAlSmtpClient.Create;
var aWSAData: TWSAData;
begin
  CheckError(WSAStartup(MAKEWORD(2,2), aWSAData) <> 0);
  Fconnected:= False;
  FSocketDescriptor:= INVALID_SOCKET;
  FAuthTypesSupported:= [];
  FSendTimeout := 60000; // 60 seconds
  FReceiveTimeout := 60000; // 60 seconds
  FKeepAlive := True;
  fTCPNoDelay := True;
end;

{*******************************}
destructor TAlSmtpClient.Destroy;
begin
  If Fconnected then Disconnect;
  WSACleanup;
  inherited;
end;

{*************************************************}
procedure TAlSmtpClient.CheckError(Error: Boolean);
begin
  if Error then RaiseLastOSError;
end;

{****************************************************************************************}
Function TAlSmtpClient.Connect(const aHost: AnsiString; const APort: integer): AnsiString;

  {--------------------------------------------------------------}
  procedure _CallServer(const Server:AnsiString; const Port:word);
  var SockAddr:Sockaddr_in;
      IP: AnsiString;
  begin
    FSocketDescriptor:=Socket(AF_INET,SOCK_STREAM,IPPROTO_TCP);
    CheckError(FSocketDescriptor=INVALID_SOCKET);
    FillChar(SockAddr,SizeOf(SockAddr),0);
    SockAddr.sin_family:=AF_INET;
    SockAddr.sin_port:=swap(Port);
    SockAddr.sin_addr.S_addr:=inet_addr(PAnsiChar(Server));
    If SockAddr.sin_addr.S_addr = INADDR_NONE then begin
      checkError(not ALHostToIP(Server, IP));
      SockAddr.sin_addr.S_addr:=inet_addr(PAnsiChar(IP));
    end;
    CheckError(Winapi.WinSock2.Connect(FSocketDescriptor,TSockAddr(SockAddr),SizeOf(SockAddr))=SOCKET_ERROR);
  end;

begin

  if FConnected then raise EALException.Create('SMTP component already connected');

  Try

    _CallServer(aHost,aPort);
    Fconnected := True;
    SetSendTimeout(FSendTimeout);
    SetReceiveTimeout(FReceiveTimeout);
    SetKeepAlive(FKeepAlive);
    SetTCPNoDelay(fTCPNoDelay);
    FAuthTypesSupported := [];
    Result := GetResponse([220]);

  Except
    Disconnect;
    raise;
  end;

end;

{*********************************}
procedure TAlSmtpClient.Disconnect;
begin
  If Fconnected then begin
    ShutDown(FSocketDescriptor,SD_BOTH);
    CloseSocket(FSocketDescriptor);
    FSocketDescriptor := INVALID_SOCKET;
    Fconnected := False;
  end;
end;

{********************}
{EhloResponse is like:
 250-ec-is.net Hello your_name, ravi de vous rencontrer
 250-VRFY
 250-ETRN
 250-AUTH=LOGIN
 250-AUTH LOGIN CRAM-MD5
 250-8BITMIME
 250 SIZE 0}
Function TAlSmtpClient.GetAuthTypeFromEhloResponse(const EhloResponse: AnsiString): TAlSmtpClientAuthTypeSet;
var k, J: Integer;
    Str1, Str2: AnsiString;
    Lst: TALStringList;
begin
  Result := [];
  Lst := TALStringList.Create;
  Try
    Lst.Text := AlUpperCase(ALTrim(EhloResponse));
    For j := 0 to Lst.Count - 1 do begin
      Str1 := ALTrim(Lst[J]);  //250-AUTH=LOGIN
      Delete(Str1, 1, 4); //AUTH=LOGIN
      Str2 := AlCopyStr(Str1, 1, 5); //AUTH=
      if (str2='AUTH ') or (Str2='AUTH=') then begin
        Str1 := AlCopyStr(Str1, 6, maxint); //LOGIN
        Str1 := AlStringReplace(Str1, '=', ' ', [rfReplaceAll]); //LOGIN
        while (str1 <> '') do begin

          K := AlPos(' ', Str1);
          if K <= 0 then begin
            Str2 := ALTrim(Str1);
            Str1 := '';
          end
          else begin
            Str2 := ALTrim(AlCopyStr(Str1, 1, k - 1));
            Delete(Str1, 1, k);
          end;

          if Str2 = ('PLAIN') then result := result + [AlsmtpClientAuthPlain]
          else if Str2 = ('LOGIN') then result := result + [AlsmtpClientAuthLogin]
          else if Str2 = ('CRAM-MD5') then result := result + [AlsmtpClientAuthCramMD5]
          else if Str2 = ('CRAM-SHA1') then result := result + [AlsmtpClientAuthCramSHA1];

        end;
      end;
    end;
  finally
    Lst.free;
  end;
end;

{****************************************************************************************}
{This command is used to identify the sender-SMTP to the receiver-SMTP. The argument field
 contains the host name of the sender-SMTP. The receiver-SMTP identifies itself to the
 sender-SMTP in the connection greeting reply, and in the response to this command.
 This command and an OK reply to it confirm that both the sender-SMTP and the receiver-SMTP
 are in the initial state, that is, there is no transaction in progress and all state tables
 and buffers are cleared.}
Function TAlSmtpClient.Helo: AnsiString;
begin
  Result := SendCmd('HELO '+AlGetLocalHostName,[250]);
end;

{**************************************}
Function TAlSmtpClient.Ehlo: AnsiString;
begin
  result := SendCmd('EHLO '+AlGetLocalHostName,[250]);
  FAuthTypesSupported := GetAuthTypeFromEhloResponse(Result);
end;

{****************************************************************************}
{This command is used to initiate a mail transaction in which the mail data is
 delivered to one or more mailboxes. The argument field contains a reverse-path.
 The reverse-path consists of an optional list of hosts and the sender mailbox. When
 the list of hosts is present, it is a "reverse" source route and indicates that the
 mail was relayed through each host on the list (the first host in the list was the
 most recent relay). This list is used as a source route to return non-delivery notices
 to the sender. As each relay host adds itself to the beginning of the list, it must
 use its name as known in the IPCE to which it is relaying the mail rather than the IPCE
 from which the mail came (if they are different). In some types of error reporting
 messages (for example, undeliverable mail notifications) the reverse-path may be null.
 This command clears the reverse-path buffer, the forward-path buffer, and the mail data
 buffer; and inserts the reverse-path information from this command into the reverse-path buffer.}
Function TAlSmtpClient.MailFrom(const aSenderEmail: AnsiString): AnsiString;
begin
  If aSenderEmail = '' then raise EALException.Create('Sender email is empty');
  If AlPos(#13#10,aSenderEmail) > 0 then raise EALException.Create('Sender email is invalid');
  Result := SendCmd('MAIL From:<'+aSenderEmail+'>',[250]);
end;

{****************************************************************************************************************}
Function TAlSmtpClient.Auth(const AUserName, APassword: AnsiString; aAuthType: TalSmtpClientAuthType): AnsiString;

  {--------------------------------}
  Function _DoAuthPlain: AnsiString;
  var aAuthPlain : AnsiString;
  begin
    If aUserName='' then raise EALException.Create('UserName is empty');
    If aPassword='' then raise EALException.Create('Password is empty');
    aAuthPlain := ALMimeEncodeStringNoCRLF(aUserName + #0 + aUserName + #0 + aPassword);
    Result := SendCmd('AUTH PLAIN ' + aAuthPlain,[235]);
  end;

  {--------------------------------}
  Function _DoAuthLogin: AnsiString;
  begin
    If aUserName='' then raise EALException.Create('UserName is empty');
    If aPassword='' then raise EALException.Create('Password is empty');
    SendCmd('AUTH LOGIN',[334]);
    SendCmd(ALMimeEncodeStringNoCRLF(aUsername),[334]);
    Result := SendCmd(ALMimeEncodeStringNoCRLF(aPassword),[235]);
  end;

var tmpAuthType: TAlSmtpClientAuthType;

begin

  if aAuthType = AlsmtpClientAuthAutoSelect then begin
    if AlsmtpClientAuthPlain in FAuthTypesSupported then tmpAuthType := AlsmtpClientAuthPlain
    else if AlsmtpClientAuthLogin in FAuthTypesSupported then tmpAuthType := AlsmtpClientAuthLogin
    else if AlsmtpClientAuthCramMD5 in FAuthTypesSupported then tmpAuthType := AlsmtpClientAuthCramMD5
    else if AlsmtpClientAuthCramSHA1 in FAuthTypesSupported then tmpAuthType := AlsmtpClientAuthCramSHA1
    else tmpAuthType := AlsmtpClientAuthNone
  end
  else tmpAuthType := aAuthType;

  case tmpAuthType of
    alsmtpClientAuthPlain : Result := _DoAuthPlain;
    alsmtpClientAuthLogin : result := _DoAuthLogin;
    alsmtpClientAuthCramMD5 : raise EALException.Create('CRAM-MD5 Authentication is not supported yet!');
    alsmtpClientAuthCramSHA1: raise EALException.Create('CRAM-SHA1 Authentication is not supported yet!');
    else raise EALException.Create('No Authentication scheme found');
  end;

end;

{*************************************************************************}
{This command is used to identify an individual recipient of the mail data;
 multiple recipients are specified by multiple use of this command.}
Function TAlSmtpClient.RcptTo(aRcptNameLst: TALStrings): AnsiString;
Var i: integer;
    aRcptNameValue: AnsiString;
begin
  Result := '';
  if aRcptNameLst.Count <= 0 then raise EALException.Create('RcptName list is empty');
  For i := 0 to aRcptNameLst.Count - 1 do begin
    aRcptNameValue := ALTrim(aRcptNameLst[i]);
    If (aRcptNameValue = '') or (AlPos(#13#10,aRcptNameValue) > 0) then raise EALException.Create('Bad entry in RcptName list');
    Result := Result + SendCmd('RCPT To:<'+aRcptNameValue+'>',[250, 251]) + #13#10;
  end;
  If result <> '' then delete(Result,Length(Result)-1,2);
end;

{********************************************************************************}
{The receiver treats the lines following the command as mail data from the sender.
 This command causes the mail data from this command to be appended to the mail data buffer.
 The mail data may contain any of the 128 ASCII character codes.
 The mail data is terminated by a line containing only a period, that is the character sequence "<CRLF>.<CRLF>".
 This is the end of mail data indication. The end of mail data indication requires that the receiver must now process
 the stored mail transaction information. This processing consumes the information in the reverse-path buffer,
 the forward-path buffer, and the mail data buffer, and on the completion of this command these buffers are cleared.
 If the processing is successful the receiver must send an OK reply. If the processing fails completely
 the receiver must send a failure reply. When the receiver-SMTP accepts a message either for relaying or for
 final delivery it inserts at the beginning of the mail data a time stamp line. The time stamp line indicates the
 identity of the host that sent the message, and the identity of the host that received the message (and is inserting this
 time stamp), and the date and time the message was received. Relayed messages will have multiple time stamp lines.
 When the receiver-SMTP makes the "final delivery" of a message it inserts at the beginning of the mail data a return path
 line. The return path line preserves the information in the <reverse-path> from the MAIL command. Here, final delivery
 means the message leaves the SMTP world. Normally, this would mean it has been delivered to the destination user, but
 in some cases it may be further processed and transmitted by another mail system.
 It is possible for the mailbox in the return path be different from the actual sender's mailbox, for example,
 if error responses are to be delivered a special error handling mailbox rather than the message senders.
 The preceding two paragraphs imply that the final mail data will begin with a return path line, followed
 by one or more time stamp lines. These lines will be followed by the mail data header and body [2].
 Special mention is needed of the response and further action required when the processing following the end of mail
 data indication is partially successful. This could arise if after accepting several recipients and the mail data,
 the receiver-SMTP finds that the mail data can be successfully delivered to some of the recipients, but it cannot
 be to others (for example, due to mailbox space allocation problems). In such a situation, the response to the DATA
 command must be an OK reply. But, the receiver-SMTP must compose and send an "undeliverable mail" notification
 message to the originator of the message. Either a single notification which lists all of the recipients that failed
 to get the message, or separate notification messages must be sent for each failed recipient. All undeliverable mail
 notification messages are sent using the MAIL command (even if they result from processing a SEND, SOML, or SAML command).}
Function TAlSmtpClient.Data(const aMailData: AnsiString): AnsiString;
Var aTmpMailData: AnsiString;
    I : Integer;
begin
  SendCmd('DATA',[354]);

  i := 2;
  aTmpMailData := aMailData;
  while i <= Length(aTmpMailData) Do begin
    If (aTmpMailData[i] = '.') and (aTmpMailData[i-1] = #10) and (aTmpMailData[i-2] = #13) then Insert('.',aTmpMailData,i);
    inc(i);
  end;

  Result := SendCmd(aTmpMailData + #13#10'.'#13#10,[250]);
end;

{************************************************************************}
Function TAlSmtpClient.Data(const aHeader, aBody: AnsiString): AnsiString;
begin
  result := Data(ALTrim(aHeader) + #13#10#13#10 + aBody);
end;

{***************************************************************************************}
Function TAlSmtpClient.Data(aHeader:TALEmailHeader; const aBody: AnsiString): AnsiString;
begin
  result := Data(aHeader.RawHeaderText, aBody);
end;

{****************************************************************}
Function TAlSmtpClient.DataMultipartMixed(aHeader: TALEmailHeader;
                                          const aInlineText, aInlineTextContentType: AnsiString;
                                          aAttachments: TALMultiPartMixedContents): AnsiString;
Var aMultipartMixedEncoder: TALMultipartMixedEncoder;
    Str: AnsiString;
begin
  aMultipartMixedEncoder := TALMultipartMixedEncoder.create;
  try
    aMultipartMixedEncoder.Encode(aInlineText,
                                  aInlineTextContentType,
                                  aAttachments);
    with aMultipartMixedEncoder do begin
      aHeader.ContentType := 'multipart/mixed; boundary="' + DataStream.Boundary + '"';
      SetLength(Str,DataStream.size);
      DataStream.Position := 0;
      DataStream.ReadBuffer(pointer(str)^,DataStream.Size);
    end;
    Result := Data(aHeader.RawHeaderText, Str);
  finally
    aMultipartMixedEncoder.free;
  end;
end;

{**************************************************************}
{This command specifies that the receiver must send an OK reply,
 and then close the transmission channel. The receiver should not
 close the transmission channel until it receives and replies to
 a QUIT command (even if there was an error). The sender should not
 close the transmission channel until it send a QUIT command and
 receives the reply (even if there was an error response to a previous
 command). If the connection is closed prematurely the receiver should
 act as if a RSET command had been received (canceling any pending
 transaction, but not undoing any previously completed transaction),
 the sender should act as if the command or transaction in progress had
 received a temporary error (4xx).}
Function TAlSmtpClient.Quit: AnsiString;
begin
  Result := SendCmd('QUIT',[221]);
  Disconnect;
end;

{*****************************************************************************}
{This command asks the receiver to confirm that the argument identifies a user.
 If it is a user name, the full name of the user (if known) and the fully
 specified mailbox are returned. This command has no effect on any of the
 reverse-path buffer, the forward-path buffer, or the mail data buffer.}
Function TAlSmtpClient.Vrfy(const aUserName: AnsiString): AnsiString;
begin
  Result := SendCmd('VRFY ' + aUserName,[250]);
end;

{*************************************************************}
{This command specifies that the current mail transaction is to
 be aborted. Any stored sender, recipients, and mail data must be
 discarded, and all buffers and state tables cleared. The receiver
 must send an OK reply.}
Function TAlSmtpClient.Rset: AnsiString;
begin
  Result := SendCmd('RSET',[250]);
end;

{*******************************************************}
procedure TAlSmtpClient.SendMail(const aHost: AnsiString;
                                 APort: integer;
                                 const aSenderEmail: AnsiString;
                                 aRcptNameLst: TALStrings;
                                 const AUserName, APassword: AnsiString;
                                 aAuthType: TalSmtpClientAuthType;
                                 const aMailData: AnsiString);
begin
  If Fconnected then Disconnect;

  connect(aHost,APort);
  Try

    If aAuthType = AlsmtpClientAuthAutoSelect then ehlo
    else Helo;
    If aAuthType <> AlsmtpClientAuthNone then Auth(AUserName, APassword, aAuthType);
    mailFrom(aSenderEmail);
    RcptTo(aRcptNameLst);
    Data(aMailData);
    Quit;

  Finally
    Disconnect;
  end;
end;

{*******************************************************}
procedure TAlSmtpClient.SendMail(const aHost: AnsiString;
                                 APort: integer;
                                 const aSenderEmail: AnsiString;
                                 aRcptNameLst: TALStrings;
                                 const AUserName, APassword: AnsiString;
                                 aAuthType: TalSmtpClientAuthType;
                                 const aHeader, aBody: AnsiString);
begin
  If Fconnected then Disconnect;

  connect(aHost,APort);
  Try

    If aAuthType = AlsmtpClientAuthAutoSelect then ehlo
    else Helo;
    If aAuthType <> AlsmtpClientAuthNone then Auth(AUserName, APassword, aAuthType);
    mailFrom(aSenderEmail);
    RcptTo(aRcptNameLst);
    Data(aHeader, aBody);
    Quit;

  Finally
    Disconnect;
  end;
end;

{*********************************************************************}
procedure TAlSmtpClient.SendMailMultipartMixed(const aHost: AnsiString;
                                               APort: integer;
                                               const aSenderEmail: AnsiString;
                                               aRcptNameLst: TALStrings;
                                               const AUserName, APassword: AnsiString;
                                               aAuthType: TalSmtpClientAuthType;
                                               aHeader: TALEmailHeader;
                                               const aInlineText, aInlineTextContentType: AnsiString;
                                               aAttachments: TALMultiPartMixedContents);
begin
  If Fconnected then Disconnect;
  connect(aHost,APort);
  Try

    If aAuthType = AlsmtpClientAuthAutoSelect then ehlo
    else Helo;
    If aAuthType <> AlsmtpClientAuthNone then Auth(AUserName, APassword, aAuthType);
    mailFrom(aSenderEmail);
    RcptTo(aRcptNameLst);
    DataMultipartMixed(aHeader,
                       aInlineText,
                       aInlineTextContentType,
                       aAttachments);
    Quit;

  Finally
    Disconnect;
  end;
end;

{******************************************************************************}
{commands consist of a command code followed by an argument field. Command codes
 are four alphabetic characters. Upper and lower case alphabetic characters are
 to be treated identically. Thus, any of the following may represent the mail command:
            MAIL    Mail    mail    MaIl    mAIl
 This also applies to any symbols representing parameter values, such as "TO" or "to"
 for the forward-path. Command codes and the argument fields are separated by one or
 more spaces. However, within the reverse-path and forward-path arguments case is
 important. In particular, in some hosts the user "smith" is different from the user
 "Smith". The argument field consists of a variable length character string ending
 with the character sequence <CRLF>. The receiver is to take no action until
 this sequence is received. Square brackets denote an optional argument field.
 If the option is not taken, the appropriate default is implied.
 The following are the SMTP commands:
            HELO <SP> <domain> <CRLF>
            MAIL <SP> FROM:<reverse-path> <CRLF>
            RCPT <SP> TO:<forward-path> <CRLF>
            DATA <CRLF>
            RSET <CRLF>
            SEND <SP> FROM:<reverse-path> <CRLF>
            SOML <SP> FROM:<reverse-path> <CRLF>
            SAML <SP> FROM:<reverse-path> <CRLF>
            VRFY <SP> <string> <CRLF>
            EXPN <SP> <string> <CRLF>
            HELP [<SP> <string>] <CRLF>
            NOOP <CRLF>
            QUIT <CRLF>
            TURN <CRLF>}
function TAlSmtpClient.SendCmd(const aCmd: AnsiString; const OkResponses: array of Word): AnsiString;
Var P: PAnsiChar;
    L: Integer;
    ByteSent: integer;
    TmpCmd: AnsiString;
begin

  If (length(aCmd) <= 1) or
     (aCmd[length(aCmd)] <> #10) or
     (aCmd[length(aCmd) - 1] <> #13)
  then TmpCmd := aCmd + #13#10
  else TmpCmd := aCmd;

  p:=@TmpCmd[1]; // pchar
  l:=length(TmpCmd);
  while l>0 do begin
    ByteSent:=SocketWrite(p^,l);
    if ByteSent<=0 then raise EALException.Create('Connection close gracefully!');
    inc(p,ByteSent);
    dec(l,ByteSent);
  end;

  Result := GetResponse(OkResponses);
end;

{*******************************************************************}
{An SMTP reply consists of a three digit number (transmitted as three
 alphanumeric characters) followed by some text. The number is intended
 for use by automata to determine what state to enter next; the text is
 meant for the human user. It is intended that the three digits contain
 enough encoded information that the sender-SMTP need not examine the
 text and may either discard it or pass it on to the user, as appropriate.
 In particular, the text may be receiver-dependent and context dependent,
 so there are likely to be varying texts for each reply code. Formally,
 a reply is defined to be the sequence:
 a three-digit code, <SP>, one line of text, and <CRLF>, or a multiline reply.
 Only the EXPN and HELP commands are expected to result in multiline replies
 in normal circumstances, however multiline replies are allowed for any
 command.}
function TAlSmtpClient.GetResponse(const OkResponses: array of Word): AnsiString;

  {-----------------------------------------------}
  function _stpblk(PValue : PAnsiChar) : PAnsiChar;
  begin
    Result := PValue;
    while Result^ in [' ', #9, #10, #13] do Inc(Result);
  end;

  {----------------------------------------------------------------------}
  function _GetInteger(Data: PAnsiChar; var Number : Integer) : PAnsiChar;
  var bSign : Boolean;
  begin
    Number := 0;
    Result := _StpBlk(Data);
    if (Result = nil) then Exit;
    { Remember the sign }
    if Result^ in ['-', '+'] then begin
      bSign := (Result^ = '-');
      Inc(Result);
    end
    else bSign  := FALSE;
    { Convert any number }
    while (Result^ <> #0) and (Result^ in ['0'..'9']) do begin
      Number := Number * 10 + ord(Result^) - ord('0');
      Inc(Result);
    end;
    { Correct for sign }
    if bSign then Number := -Number;
  end;

Var aBuffStr: AnsiString;
    aBuffStrLength: Integer;
    aResponseLength: Integer;
    aResponse: AnsiString;
    aStatusCode: Integer;
    aGoodResponse: Boolean;
    ALst : TALStringList;
    P: PAnsiChar;
    i, j: integer;

begin
  Result := '';
  Setlength(aBuffStr,512); //The maximum total length of a reply line including the reply code and the <CRLF> is 512 characters. (http://www.freesoft.org/CIE/RFC/821/24.htm)
  While true do begin

    {Read the response from the socket - end of the response is show by <CRLF>}
    aResponse := '';
    While True do begin
      aBuffStrLength := SocketRead(pointer(aBuffStr)^, length(aBuffStr));
      If aBuffStrLength <= 0 then raise EALException.Create('Connection close gracefully!');
      aResponse := AResponse + AlCopyStr(aBuffStr,1,aBuffStrLength);
      aResponseLength := length(aResponse);
      If (aResponseLength > 1) and
         (aResponse[aResponseLength] = #10) and
         (aResponse[aResponseLength - 1] = #13) then Break;
    end;
    Result := Result + aResponse;

    {The format for multiline replies requires that every line, except the last,
     begin with the reply code, followed immediately by a hyphen, "-" (also known as minus),
     followed by text. The last line will begin with the reply code, followed immediately
     by <SP>, optionally some text, and <CRLF>.}
    ALst := TALStringList.create;
    Try
      Alst.Text := aResponse;
      If Alst.count = 0 then raise EALException.Create('Emtpy response');
      For j := 0 to Alst.count - 1 do begin

        aResponse := Alst[j];
        p := _GetInteger(@aResponse[1], aStatusCode);
        aGoodResponse := False;
        for I := 0 to High(OkResponses) do
          if OkResponses[I] = aStatusCode then begin
            aGoodResponse := True;
            Break;
          end;

        If not aGoodResponse then Raise EALException.Create(aResponse);
        if p^ <> '-' then Begin
          If J <> Alst.count - 1 then Raise EALException.Create(aResponse);
          Exit;
        end;

      end;
    Finally
      ALst.Free;
    end;

  end;
end;

{*******************************************************************}
Function TAlSmtpClient.SocketWrite(const Buf; len: Integer): Integer;
begin
  Result := Send(FSocketDescriptor,Buf,len,0);
  CheckError(Result =  SOCKET_ERROR);
end;

{****************************************************************}
function TAlSmtpClient.SocketRead(var buf; len: Integer): Integer;
begin
  Result := Recv(FSocketDescriptor,Buf,len,0);
  CheckError(Result = SOCKET_ERROR);
end;

{***********************************************************}
procedure TAlSmtpClient.SetSendTimeout(const Value: integer);
begin
  FSendTimeout := Value;
  if FConnected then CheckError(setsockopt(FSocketDescriptor,SOL_SOCKET,SO_SNDTIMEO,PAnsiChar(@FSendTimeout),SizeOf(FSendTimeout))=SOCKET_ERROR);
end;

{**************************************************************}
procedure TAlSmtpClient.SetReceiveTimeout(const Value: integer);
begin
  FReceiveTimeout := Value;
  if FConnected then CheckError(setsockopt(FSocketDescriptor,SOL_SOCKET,SO_RCVTIMEO,PAnsiChar(@FReceiveTimeout),SizeOf(FReceiveTimeout))=SOCKET_ERROR);
end;

{*******************************************************************************************************************}
// http://blogs.technet.com/b/nettracer/archive/2010/06/03/things-that-you-may-want-to-know-about-tcp-keepalives.aspx
procedure TAlSmtpClient.SetKeepAlive(const Value: boolean);
var aIntBool: integer;
begin
  FKeepAlive := Value;
  if FConnected then begin
    // warning the winsock seam buggy because the getSockOpt return optlen = 1 (byte) intead of 4 (dword)
    // so the getSockOpt work only if aIntBool = byte ! (i see this on windows vista)
    // but this is only for getSockOpt, for setsockopt it's seam to work OK so i leave it like this
    if FKeepAlive then aIntBool := 1
    else aIntBool := 0;
    CheckError(setsockopt(FSocketDescriptor,SOL_SOCKET,SO_KEEPALIVE,PAnsiChar(@aIntBool),SizeOf(aIntBool))=SOCKET_ERROR);
  end;
end;

{***************************************************************************************************************************************************************************************************************}
// https://access.redhat.com/site/documentation/en-US/Red_Hat_Enterprise_MRG/1.1/html/Realtime_Tuning_Guide/sect-Realtime_Tuning_Guide-Application_Tuning_and_Deployment-TCP_NODELAY_and_Small_Buffer_Writes.html
procedure TAlSmtpClient.SetTCPNoDelay(const Value: boolean);
var aIntBool: integer;
begin
  fTCPNoDelay := Value;
  if FConnected then begin
    // warning the winsock seam buggy because the getSockOpt return optlen = 1 (byte) intead of 4 (dword)
    // so the getSockOpt work only if aIntBool = byte ! (i see this on windows vista)
    // but this is only for getSockOpt, for setsockopt it's seam to work OK so i leave it like this
    if fTCPNoDelay then aIntBool := 1
    else aIntBool := 0;
    CheckError(setsockopt(FSocketDescriptor,SOL_SOCKET,TCP_NODELAY,PAnsiChar(@aIntBool),SizeOf(aIntBool))=SOCKET_ERROR);
  end;
end;

end.
