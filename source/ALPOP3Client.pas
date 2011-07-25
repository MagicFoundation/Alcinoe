{*************************************************************
www:          http://sourceforge.net/projects/alcinoe/              
svn:          https://alcinoe.svn.sourceforge.net/svnroot/alcinoe              
Author(s):    Stéphane Vander Clock (svanderclock@arkadia.com)
              Igor (Igor@arkadia.com)
Sponsor(s):   Arkadia SA (http://www.arkadia.com)

product:      ALPOP3Client
Version:      3.52

Description:  TALPOP3Client class implements the POP3 protocol (Post Office
              Protocol - Version 3)

Legal issues: Copyright (C) 1999-2010 by Arkadia Software Engineering

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

Link :        http://www.ietf.org/rfc/rfc1939.txt

* Please send all your feedback to svanderclock@arkadia.com
* If you have downloaded this source from a website different from 
  sourceforge.net, please get the last version on http://sourceforge.net/projects/alcinoe/
* Please, help us to keep the development of these components free by 
  voting on http://www.arkadia.com/html/alcinoe_like.html
**************************************************************}
unit ALPOP3Client;

interface

uses windows,
     Classes,
     WinSock,
     ALInternetMessageCommon;

type

    {----------------------------}
    TAlPOP3Client = class(TObject)
    Private
      FWSAData : TWSAData;
      Fconnected: Boolean;
      FSocketDescriptor: Integer;
      Ftimeout: integer;
      procedure Settimeout(const Value: integer);
    protected
      procedure CheckError(Error: Boolean);
      Function SendCmd(aCmd:String; const MultilineResponse: Boolean=False): String; virtual;
      Function GetResponse(const MultilineResponse: Boolean=False): String;
      Function SocketWrite(Var Buffer; Count: Longint): Longint; Virtual;
      Function SocketRead(var Buffer; Count: Longint): Longint; Virtual;
    public
      constructor Create; virtual;
      destructor Destroy; override;
      Function  Connect(aHost: String; APort: integer): String; virtual;
      Procedure Disconnect; virtual;
      function  User(UserName: String): String; virtual;
      function  Pass(Password: String): String; virtual;
      Function  List: String; overload; virtual;
      procedure List(ALst: Tstrings); overload; virtual;
      Function  List(aMsgNumber: integer): String; overload; virtual;
      Function  Uidl: String; overload; virtual;
      procedure Uidl(ALst: Tstrings); overload; virtual;
      Function  Uidl(aMsgNumber: Integer): String; overload; virtual;
      Procedure Uidl(aMsgNumber: Integer; Var aUniqueIDListing: String); overload; virtual;
      Function  Quit: String; virtual;
      Function  Rset: String; virtual;
      Function  Stat: String; overload; virtual;
      Procedure Stat(Var ANumberofMsgInthemaildrop: Integer; Var aSizeofthemaildrop: integer); overload; virtual;
      Function  Retr(aMsgNumber: Integer): String; overload; virtual;
      procedure Retr(aMsgNumber: Integer; var aMsgBodyContent: String; aMsgHeaderContent: TALEMailHeader); overload; virtual;
      Function  Top(aMsgNumber: Integer; aNumberOfLines: integer): String; virtual;
      Function  Dele(aMsgNumber: Integer): String; virtual;
      Function  Noop: String; virtual;
      property  Connected: Boolean read FConnected;
      Property  Timeout: integer read Ftimeout write Settimeout default 60000;
    end;

implementation

Uses SysUtils,
     AlFcnWinsock,
     AlFcnString;

{*************************************************************************}
Procedure ALPOP3ClientSplitResponseLine(aResponse: String; ALst: Tstrings);
Begin
  aResponse := Trim(aResponse); // +OK 2 320
  aResponse := AlStringReplace(aResponse,#9,' ',[RfReplaceAll]); // +OK 2 320
  While alPos('  ',aResponse) > 0 do aResponse := AlStringReplace(aResponse,'  ',' ',[RfReplaceAll]); // +OK 2 320
  aResponse := trim(AlStringReplace(aResponse,' ',#13#10,[RfReplaceAll]));  // +OK
                                                                            // 2
                                                                            // 320
  aLst.Text := aResponse;
end;

{********************************************************************}
{ Responses to certain commands are multi-line.  In these cases, which
   are clearly indicated below, after sending the first line of the
   response and a CRLF, any additional lines are sent, each terminated
   by a CRLF pair.  When all lines of the response have been sent, a
   final line is sent, consisting of a termination octet (decimal code
   046, ".") and a CRLF pair.  If any line of the multi-line response
   begins with the termination octet, the line is "byte-stuffed" by
   pre-pending the termination octet to that line of the response.
   Hence a multi-line response is terminated with the five octets
   "CRLF.CRLF".  When examining a multi-line response, the client checks
   to see if the line begins with the termination octet.  If so and if
   octets other than CRLF follow, the first octet of the line (the
   termination octet) is stripped away.  If so and if CRLF immediately
   follows the termination character, then the response from the POP
   server is ended and the line containing ".CRLF" is not considered
   part of the multi-line response.}
Function ALPOP3ClientExtractTextFromMultilineResponse(aMultilineResponse: String): String;
Var ln: Integer;
    P: Integer;
begin
  Result := aMultilineResponse;
  ln := length(aMultilineResponse);
  If (ln > 4) and
     (Result[ln] = #10) and
     (Result[ln - 1] = #13) and
     (Result[ln - 2] = '.') and
     (Result[ln - 3] = #10) and
     (Result[ln - 4] = #13) then delete(Result,LN-4,5);
  P := AlPos(#13#10,Result);
  If P > 0 then delete(Result,1,P+1)
  else result := '';

  {If any line of the multi-line response
   begins with the termination octet, the line is "byte-stuffed" by
   pre-pending the termination octet to that line of the response.}
  Result := AlStringReplace(Result,#13#10'..',#13#10'.', [RfReplaceAll]);
end;


///////////////////////////////////
////////// TAlPOP3Client //////////
///////////////////////////////////

{*******************************}
constructor TAlPOP3Client.Create;
begin
  FWSAData.wVersion := 0;
  Fconnected:= False;
  FSocketDescriptor:= INVALID_SOCKET;
  Ftimeout:= 60000;
  Randomize;
end;

{*******************************}
destructor TAlPOP3Client.Destroy;
begin
  If Fconnected then Disconnect;
  inherited;
end;

{*************************************************}
procedure TAlPOP3Client.CheckError(Error: Boolean);
begin
  if Error then RaiseLastOSError;
end;

{********************************************************************}
Function TAlPOP3Client.Connect(aHost: String; APort: integer): String;

  {---------------------------------------------}
  procedure CallServer(Server:string; Port:word);
  var SockAddr:Sockaddr_in;
      IP: String;
  begin
    FSocketDescriptor:=Socket(AF_INET,SOCK_STREAM,IPPROTO_IP);
    CheckError(FSocketDescriptor=INVALID_SOCKET);
    FillChar(SockAddr,SizeOf(SockAddr),0);
    SockAddr.sin_family:=AF_INET;
    SockAddr.sin_port:=swap(Port);
    SockAddr.sin_addr.S_addr:=inet_addr(Pchar(Server));
    If SockAddr.sin_addr.S_addr = INADDR_NONE then begin
      checkError(not ALHostToIP(Server, IP));
      SockAddr.sin_addr.S_addr:=inet_addr(Pchar(IP));
    end;
    CheckError(WinSock.Connect(FSocketDescriptor,SockAddr,SizeOf(SockAddr))=SOCKET_ERROR);
  end;

begin
  if FConnected then raise Exception.Create('Already connected');

  Try

    WSAStartup (MAKEWORD(2,2), FWSAData);
    CallServer(aHost,aPort);
    CheckError(setsockopt(FSocketDescriptor,SOL_SOCKET,SO_RCVTIMEO,PChar(@FTimeOut),SizeOf(Integer))=SOCKET_ERROR);
    CheckError(setsockopt(FSocketDescriptor,SOL_SOCKET,SO_SNDTIMEO,PChar(@FTimeOut),SizeOf(Integer))=SOCKET_ERROR);
    Fconnected := True;
    Result := GetResponse();

  Except
    Disconnect;
    raise;
  end;

end;

{*********************************}
procedure TAlPOP3Client.Disconnect;
begin
  If Fconnected then begin
    ShutDown(FSocketDescriptor,SD_BOTH);
    CloseSocket(FSocketDescriptor);
    FSocketDescriptor := INVALID_SOCKET;
    if FWSAData.wVersion = 2 then WSACleanup;
    FWSAData.wVersion := 0;
    Fconnected := False;
  end;
end;

{*********}
{USER name

 Arguments:
     a string identifying a mailbox (required), which is of
     significance ONLY to the server

 Restrictions:
     may only be given in the AUTHORIZATION state after the POP3
     greeting or after an unsuccessful USER or PASS command

 Discussion:
     To authenticate using the USER and PASS command
     combination, the client must first issue the USER
     command.  If the POP3 server responds with a positive
     status indicator ("+OK"), then the client may issue
     either the PASS command to complete the authentication,
     or the QUIT command to terminate the POP3 session.  If
     the POP3 server responds with a negative status indicator
     ("-ERR") to the USER command, then the client may either
     issue a new authentication command or may issue the QUIT
     command.

     The server may return a positive response even though no
     such mailbox exists.  The server may return a negative
     response if mailbox exists, but does not permit plaintext
     password authentication.

 Possible Responses:
     +OK name is a valid mailbox
     -ERR never heard of mailbox name

 Examples:
     C: USER frated
     S: -ERR sorry, no mailbox for frated here
        ...
     C: USER mrose
     S: +OK mrose is a real hoopy frood}
function TAlPOP3Client.User(UserName: String): String;
begin
  Result := SendCmd('USER ' + UserName, False);
end;

{**********}
{PASS string

 Arguments:
     a server/mailbox-specific password (required)

 Restrictions:
     may only be given in the AUTHORIZATION state immediately
     after a successful USER command

 Discussion:
     When the client issues the PASS command, the POP3 server
     uses the argument pair from the USER and PASS commands to
     determine if the client should be given access to the
     appropriate maildrop.

     Since the PASS command has exactly one argument, a POP3
     server may treat spaces in the argument as part of the
     password, instead of as argument separators.

 Possible Responses:
     +OK maildrop locked and ready
     -ERR invalid password
     -ERR unable to lock maildrop

 Examples:
     C: USER mrose
     S: +OK mrose is a real hoopy frood
     C: PASS secret
     S: -ERR maildrop already locked
       ...
     C: USER mrose
     S: +OK mrose is a real hoopy frood
     C: PASS secret
     S: +OK mrose's maildrop has 2 messages (320 octets)}
function TAlPOP3Client.Pass(Password: String): String;
begin
  Result := SendCmd('PASS ' + Password, False);
end;

{*********}
{LIST [msg]

 Arguments:
     a message-number (optional), which, if present, may NOT
     refer to a message marked as deleted

 Restrictions:
     may only be given in the TRANSACTION state

 Discussion:
     If an argument was given and the POP3 server issues a
     positive response with a line containing information for
     that message.  This line is called a "scan listing" for
     that message.

     If no argument was given and the POP3 server issues a
     positive response, then the response given is multi-line.
     After the initial +OK, for each message in the maildrop,
     the POP3 server responds with a line containing
     information for that message.  This line is also called a
     "scan listing" for that message.  If there are no
     messages in the maildrop, then the POP3 server responds
     with no scan listings--it issues a positive response
     followed by a line containing a termination octet and a
     CRLF pair.

     In order to simplify parsing, all POP3 servers are
     required to use a certain format for scan listings.  A
     scan listing consists of the message-number of the
     message, followed by a single space and the exact size of
     the message in octets.  Methods for calculating the exact
     size of the message are described in the "Message Format"
     section below.  This memo makes no requirement on what
     follows the message size in the scan listing.  Minimal
     implementations should just end that line of the response
     with a CRLF pair.  More advanced implementations may
     include other information, as parsed from the message.

        NOTE: This memo STRONGLY discourages implementations
        from supplying additional information in the scan
        listing.  Other, optional, facilities are discussed
        later on which permit the client to parse the messages
        in the maildrop.

     Note that messages marked as deleted are not listed.

 Possible Responses:
     +OK scan listing follows
     -ERR no such message

 Examples:
     C: LIST
     S: +OK 2 messages (320 octets)
     S: 1 120
     S: 2 200
     S: .
       ...
     C: LIST 2
     S: +OK 2 200
       ...
     C: LIST 3
     S: -ERR no such message, only 2 messages in maildrop}
Function TAlPOP3Client.List: String;
begin
  Result := SendCmd('LIST', True);
end;

{*******************************************}
procedure TAlPOP3Client.List(ALst: Tstrings);
begin
  ALst.Text := Trim(AlStringReplace(ALPOP3ClientExtractTextFromMultilineResponse(List), ' ', ALst.NameValueSeparator, [rfReplaceAll]));
end;

{*******************************************************}
Function TAlPOP3Client.List(aMsgNumber: integer): String;
begin
  Result := SendCmd('LIST ' + inttostr(aMsgNumber), False);
end;

{*********}
{UIDL [msg]

 Arguments:
    a message-number (optional), which, if present, may NOT
    refer to a message marked as deleted

 Restrictions:
    may only be given in the TRANSACTION state.

 Discussion:
    If an argument was given and the POP3 server issues a positive
    response with a line containing information for that message.
    This line is called a "unique-id listing" for that message.

    If no argument was given and the POP3 server issues a positive
    response, then the response given is multi-line.  After the
    initial +OK, for each message in the maildrop, the POP3 server
    responds with a line containing information for that message.
    This line is called a "unique-id listing" for that message.

    In order to simplify parsing, all POP3 servers are required to
    use a certain format for unique-id listings.  A unique-id
    listing consists of the message-number of the message,
    followed by a single space and the unique-id of the message.
    No information follows the unique-id in the unique-id listing.

    The unique-id of a message is an arbitrary server-determined
    string, consisting of one to 70 characters in the range 0x21
    to 0x7E, which uniquely identifies a message within a
    maildrop and which persists across sessions.  This
    persistence is required even if a session ends without
    entering the UPDATE state.  The server should never reuse an
    unique-id in a given maildrop, for as long as the entity
    using the unique-id exists.

    Note that messages marked as deleted are not listed.

    While it is generally preferable for server implementations
    to store arbitrarily assigned unique-ids in the maildrop,
    this specification is intended to permit unique-ids to be
    calculated as a hash of the message.  Clients should be able
    to handle a situation where two identical copies of a
    message in a maildrop have the same unique-id.

 Possible Responses:
    +OK unique-id listing follows
    -ERR no such message

 Examples:
    C: UIDL
    S: +OK
    S: 1 whqtswO00WBw418f9t5JxYwZ
    S: 2 QhdPYR:00WBw1Ph7x7
    S: .
       ...
    C: UIDL 2
    S: +OK 2 QhdPYR:00WBw1Ph7x7
       ...
    C: UIDL 3
    S: -ERR no such message, only 2 messages in maildrop}
Function TAlPOP3Client.UIDL: String;
begin
  Result := SendCmd('UIDL', True);
end;

{*******************************************}
procedure TAlPOP3Client.UIDL(ALst: Tstrings);
begin
  ALst.Text := Trim(AlStringReplace(ALPOP3ClientExtractTextFromMultilineResponse(Uidl), ' ', ALst.NameValueSeparator, [rfReplaceAll]));
end;

{*******************************************************}
Function TAlPOP3Client.UIDL(aMsgNumber: Integer): String;
begin
  Result := SendCmd('UIDL ' + inttostr(aMsgNumber), False);
end;

{******************************************************************************}
Procedure TAlPOP3Client.Uidl(aMsgNumber: Integer; Var aUniqueIDListing: String);
Var aLst: TstringList;
Begin
  aLst := TstringList.Create;
  Try
    ALPOP3ClientSplitResponseLine(UIDL(aMsgNumber), aLst);
    If aLst.Count < 3 then raise Exception.Create('UIDL cmd Error');
    aUniqueIDListing := aLst[2];
  finally
    aLst.Free;
  end;
end;

{***}
{STAT

 Arguments: none

 Restrictions:
     may only be given in the TRANSACTION state

 Discussion:
     The POP3 server issues a positive response with a line
     containing information for the maildrop.  This line is
     called a "drop listing" for that maildrop.

     In order to simplify parsing, all POP3 servers are
     required to use a certain format for drop listings.  The
     positive response consists of "+OK" followed by a single
     space, the number of messages in the maildrop, a single
     space, and the size of the maildrop in octets.  This memo
     makes no requirement on what follows the maildrop size.
     Minimal implementations should just end that line of the
     response with a CRLF pair.  More advanced implementations
     may include other information.

        NOTE: This memo STRONGLY discourages implementations
        from supplying additional information in the drop
        listing.  Other, optional, facilities are discussed
        later on which permit the client to parse the messages
        in the maildrop.

     Note that messages marked as deleted are not counted in
     either total.

 Possible Responses:
     +OK nn mm

 Examples:
     C: STAT
     S: +OK 2 320}
function TAlPOP3Client.Stat: String;
begin
  Result := SendCmd('STAT', False);
end;

{****************************************************************************************************}
procedure TAlPOP3Client.Stat(Var ANumberofMsgInthemaildrop: Integer; Var aSizeofthemaildrop: integer);
Var aLst: TstringList;
Begin
  aLst := TstringList.Create;
  Try
    ALPOP3ClientSplitResponseLine(Stat, aLst);
    If aLst.Count < 3 then raise Exception.Create('Stat cmd Error');
    If not TryStrToInt(aLst[1],ANumberofMsgInthemaildrop) then raise Exception.Create('Stat cmd Error');
    If not TryStrToInt(aLst[2],aSizeofthemaildrop) then raise Exception.Create('Stat cmd Error');
  finally
    aLst.Free;
  end;
end;

{********}
{RETR msg

 Arguments:
     a message-number (required) which may NOT refer to a
     message marked as deleted

 Restrictions:
     may only be given in the TRANSACTION state

 Discussion:
     If the POP3 server issues a positive response, then the
     response given is multi-line.  After the initial +OK, the
     POP3 server sends the message corresponding to the given
     message-number, being careful to byte-stuff the termination
     character (as with all multi-line responses).

 Possible Responses:
     +OK message follows
     -ERR no such message

 Examples:
     C: RETR 1
     S: +OK 120 octets
     S: <the POP3 server sends the entire message here>
     S: .}
Function TAlPOP3Client.Retr(aMsgNumber: Integer): String;
begin
  Result := SendCmd('RETR ' + inttostr(aMsgNumber), True);
end;

{****************************************************************************************************************}
procedure TAlPOP3Client.Retr(aMsgNumber: Integer; var aMsgBodyContent: String; aMsgHeaderContent: TALEMailHeader);
Var P: integer;
begin
  aMsgBodyContent := ALPop3ClientExtractTextFromMultilineResponse(Retr(aMsgNumber));
  P := AlPos(#13#10#13#10,aMsgBodyContent);
  If P > 0 then begin
    aMsgHeaderContent.RawHeaderText := AlcopyStr(aMsgBodyContent,1,P);
    Delete(aMsgBodyContent,1,P+3);
  end
  else begin
    aMsgHeaderContent.RawHeaderText := aMsgBodyContent;
    aMsgBodyContent := '';
  end;
end;

{********}
{TOP msg n

 Arguments:
     a message-number (required) which may NOT refer to to a
     message marked as deleted, and a non-negative number
     of lines (required)

 Restrictions:
     may only be given in the TRANSACTION state

 Discussion:
     If the POP3 server issues a positive response, then the
     response given is multi-line.  After the initial +OK, the
     POP3 server sends the headers of the message, the blank
     line separating the headers from the body, and then the
     number of lines of the indicated message's body, being
     careful to byte-stuff the termination character (as with
     all multi-line responses).

     Note that if the number of lines requested by the POP3
     client is greater than than the number of lines in the
     body, then the POP3 server sends the entire message.

 Possible Responses:
     +OK top of message follows
     -ERR no such message

 Examples:
     C: TOP 1 10
     S: +OK
     S: <the POP3 server sends the headers of the
        message, a blank line, and the first 10 lines
        of the body of the message>
     S: .
        ...
     C: TOP 100 3
     S: -ERR no such message}
Function TAlPOP3Client.Top(aMsgNumber: Integer; aNumberOfLines: integer): String;
begin
  Result := SendCmd('TOP ' + inttostr(aMsgNumber) + ' ' + inttostr(aNumberOfLines), True);
end;

{***}
{NOOP

 Arguments: none

 Restrictions:
     may only be given in the TRANSACTION state

 Discussion:
     The POP3 server does nothing, it merely replies with a
     positive response.

 Possible Responses:
     +OK

 Examples:
     C: NOOP
     S: +OK}
Function TAlPOP3Client.Noop: String;
begin
  Result := SendCmd('NOOP', False);
end;

{********}
{DELE msg

 Arguments:
     a message-number (required) which may NOT refer to a
     message marked as deleted

 Restrictions:
     may only be given in the TRANSACTION state

 Discussion:
     The POP3 server marks the message as deleted.  Any future
     reference to the message-number associated with the message
     in a POP3 command generates an error.  The POP3 server does
     not actually delete the message until the POP3 session
     enters the UPDATE state.

 Possible Responses:
     +OK message deleted
     -ERR no such message

 Examples:
     C: DELE 1
     S: +OK message 1 deleted
        ...
     C: DELE 2
     S: -ERR message 2 already deleted}
Function TAlPOP3Client.Dele(aMsgNumber: Integer): String;
begin
  Result := SendCmd('DELE ' + inttostr(aMsgNumber), False);
end;

{***}
{QUIT

 Arguments: none

 Restrictions: none

 Discussion:
     The POP3 server removes all messages marked as deleted
     from the maildrop and replies as to the status of this
     operation.  If there is an error, such as a resource
     shortage, encountered while removing messages, the
     maildrop may result in having some or none of the messages
     marked as deleted be removed.  In no case may the server
     remove any messages not marked as deleted.

     Whether the removal was successful or not, the server
     then releases any exclusive-access lock on the maildrop
     and closes the TCP connection.

 Possible Responses:
     +OK
     -ERR some deleted messages not removed

 Examples:
     C: QUIT
     S: +OK dewey POP3 server signing off (maildrop empty)
        ...
     C: QUIT
     S: +OK dewey POP3 server signing off (2 messages left)}
function TAlPOP3Client.Quit: String;
begin
  Result := SendCmd('QUIT', False);
  Disconnect;
end;

{***}
{RSET

 Arguments: none

 Restrictions:
     may only be given in the TRANSACTION state

 Discussion:
     If any messages have been marked as deleted by the POP3
     server, they are unmarked.  The POP3 server then replies
     with a positive response.

 Possible Responses:
     +OK

 Examples:
     C: RSET
     S: +OK maildrop has 2 messages (320 octets)}
function TAlPOP3Client.Rset: String;
begin
  Result := SendCmd('RSET', False);
end;

{**************************************************************}
{Commands consist of a command word, which in some cases may be
 followed by a parameter.  Commands with parameters must separate the
 parameters from each other and from the command by one or more space
 or tab characters.  Command lines must be complete with all required
 parameters, and may not contain more than one command.

 Commands and command parameters are not case sensitive. That is, a
 command or parameter word may be upper case, lower case, or any
 mixture of upper and lower case.

 Each command line must be terminated by a CR-LF (Carriage Return -
 Line Feed) pair.

 Command lines shall not exceed 512 characters in length, counting all
 characters including spaces, separators, punctuation, and the
 trailing CR-LF (thus there are 510 characters maximum allowed for the
 command and its parameters).  There is no provision for continuation
 command lines.}
function TAlPOP3Client.SendCmd(aCmd: String;
                               Const MultilineResponse: Boolean=False): String;
Var P: Pchar;
    L: Integer;
    ByteSent: integer;
begin
  If (length(aCmd) <= 1) or
     (aCmd[length(aCmd)] <> #10) or
     (aCmd[length(aCmd) - 1] <> #13)
  then aCmd := aCmd + #13#10;

  p:=@aCmd[1]; // pchar
  l:=length(aCmd);
  while l>0 do begin
    ByteSent:=SocketWrite(p^,l);
    if ByteSent<=0 then raise Exception.Create('Connection close gracefully!');
    inc(p,ByteSent);
    dec(l,ByteSent);
  end;

  Result := GetResponse(MultilineResponse);
end;

{****************************************************************}
{Responses in the POP3 consist of a status indicator and a keyword
 possibly followed by additional information.  All responses are
 terminated by a CRLF pair.  Responses may be up to 512 characters
 long, including the terminating CRLF.  There are currently two status
 indicators: positive ("+OK") and negative ("-ERR").  Servers MUST
 send the "+OK" and "-ERR" in upper case.

 Responses to certain commands are multi-line.  In these cases, which
 are clearly indicated below, after sending the first line of the
 response and a CRLF, any additional lines are sent, each terminated
 by a CRLF pair.  When all lines of the response have been sent, a
 final line is sent, consisting of a termination octet (decimal code
 046, ".") and a CRLF pair.  If any line of the multi-line response
 begins with the termination octet, the line is "byte-stuffed" by
 pre-pending the termination octet to that line of the response.
 Hence a multi-line response is terminated with the five octets
 "CRLF.CRLF".  When examining a multi-line response, the client checks
 to see if the line begins with the termination octet.  If so and if
 octets other than CRLF follow, the first octet of the line (the
 termination octet) is stripped away.  If so and if CRLF immediately
 follows the termination character, then the response from the POP
 server is ended and the line containing ".CRLF" is not considered
 part of the multi-line response.}
function TAlPOP3Client.GetResponse(Const MultilineResponse: Boolean=False): String;
Var aBuffStr: String;
    aBuffStrLength: Integer;
    aResponseLength: Integer;
    aGoodResponse: integer;
begin
  {Read the response from the socket - end of the Multiline response is show by <CRLF>.<CRLF> and
   end of single line response is show by <CRLF>}
  Result := '';
  Setlength(aBuffStr,512);
  aGoodResponse := -1;
  While True do begin
    aBuffStrLength := SocketRead(aBuffStr[1], length(aBuffStr));
    If aBuffStrLength <= 0 then raise Exception.Create('Connection close gracefully!');
    Result := Result + AlCopyStr(aBuffStr,1,aBuffStrLength);
    aResponseLength := length(Result);

    If aGoodResponse = -1 then begin
      if ALPos('+OK ', Result) = 1 then aGoodResponse := 1
      else if ALPos('-ERR ', Result) = 1 then aGoodResponse := 0;
    end;

    If (aGoodResponse = 0) or (not MultilineResponse) then begin
      If (aResponseLength > 1) and
         (Result[aResponseLength] = #10) and
         (Result[aResponseLength - 1] = #13) then Break;
    end
    else begin
      If (aResponseLength > 4) and
         (Result[aResponseLength] = #10) and
         (Result[aResponseLength - 1] = #13) and
         (Result[aResponseLength - 2] = '.') and
         (Result[aResponseLength - 3] = #10) and
         (Result[aResponseLength - 4] = #13) then Break;
    end;
  end;

  If aGoodResponse <> 1 then Raise Exception.Create(Result);
end;

{**********************************************************************}
Function TAlPOP3Client.SocketWrite(Var Buffer; Count: Longint): Longint;
begin
  if not FConnected then raise Exception.Create('Not Connected');
  Result := Send(FSocketDescriptor,Buffer,Count,0);
  CheckError(Result =  SOCKET_ERROR);
end;

{*********************************************************************}
function TAlPOP3Client.SocketRead(var Buffer; Count: Longint): Longint;
begin
  if not FConnected then raise Exception.Create('Not Connected');
  Result := Recv(FSocketDescriptor,Buffer,Count,0);
  CheckError(Result = SOCKET_ERROR);
end;

{*******************************************************}
procedure TAlPOP3Client.Settimeout(const Value: integer);
begin
  If Value <> Ftimeout then begin
    if FConnected then begin
      CheckError(setsockopt(FSocketDescriptor,SOL_SOCKET,SO_RCVTIMEO,PChar(@FTimeOut),SizeOf(Integer))=SOCKET_ERROR);
      CheckError(setsockopt(FSocketDescriptor,SOL_SOCKET,SO_SNDTIMEO,PChar(@FTimeOut),SizeOf(Integer))=SOCKET_ERROR);
    end;
    Ftimeout := Value;
  end;
end;

end.
