{*******************************************************************************
TALNNTPClient class implements the NNTP protocol (RFC-977 and RFC-850)
Support authentification (RFC-977 Extension)
*******************************************************************************}

unit ALNNTPClient;

interface

Uses
  Winapi.WinSock2,
  ALInternetMessages,
  ALMultiPartParser,
  ALStringList;

type

    {----------------------------}
    TAlNNTPClient = class(TObject)
    Private
      Fconnected: Boolean;
      FSocketDescriptor: TSocket;
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
      Function SendCmd(aCmd:AnsiString; const OkResponses: array of Word; const MultilineResponse: Boolean=False): AnsiString; virtual;
      Function GetResponse(const OkResponses: array of Word; const MultilineResponse: Boolean=False): AnsiString;
      Function SocketWrite(const Buf; len: Integer): Integer; Virtual;
      Function SocketRead(var buf; len: Integer): Integer; Virtual;
    public
      constructor Create; virtual;
      destructor Destroy; override;
      Function  Connect(const aHost: AnsiString; const APort: integer): AnsiString; virtual;
      Procedure Disconnect; virtual;
      Function  GetStatusCodeFromResponse(const aResponse: AnsiString): Integer;
      Procedure AUTHINFO(const UserName, Password: AnsiString); virtual;
      Function  List: AnsiString; overload; virtual;
      procedure List(ALst: TALStrings); overload; virtual;
      Function  ListDistributions: AnsiString; overload; virtual;
      procedure ListDistributions(ALst: TALStrings); overload; virtual;
      Function  ListNewsGroups(wildmat: AnsiString): AnsiString; overload; virtual;
      procedure ListNewsGroups(ALst: TALStrings; const wildmat: AnsiString); overload; virtual;
      Function  ListGroup(NewsGroup: AnsiString): AnsiString; overload; virtual;
      procedure ListGroup(ALst: TALStrings; const NewsGroup: AnsiString); overload; virtual;
      Function  NewsGroups(FromGMTDate: TdateTime; const distributions: AnsiString): AnsiString; overload; virtual;
      procedure NewsGroups(FromGMTDate: TdateTime; const distributions: AnsiString; ALst: TALStrings); overload; virtual;
      Function  NewNews(const NewsGroups: AnsiString; FromGMTDate: TdateTime; const distributions: AnsiString): AnsiString; overload; virtual;
      procedure NewNews(const NewsGroups: AnsiString; FromGMTDate: TdateTime; const distributions: AnsiString; ALst: TALStrings); overload; virtual;
      function  Group(const NewsGroupName : AnsiString): AnsiString; overload; virtual;
      Procedure Group(const NewsGroupName: AnsiString; Var EstimatedNumberArticles, FirstArticleNumber, LastArticleNumber: Integer); overload; Virtual;
      Function  ArticleByNumber(ArticleNumber: Integer): AnsiString; overload; virtual;
      Procedure ArticleByNumber(ArticleNumber: Integer; Var ArticleContent: AnsiString); overload; virtual;
      Procedure ArticleByNumber(ArticleNumber: Integer; Var ArticleBodyContent: AnsiString; ArticleHeaderContent: TALNewsArticleHeader); overload; virtual;
      Function  ArticleByID(const ArticleID : AnsiString): AnsiString; overload; virtual;
      Procedure ArticleByID(const ArticleID : AnsiString; Var ArticleContent: AnsiString); overload; virtual;
      Procedure ArticleByID(const ArticleID : AnsiString; Var ArticleBodyContent: AnsiString; ArticleHeaderContent: TALNewsArticleHeader); overload; virtual;
      function  Article: AnsiString; overload; virtual;
      procedure Article(var ArticleContent: AnsiString); overload; virtual;
      procedure Article(var ArticleBodyContent: AnsiString; ArticleHeaderContent: TALNewsArticleHeader); overload; virtual;
      Function  HeadByID(const ArticleID : AnsiString): AnsiString; overload; virtual;
      Procedure HeadByID(const ArticleID : AnsiString; Var HeadContent: AnsiString); overload; virtual;
      Procedure HeadByID(const ArticleID : AnsiString; HeadContent: TALNewsArticleHeader); overload; virtual;
      Function  HeadByNumber(ArticleNumber: Integer): AnsiString; overload; virtual;
      Procedure HeadByNumber(ArticleNumber: Integer; Var HeadContent: AnsiString); overload; virtual;
      Procedure HeadByNumber(ArticleNumber: Integer; HeadContent: TALNewsArticleHeader); overload; virtual;
      function  Head: AnsiString; overload; virtual;
      procedure Head(var HeadContent: AnsiString); overload; virtual;
      procedure Head(HeadContent: TALNewsArticleHeader); overload; virtual;
      Function  BodyByID(const ArticleID : AnsiString): AnsiString; overload; virtual;
      Procedure BodyByID(const ArticleID : AnsiString; Var BodyContent: AnsiString); overload; virtual;
      Function  BodyByNumber(ArticleNumber: Integer): AnsiString; overload; virtual;
      Procedure BodyByNumber(ArticleNumber: Integer; Var BodyContent: AnsiString); overload; virtual;
      Function  Body: AnsiString; overload; virtual;
      Procedure Body(Var BodyContent: AnsiString); overload; virtual;
      Function  StatByID(const ArticleID : AnsiString): AnsiString; overload; virtual;
      Procedure StatByID(const ArticleID : AnsiString; Var ArticleNumber: integer); overload; virtual;
      Function  StatByNumber(ArticleNumber: Integer): AnsiString; overload; virtual;
      Procedure StatByNumber(ArticleNumber: Integer; Var ArticleID: AnsiString); overload; virtual;
      Function  Stat: AnsiString; overload; virtual;
      Procedure Stat(Var ArticleID : AnsiString; Var ArticleNumber: integer); overload; virtual;
      Function  XHDRByID(const HeaderName: AnsiString; const ArticleID: AnsiString): AnsiString; overload; virtual;
      Procedure XHDRByID(const HeaderName: AnsiString; const ArticleID: AnsiString; var HeaderContent: AnsiString); overload; virtual;
      Function  XHDRByNumber(const HeaderName: AnsiString; ArticleNumber: integer): AnsiString; overload; virtual;
      Procedure XHDRByNumber(const HeaderName: AnsiString; ArticleNumber: integer; var HeaderContent: AnsiString); overload; virtual;
      Function  XHDR(const HeaderName: AnsiString): AnsiString; overload; virtual;
      Procedure XHDR(const HeaderName: AnsiString; var HeaderContent: AnsiString); overload; virtual;
      Function  Next: Boolean; overload; virtual;
      Procedure Next(Var ArticleNumber: Integer; Var ArticleID: AnsiString); overload; virtual;
      Function  Last: Boolean; overload; virtual;
      Procedure Last(Var ArticleNumber: Integer; Var ArticleID: AnsiString); overload; virtual;
      Function  IHave(const ArticleID: AnsiString; ArticleContent: AnsiString): Boolean; virtual;
      Procedure Post(ArticleContent: AnsiString); overload; virtual;
      Procedure Post(const HeaderContent, BodyContent: AnsiString); overload; virtual;
      Procedure Post(HeaderContent:TALNewsArticleHeader; const BodyContent: AnsiString); overload; virtual;
      Procedure PostMultipartMixed(HeaderContent: TALNewsArticleHeader; const InlineText, InlineTextContentType: AnsiString; Attachments: TALMultiPartMixedContents); virtual;
      function  Slave: AnsiString; virtual;
      Function  Quit: AnsiString; virtual;
      property  Connected: Boolean read FConnected;
      property  SendTimeout: Integer read FSendTimeout write SetSendTimeout;
      property  ReceiveTimeout: Integer read FReceiveTimeout write SetReceiveTimeout;
      property  KeepAlive: Boolean read fKeepAlive write SetKeepAlive;
      property  TcpNoDelay: Boolean read fTCPNoDelay write fTCPNoDelay;
    end;

implementation

Uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  AlWinsock,
  ALString;

{*******************************************************************************}
Procedure ALNNTPClientSplitResponseLine(aResponse: AnsiString; ALst: TALStrings);
Begin
  aResponse := ALTrim(aResponse); // 211 111225 12861 362539 nzn.fr.delphi
  aResponse := AlStringReplace(aResponse,#9,' ',[RfReplaceAll]); // 211 111225 12861 362539 nzn.fr.delphi
  While alPos('  ',aResponse) > 0 do aResponse := AlStringReplace(aResponse,'  ',' ',[RfReplaceAll]); // 211 111225 12861 362539 nzn.fr.delphi
  aResponse := ALTrim(AlStringReplace(aResponse,' ',#13#10,[RfReplaceAll])); // 211
                                                                           // 111225
                                                                           // 12861
                                                                           // 362539
                                                                           // nzn.fr.delphi
  aLst.Text := aResponse;
end;

{******************************************************************************************************}
Function ALNNTPClientExtractTextFromMultilineResponse(const aMultilineResponse: AnsiString): AnsiString;
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

  {If the text contained a period as the first character of the text
  line in the original, that first period is doubled.  Therefore, the
  client must examine the first character of each line received, and
  for those beginning with a period, determine either that this is the
  end of the text or whether to collapse the doubled period to a single one.}
  Result := AlStringReplace(Result,#13#10'..',#13#10'.', [RfReplaceAll]);
end;

{**************************************************************************************************************}
Function ALNNTPClientEnclosedInAngleBrackets(const Str: AnsiString; const insSpace: Boolean = True): AnsiString;
begin
  Result := ALTrim(Str);
  If Result <> '' then begin
    IF Result[1] <> '<' then Result := '<' + Result;
    IF Result[length(Result)] <> '>' then Result := Result + '>';
    if insSpace then Result := ' ' + Result;
  end;
end;

{*******************************}
constructor TAlNNTPClient.Create;
var LWSAData: TWSAData;
begin
  CheckError(WSAStartup(MAKEWORD(2,2), LWSAData) <> 0);
  Fconnected:= False;
  FSocketDescriptor:= INVALID_SOCKET;
  FSendTimeout := 60000; // 60 seconds
  FReceiveTimeout := 60000; // 60 seconds
  FKeepAlive := True;
  fTCPNoDelay := True;
end;

{*******************************}
destructor TAlNNTPClient.Destroy;
begin
  If Fconnected then Disconnect;
  WSACleanup;
  inherited;
end;

{*************************************************}
procedure TAlNNTPClient.CheckError(Error: Boolean);
begin
  if Error then RaiseLastOSError;
end;

{****************************************************************************************}
Function TAlNNTPClient.Connect(const aHost: AnsiString; const APort: integer): AnsiString;

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
  if FConnected then raise Exception.Create('Already connected');

  Try

    _CallServer(aHost,aPort);
    Fconnected := True;
    SetSendTimeout(FSendTimeout);
    SetReceiveTimeout(FReceiveTimeout);
    SetKeepAlive(FKeepAlive);
    SetTCPNoDelay(fTCPNoDelay);
    Result := GetResponse([200, 201]);

  Except
    Disconnect;
    raise;
  end;

end;

{*********************************}
procedure TAlNNTPClient.Disconnect;
begin
  If Fconnected then begin
    ShutDown(FSocketDescriptor,SD_BOTH);
    CloseSocket(FSocketDescriptor);
    FSocketDescriptor := INVALID_SOCKET;
    Fconnected := False;
  end;
end;

{*******}
{AUTHINFO

 AUTHINFO is used to inform a server about the identity of a user of
 the server.  In all cases, clients must provide this information when
 requested by the server.  Servers are not required to accept
 authentication information that is volunteered by the client.
 Clients must accommodate servers that reject any authentication
 information volunteered by the client.

 There are three forms of AUTHINFO in use.  The original version, an
 NNTP v2 revision called AUTHINFO SIMPLE and a more recent version
 which is called AUTHINFO GENERIC.


 Original AUTHINFO

 AUTHINFO USER username
 AUTHINFO PASS password

 The original AUTHINFO is used to identify a specific entity to the
 server using a simple username/password combination.  It first
 appeared in the UNIX reference implementation.

 When authorization is required, the server will send a 480 response
 requesting authorization from the client.  The client must enter
 AUTHINFO USER followed by the username.  Once sent, the server will
 cache the username and may send a 381 response requesting the
 password associated with that username.  Should the server request a
 password using the 381 response, the client must enter AUTHINFO PASS
 followed by a password and the server will then check the
 authentication database to see if the username/password combination
 is valid.  If the combination is valid or if no password is required,
 the server will return a 281 response.  The client should then retry
 the original command to which the server responded with the 480
 response.  The command should then be processed by the server
 normally.  If the combination is not valid, the server will return a
 502 response.

 Clients must provide authentication when requested by the server.  It
 is possible that some implementations will accept authentication
 information at the beginning of a session, but this was not the
 original intent of the specification.  If a client attempts to
 reauthenticate, the server may return 482 response indicating that
 the new authentication data is rejected by the server.  The 482 code
 will also be returned when the AUTHINFO commands are not entered in
 the correct sequence (like two AUTHINFO USERs in a row, or AUTHINFO
 PASS preceding AUTHINFO USER).

 All information is passed in cleartext.

 When authentication succeeds, the server will create an email address
 for the client from the user name supplied in the AUTHINFO USER
 command and the hostname generated by a reverse lookup on the IP
 address of the client.  If the reverse lookup fails, the IP address,
 represented in dotted-quad format, will be used.  Once authenticated,
 the server shall generate a Sender:  line using the email address
 provided by authentication if it does not match the client-supplied
 From: line.  Additionally, the server should log the event, including
 the email address.  This will provide a means by which subsequent
 statistics generation can associate newsgroup references with unique
 entities - not necessarily by name.

 Responses

    281 Authentication accepted
    381 More authentication information required
    480 Authentication required
    482 Authentication rejected
    502 No permission


 AUTHINFO SIMPLE

 AUTHINFO SIMPLE
 user password

 This version of AUTHINFO was part of a proposed NNTP V2
 specification, which was started in 1991 but never completed, and is
 implemented in some servers and clients.  It is a refinement of the
 original AUTHINFO and provides the same basic functionality, but the
 sequence of commands is much simpler.

 When authorization is required, the server sends a 450 response
 requesting authorization from the client.  The client must enter
 AUTHINFO SIMPLE.  If the server will accept this form of
 authentication, the server responds with a 350 response.  The client
 must then send the username followed by one or more space characters
 followed by the password.  If accepted, the server returns a 250
 response and the client should then retry the original command to
 which the server responded with the 450 response.  The command should
 then be processed by the server normally.  If the combination is not
 valid, the server will return a 452 response.

 Note that the response codes used here were part of the proposed NNTP
 V2 specification and are violations of RFC 977.  It is recommended
 that this command not be implemented, but use either or both of the
 other forms of AUTHINFO if such functionality if required.

 Responses

    250 Authorization accepted
    350 Continue with authorization sequence
    450 Authorization required for this command
    452 Authorization rejected


 AUTHINFO GENERIC

 AUTHINFO GENERIC authenticator arguments...

 AUTHINFO GENERIC is used to identify a specific entity to the server
 using arbitrary authentication  or identification protocols.  The
 desired protocol is indicated by the authenticator parameter, and any
 number of parameters can be passed to the authenticator.

 When authorization is required, the server will send a 480 response
 requesting authorization from the client.  The client should enter
 AUTHINFO GENERIC followed by the authenticator name, and the
 arguments if any.  The authenticator and arguments must not contain
 the sequence "..".

 The server will attempt to engage the server end authenticator,
 similarly, the client should engage the client end authenticator.
 The server end authenticator will then initiate authentication using
 the NNTP sockets (if appropriate for that authentication protocol),
 using the protocol specified by the authenticator name.  These
 authentication protocols are not included in this document, but are
 similar in structure to those referenced in RFC 1731 [8] for the
 IMAP-4 protocol.

 If the server returns 501, this means that the authenticator
 invocation was syntactically incorrect, or that AUTHINFO GENERIC is
 not supported.  The client should retry using the AUTHINFO USER
 command.

 If the requested authenticator capability is not found, the server
 returns the 503 response code.

 If there is some other unspecified server program error, the server
 returns the 500 response code.

 The authenticators converse using their protocol until complete.  If
 the authentication succeeds, the server authenticator will terminate
 with a 281, and the client can continue by reissuing the command that
 prompted the 380.  If the authentication fails, the server will
 respond with a 502.

 The client must provide authentication when requested by the server.
 The server may request authentication at any time.  Servers may
 request authentication more than once during a single session.

 When the server authenticator completes, it provides to the server
 (by a mechanism herein undefined) the email address of the user, and
 potentially what the user is allowed to access.  Once authenticated,
 the server shall generate a Sender:  line using the email address
 provided by the authenticator if it does not match the user-supplied
 From: line.  Additionally, the server should log the event, including
 the user's authenticated email address (if available).  This will
 provide a means by which subsequent statistics generation can
 associate newsgroup references with unique entities - not necessarily
 by name.

 Some implementations make it possible to obtain a list of
 authentication procedures available by sending the server AUTHINFO
 GENERIC with no arguments.  The server then returns a list of
 supported mechanisms followed by a period on a line by itself.

 Responses

    281 Authentication succeeded
    480 Authentication required
    500 Command not understood
    501 Command not supported
    502 No permission
    503 Program error, function not performed
    nnn  authenticator-specific protocol.}
procedure TAlNNTPClient.AUTHINFO(const UserName, Password: AnsiString);
begin
  SendCmd('AUTHINFO USER ' + UserName,[381], False);
  SendCmd('AUTHINFO PASS ' + Password,[281], False);
end;

{***}
{LIST

 Returns a list of valid newsgroups and associated information.  Each
 newsgroup is sent as a line of text in the following format:

    group last first p

 where <group> is the name of the newsgroup, <last> is the number of
 the last known article currently in that newsgroup, <first> is the
 number of the first article currently in the newsgroup, and <p> is
 either 'y' or 'n' indicating whether posting to this newsgroup is
 allowed ('y') or prohibited ('n').

 The <first> and <last> fields will always be numeric.  They may have
 leading zeros.  If the <last> field evaluates to less than the
 <first> field, there are no articles currently on file in the
 newsgroup.

 Note that posting may still be prohibited to a client even though the
 LIST command indicates that posting is permitted to a particular
 newsgroup. See the POST command for an explanation of client
 prohibitions.  The posting flag exists for each newsgroup because
 some newsgroups are moderated or are digests, and therefore cannot be
 posted to; that is, articles posted to them must be mailed to a
 moderator who will post them for the submitter.  This is independent
 of the posting permission granted to a client by the NNTP server.

 Please note that an empty list (i.e., the text body returned by this
 command consists only of the terminating period) is a possible valid
 response, and indicates that there are currently no valid newsgroups.

 Responses
 215 list of newsgroups follows}
Function TAlNNTPClient.List: AnsiString;
begin
  Result := SendCmd('LIST',[215], True);
end;

{*********************************************}
procedure TAlNNTPClient.List(ALst: TALStrings);
begin
  ALst.Text := ALTrim(ALNNTPClientExtractTextFromMultilineResponse(List));
end;

{*****************}
{LIST DISTRIBUTIONS

 The distributions file is maintained by some news transport systems
 to contain information about valid values for the Distribution: line
 in a news article header and about what the values mean.  Each line

 contains two fields, the value and a short explanation on the meaning
 of the value.  When executed, the information is displayed following
 the 215 response.  When display is completed, the server will send a
 period on a line by itself.  If the information is not available, the
 server will return the 503 error response.  This command first
 appeared in the UNIX reference version.

 Responses

 215 information follows
 503 program error, function not performed}
function TAlNNTPClient.ListDistributions: AnsiString;
begin
  Result := SendCmd('LIST DISTRIBUTIONS',[215], True);
end;

{**********************************************************}
procedure TAlNNTPClient.ListDistributions(ALst: TALStrings);
begin
  ALst.Text := ALTrim(ALNNTPClientExtractTextFromMultilineResponse(ListDISTRIBUTIONS));
end;

{************************}
{LIST NEWSGROUPS [wildmat]

 The newsgroups file is maintained by some news transport systems to
 contain the name of each news group which is active on the server and
 a short description about the purpose of each news group.  Each line
 in the file contains two fields, the news group name and a short
 explanation of the purpose of that news group.  When executed, the
 information is displayed following the 215 response.  When display is
 completed, the server will send a period on a line by itself.  If the
 information is not available, the server will return the 503
 response.  If the optional matching parameter is specified, the list
 is limited to only the groups that match the pattern (no matching is
 done on the group descriptions).  Specifying a single group is
 usually very efficient for the server, and multiple groups may be
 specified by using wildmat patterns (similar to file globbing), not
 regular expressions.  If nothing is matched an empty list is
 returned, not an error.

 When the optional parameter is specified, this command is equivalent
 to the XGTITLE command, though the response code are different.

    215 information follows
    503 program error, function not performed}
function TAlNNTPClient.ListNewsGroups(wildmat: AnsiString): AnsiString;
begin
  if wildmat <> '' then wildmat := ' ' + wildmat;
  Result := SendCmd('LIST NEWSGROUPS' + wildmat,[215], True);
end;

{**********************************************************************************}
procedure TAlNNTPClient.ListNewsGroups(ALst: TALStrings; const wildmat: AnsiString);
begin
  ALst.Text := ALTrim(ALNNTPClientExtractTextFromMultilineResponse(ListNewsGroups(wildmat)));
end;

{**************}
{LISTGROUP [ggg]

 The LISTGROUP command is used to get a listing of all the article
 numbers in a particular news group.

 The optional parameter ggg is the name of the news group to be
 selected (e.g. "news.software.b").  A list of valid news groups may
 be obtained from the LIST command.  If no group is specified, the
 current group is used as the default argument.

 The successful selection response will be a list of the article
 numbers in the group followed by a period on a line by itself.

 When a valid group is selected by means of this command, the
 internally maintained "current article pointer" is set to the first
 article in the group.  If an invalid group is specified, the
 previously selected group and article remain selected.  If an empty
 news group is selected, the "current article pointer" is in an
 indeterminate state and should not be used.

 Note that the name of the news group is not case-dependent.  It must
 otherwise match a news group obtained from the LIST command or an
 error will result.

 Responses

 211 list of article numbers follow
 412 Not currently in newsgroup
 502 no permission}
function TAlNNTPClient.ListGroup(NewsGroup: AnsiString): AnsiString;
begin
  if NewsGroup <> '' then NewsGroup := ' ' + NewsGroup;
  Result := SendCmd('LISTGROUP' + NewsGroup,[211], True);
end;

{*******************************************************************************}
procedure TAlNNTPClient.ListGroup(ALst: TALStrings; const NewsGroup: AnsiString);
begin
  ALst.Text := ALTrim(ALNNTPClientExtractTextFromMultilineResponse(ListGroup(NewsGroup)));
end;

{******************************************}
{NEWGROUPS date time [GMT] [<distributions>]

 A list of newsgroups created since <date and time> will be listed in
 the same format as the LIST command.

 The date is sent as 6 digits in the format YYMMDD, where YY is the
 last two digits of the year, MM is the two digits of the month (with
 leading zero, if appropriate), and DD is the day of the month (with
 leading zero, if appropriate).  The closest century is assumed as
 part of the year (i.e., 86 specifies 1986, 30 specifies 2030, 99 is
 1999, 00 is 2000).

 Time must also be specified.  It must be as 6 digits HHMMSS with HH
 being hours on the 24-hour clock, MM minutes 00-59, and SS seconds
 00-59.  The time is assumed to be in the server's timezone unless the
 token "GMT" appears, in which case both time and date are evaluated
 at the 0 meridian.

 The optional parameter "distributions" is a list of distribution
 groups, enclosed in angle brackets.  If specified, the distribution
 portion of a new newsgroup (e.g, 'net' in 'net.wombat') will be
 examined for a match with the distribution categories listed, and
 only those new newsgroups which match will be listed.  If more than
 one distribution group is to be listed, they must be separated by
 commas within the angle brackets.

 Please note that an empty list (i.e., the text body returned by this
 command consists only of the terminating period) is a possible valid
 response, and indicates that there are currently no new newsgroups.

 Responses
 231 list of new newsgroups follows}
Function TAlNNTPClient.NewsGroups(FromGMTDate: TdateTime; const distributions: AnsiString): AnsiString;
begin
  Result := SendCmd('NEWGROUPS ' + ALFormatDateTime('yymmdd" "hhnnss',FromGMTDate, ALDefaultFormatSettings) + ' GMT' + ALNNTPClientEnclosedInAngleBrackets(distributions), [231], True);
end;

{************************************************************************************************************}
procedure TAlNNTPClient.NewsGroups(FromGMTDate: TdateTime; const distributions: AnsiString; ALst: TALStrings);
begin
  ALst.Text := ALTrim(ALNNTPClientExtractTextFromMultilineResponse(NewsGroups(FromGMTDate,distributions)));
end;

{**************************************************}
{NEWNEWS newsgroups date time [GMT] [<distribution>]

 A list of message-ids of articles posted or received to the specified
 newsgroup since "date" will be listed. The format of the listing will
 be one message-id per line, as though text were being sent.  A single
 line consisting solely of one period followed by CR-LF will terminate
 the list.

 Date and time are in the same format as the NEWGROUPS command.

 A newsgroup name containing a "*" (an asterisk) may be specified to
 broaden the article search to some or all newsgroups.  The asterisk
 will be extended to match any part of a newsgroup name (e.g.,
 net.micro* will match net.micro.wombat, net.micro.apple, etc). Thus
 if only an asterisk is given as the newsgroup name, all newsgroups
 will be searched for new news.

 (Please note that the asterisk "*" expansion is a general
 replacement; in particular, the specification of e.g., net.*.unix
 should be correctly expanded to embrace names such as net.wombat.unix
 and net.whocares.unix.)

 Conversely, if no asterisk appears in a given newsgroup name, only
 the specified newsgroup will be searched for new articles. Newsgroup
 names must be chosen from those returned in the listing of available
 groups.  Multiple newsgroup names (including a "*") may be specified
 in this command, separated by a comma.  No comma shall appear after
 the last newsgroup in the list.  [Implementors are cautioned to keep
 the 512 character command length limit in mind.]

 The exclamation point ("!") may be used to negate a match. This can
 be used to selectively omit certain newsgroups from an otherwise
 larger list.  For example, a newsgroups specification of
 "net.*,mod.*,!mod.map.*" would specify that all net.<anything> and
 all mod.<anything> EXCEPT mod.map.<anything> newsgroup names would be
 matched.  If used, the exclamation point must appear as the first
 character of the given newsgroup name or pattern.

 The optional parameter "distributions" is a list of distribution
 groups, enclosed in angle brackets.  If specified, the distribution
 portion of an article's newsgroup (e.g, 'net' in 'net.wombat') will
 be examined for a match with the distribution categories listed, and
 only those articles which have at least one newsgroup belonging to
 the list of distributions will be listed.  If more than one
 distribution group is to be supplied, they must be separated by
 commas within the angle brackets.

 Please note that an empty list (i.e., the text body returned by this
 command consists only of the terminating period) is a possible valid
 response, and indicates that there is currently no new news.


 Responses
 230 list of new articles by message-id follows}
Function TAlNNTPClient.NewNews(const Newsgroups: AnsiString; FromGMTDate: TdateTime; const distributions: AnsiString): AnsiString;
begin
  Result := SendCmd('NEWNEWS ' + newsgroups + ' ' + ALFormatDateTime('yymmdd" "hhnnss',FromGMTDate, ALDefaultFormatSettings) + ' GMT' + ALNNTPClientEnclosedInAngleBrackets(distributions), [230], True);
end;

{***************************************************************************************************************************************}
procedure TAlNNTPClient.NewNews(const Newsgroups: AnsiString; FromGMTDate: TdateTime; const distributions: AnsiString; ALst: TALStrings);
begin
  ALst.Text := ALTrim(ALNNTPClientExtractTextFromMultilineResponse(NewNews(NewsGroups, FromGMTDate, distributions)));
end;

{*********}
{GROUP ggg

 The required parameter ggg is the name of the newsgroup to be
 selected (e.g. "net.news").  A list of valid newsgroups may be
 obtained from the LIST command.

 The successful selection response will return the article numbers of
 the first and last articles in the group, and an estimate of the
 number of articles on file in the group.  It is not necessary that
 the estimate be correct, although that is helpful; it must only be
 equal to or larger than the actual number of articles on file.  (Some
 implementations will actually count the number of articles on file.
 Others will just subtract first article number from last to get an
 estimate.)

 When a valid group is selected by means of this command, the
 internally maintained "current article pointer" is set to the first
 article in the group.  If an invalid group is specified, the
 previously selected group and article remain selected.  If an empty
 newsgroup is selected, the "current article pointer" is in an
 indeterminate state and should not be used.

 Note that the name of the newsgroup is not case-dependent.  It must
 otherwise match a newsgroup obtained from the LIST command or an
 error will result.

 Responses
 211 n f l s group selected
     n = estimated number of articles in group,
     f = first article number in the group,
     l = last article number in the group,
     s = name of the group.
 411 no such news group }
Function TAlNNTPClient.Group(const NewsGroupName: AnsiString): AnsiString;
begin
  Result := SendCmd('GROUP ' + ALTrim(NewsGroupName),[211], False); //211 111225 12861 362539 nzn.fr.delphi
end;

{************************************************************}
Procedure TAlNNTPClient.Group(const NewsGroupName: AnsiString;
                              Var EstimatedNumberArticles,
                                  FirstArticleNumber,
                                  LastArticleNumber: Integer);
Var LLst: TALStringList;
Begin
  LLst := TALStringList.Create;
  Try
    ALNNTPClientSplitResponseLine(Group(NewsGroupName), LLst);
    If LLst.Count < 4 then raise Exception.Create('GROUP cmd Error');
    If not ALTryStrToInt(LLst[1], EstimatedNumberArticles) then raise Exception.Create('GROUP cmd Error');
    If not ALTryStrToInt(LLst[2], FirstArticleNumber) then raise Exception.Create('GROUP cmd Error');
    If not ALTryStrToInt(LLst[3], LastArticleNumber) then raise Exception.Create('GROUP cmd Error');
  finally
    LLst.Free;
  end;
end;

{*****************************************}
{The ARTICLE, BODY, HEAD, and STAT commands
 There are two forms to the ARTICLE command (and the related BODY,
 HEAD, and STAT commands), each using a different method of specifying
 which article is to be retrieved.  When the ARTICLE command is
 followed by a message-id in angle brackets ("<" and ">"), the first
 form of the command is used; when a numeric parameter or no parameter
 is supplied, the second form is invoked.

 The text of the article is returned as a textual response, as
 described earlier in this document.

 The HEAD and BODY commands are identical to the ARTICLE command
 except that they respectively return only the header lines or text
 body of the article.

 The STAT command is similar to the ARTICLE command except that no
 text is returned.  When selecting by message number within a group,
 the STAT command serves to set the current article pointer without
 sending text. The returned acknowledgement response will contain the
 message-id, which may be of some value.  Using the STAT command to
 select by message-id is valid but of questionable value, since a
 selection by message-id does NOT alter the "current article pointer".


 ARTICLE <message-id>

 Display the header, a blank line, then the body (text) of the
 specified article.  Message-id is the message id of an article as
 shown in that article's header.  It is anticipated that the client
 will obtain the message-id from a list provided by the NEWNEWS
 command, from references contained within another article, or from
 the message-id provided in the response to some other commands.

 Please note that the internally-maintained "current article pointer"
 is NOT ALTERED by this command. This is both to facilitate the
 presentation of articles that may be referenced within an article
 being read, and because of the semantic difficulties of determining
 the proper sequence and membership of an article which may have been
 posted to more than one newsgroup.


 ARTICLE [nnn]

 Displays the header, a blank line, then the body (text) of the
 current or specified article.  The optional parameter nnn is the

 numeric id of an article in the current newsgroup and must be chosen
 from the range of articles provided when the newsgroup was selected.
 If it is omitted, the current article is assumed.

 The internally-maintained "current article pointer" is set by this
 command if a valid article number is specified.

 [the following applies to both forms of the article command.] A
 response indicating the current article number, a message-id string,
 and that text is to follow will be returned.

 The message-id string returned is an identification string contained
 within angle brackets ("<" and ">"), which is derived from the header
 of the article itself.  The Message-ID header line (required by
 RFC850) from the article must be used to supply this information. If
 the message-id header line is missing from the article, a single
 digit "0" (zero) should be supplied within the angle brackets.

 Since the message-id field is unique with each article, it may be
 used by a news reading program to skip duplicate displays of articles
 that have been posted more than once, or to more than one newsgroup.


 Responses
 220 n <a> article retrieved - head and body follow
         (n = article number, <a> = message-id)
 221 n <a> article retrieved - head follows
 222 n <a> article retrieved - body follows
 223 n <a> article retrieved - request text separately
 412 no newsgroup has been selected
 420 no current article has been selected
 423 no such article number in this group
 430 no such article found}
function TAlNNTPClient.ArticleByID(const ArticleID: AnsiString): AnsiString;
begin
  Result := SendCmd('ARTICLE' + ALNNTPClientEnclosedInAngleBrackets(ArticleID),[220], True);
end;

{***********************************************************************************************}
procedure TAlNNTPClient.ArticleByID(const ArticleID: AnsiString; var ArticleContent: AnsiString);
begin
  ArticleContent := ALNNTPClientExtractTextFromMultilineResponse(ArticleByID(ArticleID));
end;

{**************************************************************}
procedure TAlNNTPClient.ArticleByID(const ArticleID: AnsiString;
                                    var ArticleBodyContent: AnsiString;
                                    ArticleHeaderContent: TALNewsArticleHeader);
Var P: integer;
begin
  ArticleBodyContent := ALNNTPClientExtractTextFromMultilineResponse(ArticleByID(ArticleID));
  P := AlPos(#13#10#13#10,ArticleBodyContent);
  If P <= 0 then raise Exception.Create('Bad formated article!');
  ArticleHeaderContent.RawHeaderText := AlcopyStr(ArticleBodyContent,1,P);
  Delete(ArticleBodyContent,1,P+3);
end;

{*************************************************************************}
function TAlNNTPClient.ArticleByNumber(ArticleNumber: Integer): AnsiString;
begin
  Result := SendCmd('ARTICLE ' + ALIntToStr(ArticleNumber),[220], true);
end;

{**********************************************************************************************}
procedure TAlNNTPClient.ArticleByNumber(ArticleNumber: Integer; var ArticleContent: AnsiString);
begin
  ArticleContent := ALNNTPClientExtractTextFromMultilineResponse(ArticleByNumber(ArticleNumber));
end;

{*************************************************************}
procedure TAlNNTPClient.ArticleByNumber(ArticleNumber: Integer;
                                        var ArticleBodyContent: AnsiString;
                                        ArticleHeaderContent: TALNewsArticleHeader);
Var P: integer;
begin
  ArticleBodyContent := ALNNTPClientExtractTextFromMultilineResponse(ArticleByNumber(ArticleNumber));
  P := AlPos(#13#10#13#10,ArticleBodyContent);
  If P <= 0 then raise Exception.Create('Bad formated article!');
  ArticleHeaderContent.RawHeaderText := AlcopyStr(ArticleBodyContent,1,P);
  Delete(ArticleBodyContent,1,P+3);
end;

{*****************************************}
function TAlNNTPClient.Article: AnsiString;
Begin
  Result := SendCmd('ARTICLE', [220], True);
end;

{**************************************************************}
procedure TAlNNTPClient.Article(var ArticleContent: AnsiString);
Begin
  ArticleContent := ALNNTPClientExtractTextFromMultilineResponse(Article);
end;

{**************************************************************************************************************}
procedure TAlNNTPClient.Article(var ArticleBodyContent: AnsiString; ArticleHeaderContent: TALNewsArticleHeader);
Var P: integer;
begin
  ArticleBodyContent := ALNNTPClientExtractTextFromMultilineResponse(Article);
  P := AlPos(#13#10#13#10,ArticleBodyContent);
  If P <= 0 then raise Exception.Create('Bad formated article!');
  ArticleHeaderContent.RawHeaderText := AlcopyStr(ArticleBodyContent,1,P);
  Delete(ArticleBodyContent,1,P+3);
end;

{***********************************************************************}
function TAlNNTPClient.HeadByID(const ArticleID: AnsiString): AnsiString;
begin
  Result := SendCmd('HEAD' + ALNNTPClientEnclosedInAngleBrackets(ArticleID),[221], True);
end;

{*****************************************************************************************}
procedure TAlNNTPClient.HeadByID(const ArticleID: AnsiString; var HeadContent: AnsiString);
begin
  HeadContent := ALNNTPClientExtractTextFromMultilineResponse(HeadByID(ArticleID));
end;

{***********************************************************************************************}
procedure TAlNNTPClient.HeadByID(const ArticleID: AnsiString; HeadContent: TALNewsArticleHeader);
begin
  HeadContent.RawHeaderText := ALNNTPClientExtractTextFromMultilineResponse(HeadByID(ArticleID));
end;

{**********************************************************************}
function TAlNNTPClient.HeadByNumber(ArticleNumber: Integer): AnsiString;
begin
  Result := SendCmd('HEAD ' + ALIntToStr(ArticleNumber),[221], true);
end;

{****************************************************************************************}
procedure TAlNNTPClient.HeadByNumber(ArticleNumber: Integer; var HeadContent: AnsiString);
begin
  HeadContent := ALNNTPClientExtractTextFromMultilineResponse(HeadByNumber(ArticleNumber));
end;

{**********************************************************************************************}
procedure TAlNNTPClient.HeadByNumber(ArticleNumber: Integer; HeadContent: TALNewsArticleHeader);
begin
  HeadContent.RawHeaderText := ALNNTPClientExtractTextFromMultilineResponse(HeadByNumber(ArticleNumber));
end;

{**************************************}
function TAlNNTPClient.Head: AnsiString;
begin
  Result := SendCmd('HEAD',[221], True);
end;

{********************************************************}
procedure TAlNNTPClient.Head(var HeadContent: AnsiString);
begin
  HeadContent := ALNNTPClientExtractTextFromMultilineResponse(Head);
end;

{**************************************************************}
procedure TAlNNTPClient.Head(HeadContent: TALNewsArticleHeader);
begin
  HeadContent.RawHeaderText := ALNNTPClientExtractTextFromMultilineResponse(Head);
end;

{***********************************************************************}
function TAlNNTPClient.BodyByID(const ArticleID: AnsiString): AnsiString;
begin
  Result := SendCmd('BODY' + ALNNTPClientEnclosedInAngleBrackets(ArticleID),[222], True);
end;

{*****************************************************************************************}
procedure TAlNNTPClient.BodyByID(const ArticleID: AnsiString; var BodyContent: AnsiString);
begin
  BodyContent := ALNNTPClientExtractTextFromMultilineResponse(BodyByID(ArticleID));
end;

{**********************************************************************}
function TAlNNTPClient.BodyByNumber(ArticleNumber: Integer): AnsiString;
begin
  Result := SendCmd('BODY ' + ALIntToStr(ArticleNumber),[222], true);
end;

{****************************************************************************************}
procedure TAlNNTPClient.BodyByNumber(ArticleNumber: Integer; var BodyContent: AnsiString);
begin
  BodyContent := ALNNTPClientExtractTextFromMultilineResponse(BodyByNumber(ArticleNumber));
end;

{**************************************}
function TAlNNTPClient.Body: AnsiString;
begin
  Result := SendCmd('BODY',[222], True);
end;

{********************************************************}
procedure TAlNNTPClient.Body(var BodyContent: AnsiString);
begin
  BodyContent := ALNNTPClientExtractTextFromMultilineResponse(Body);
end;

{***********************************************************************}
function TAlNNTPClient.StatByID(const ArticleID: AnsiString): AnsiString;
begin
  Result := SendCmd('STAT' + ALNNTPClientEnclosedInAngleBrackets(ArticleID),[223], False); //223 10110 <23445@sdcsvax.ARPA> status
end;

{****************************************************************************************}
procedure TAlNNTPClient.StatByID(const ArticleID: AnsiString; var ArticleNumber: integer);
Var LLst: TALStringList;
Begin
  LLst := TALStringList.Create;
  Try
    ALNNTPClientSplitResponseLine(StatByID(ArticleID), LLst);
    If LLst.Count < 3 then raise Exception.Create('STAT cmd Error');
    If not ALTryStrToInt(LLst[1], ArticleNumber) then raise Exception.Create('STAT cmd Error');
  finally
    LLst.Free;
  end;
end;

{**********************************************************************}
function TAlNNTPClient.StatByNumber(ArticleNumber: Integer): AnsiString;
begin
  Result := SendCmd('STAT ' + ALIntToStr(ArticleNumber),[223], False); //223 10110 <23445@sdcsvax.ARPA> status
end;

{**************************************************************************************}
procedure TAlNNTPClient.StatByNumber(ArticleNumber: Integer; var ArticleID: AnsiString);
Var LLst: TALStringList;
Begin
  LLst := TALStringList.Create;
  Try
    ALNNTPClientSplitResponseLine(StatByNumber(ArticleNumber), LLst);
    If LLst.Count < 3 then raise Exception.Create('STAT cmd Error');
    ArticleID  := LLst[2]; // <23445@sdcsvax.ARPA>
  finally
    LLst.Free;
  end;
end;

{**************************************}
function TAlNNTPClient.Stat: AnsiString;
begin
  Result := SendCmd('STAT', [223], False); //223 10110 <23445@sdcsvax.ARPA> status
end;


{**********************************************************************************}
procedure TAlNNTPClient.Stat(var ArticleID: AnsiString; var ArticleNumber: integer);
Var LLst: TALStringList;
Begin
  LLst := TALStringList.Create;
  Try
    ALNNTPClientSplitResponseLine(Stat, LLst);
    If LLst.Count < 3 then raise Exception.Create('STAT cmd Error');
    If not ALTryStrToInt(LLst[1],ArticleNumber) then raise Exception.Create('STAT cmd Error');
    ArticleID  := LLst[2]; // <23445@sdcsvax.ARPA>
  finally
    LLst.Free;
  end;
end;

{*******************************}
{XHDR header [range|<message-id>]

 The XHDR command is used to retrieve specific headers from specific
 articles.

 The required parameter is the name of a header line (e.g.  "subject")
 in a news group article.  See RFC 1036 for a list of valid header
 lines.  The optional range argument may be any of the following:

             an article number
             an article number followed by a dash to indicate
                all following
             an article number followed by a dash followed by
                another article number

 The optional message-id argument indicates a specific article.  The
 range and message-id arguments are mutually exclusive.  If no
 argument is specified, then information from the current article is
 displayed.  Successful responses start with a 221 response followed
 by a the matched headers from all matched messages.  Each line
 containing matched headers returned by the server has an article
 number (or message ID, if a message ID was specified in the command),
 then one or more spaces, then the value of the requested header in
 that article.  Once the output is complete, a period is sent on a
 line by itself.  If the optional argument is a message-id and no such
 article exists, the 430 error response is returned.  If a range is
 specified, a news group must have been selected earlier, else a 412
 error response is returned.  If no articles are in the range
 specified, a 420 error response is returned by the server.  A 502
 response will be returned if the client only has permission to
 transfer articles.

 Some implementations will return "(none)" followed by a period on a
 line by itself if no headers match in any of the articles searched.
 Others return the 221 response code followed by a period on a line by
 itself.

 The XHDR command has been available in the UNIX reference
 implementation from its first release.  However, until now, it has
 been documented only in the source for the server.

 Responses

 221 Header follows
 412 No news group current selected
 420 No current article selected
 430 no such article
 502 no permission}
function TAlNNTPClient.XHDRByID(const HeaderName: AnsiString; const ArticleID: AnsiString): AnsiString;
begin
  Result := SendCmd('XHDR ' + HeaderName + ' ' + ALNNTPClientEnclosedInAngleBrackets(ArticleID), [221], True);
end;

{*************************************************************************************************************************}
procedure TAlNNTPClient.XHDRByID(const HeaderName: AnsiString; const ArticleID: AnsiString; Var HeaderContent: AnsiString);
begin
  HeaderContent := ALNNTPClientExtractTextFromMultilineResponse(XHDRByID(HeaderName,ArticleID));
end;

{****************************************************************************************************}
function TAlNNTPClient.XHDRByNumber(const HeaderName: AnsiString; ArticleNumber: integer): AnsiString;
begin
  Result := SendCmd('XHDR ' + HeaderName + ' ' + ALIntToStr(ArticleNumber), [221], True);
end;

{************************************************************************************************************************}
procedure TAlNNTPClient.XHDRByNumber(const HeaderName: AnsiString; ArticleNumber: integer; Var HeaderContent: AnsiString);
begin
  HeaderContent := ALNNTPClientExtractTextFromMultilineResponse(XHDRByNumber(HeaderName,ArticleNumber));
end;

{********************************************************************}
function TAlNNTPClient.XHDR(const HeaderName: AnsiString): AnsiString;
begin
  Result := SendCmd('XHDR ' + HeaderName, [221], True);
end;

{****************************************************************************************}
procedure TAlNNTPClient.XHDR(const HeaderName: AnsiString; Var HeaderContent: AnsiString);
begin
  HeaderContent := ALNNTPClientExtractTextFromMultilineResponse(XHDR(HeaderName));
end;

{******************************************************************}
{The internally maintained "current article pointer" is advanced to
 the next article in the current newsgroup.  If no more articles
 remain in the current group, an error message is returned and the
 current article remains selected.

 The internally-maintained "current article pointer" is set by this
 command.

 A response indicating the current article number, and the message-id
 string will be returned.  No text is sent in response to this
 command.


 Responses
 223 n a article retrieved - request text separately
         (n = article number, a = unique article id)
 412 no newsgroup selected
 420 no current article has been selected
 421 no next article in this group

 Note: I discover some servers that use 422 in place of 421!}
function TAlNNTPClient.Next: Boolean;
Var LStatutCode: integer;
begin
  LStatutCode := GetStatusCodeFromResponse(SendCmd('NEXT',[223,421,422], False)); //223 10113 <21495@nudebch.uucp> article retrieved - request text separately
  Result := LStatutCode=223;
end;

{**********************************************************************************}
procedure TAlNNTPClient.Next(var ArticleNumber: Integer; var ArticleID: AnsiString);
Var LLst: TALStringList;
begin
  LLst := TALStringList.Create;
  Try
    ALNNTPClientSplitResponseLine(SendCmd('NEXT',[223], False), LLst);
    If LLst.Count < 3 then raise Exception.Create('NEXT cmd Error');
    If not ALTryStrToInt(LLst[1],ArticleNumber) then raise Exception.Create('NEXT cmd Error');
    ArticleID  := LLst[2]; //<21495@nudebch.uucp>
  finally
    LLst.Free;
  end;
end;

{*****************************************************************}
{The internally maintained "current article pointer" is set to the
 previous article in the current newsgroup.  If already positioned at
 the first article of the newsgroup, an error message is returned and
 the current article remains selected.

 The internally-maintained "current article pointer" is set by this
 command.

 A response indicating the current article number, and a message-id
 string will be returned.  No text is sent in response to this
 command.


 Responses
 223 n a article retrieved - request text separately
         (n = article number, a = unique article id)
 412 no newsgroup selected
 420 no current article has been selected
 422 no previous article in this group

 Note: I discover some servers that use 421 in place of 422!}
function TAlNNTPClient.Last: Boolean;
Var LStatutCode: integer;
begin
  LStatutCode := GetStatusCodeFromResponse(SendCmd('LAST',[223, 421, 422], False)); //223 10113 <21495@nudebch.uucp> article retrieved
  Result := LStatutCode=223;
end;

{**********************************************************************************}
procedure TAlNNTPClient.Last(var ArticleNumber: Integer; var ArticleID: AnsiString);
Var LLst: TALStringList;
begin
  LLst := TALStringList.Create;
  Try
    ALNNTPClientSplitResponseLine(SendCmd('LAST',[223], False), LLst);
    If LLst.Count < 3 then raise Exception.Create('LAST cmd Error');
    if not ALTryStrToInt(LLst[1],ArticleNumber) then raise Exception.Create('LAST cmd Error');
    ArticleID  := LLst[2]; //<21495@nudebch.uucp>
  finally
    LLst.Free;
  end;
end;

{****************}
{IHAVE <messageid>

 The IHAVE command informs the server that the client has an article
 whose id is <messageid>.  If the server desires a copy of that
 article, it will return a response instructing the client to send the
 entire article.  If the server does not want the article (if, for
 example, the server already has a copy of it), a response indicating
 that the article is not wanted will be returned.

 If transmission of the article is requested, the client should send
 the entire article, including header and body, in the manner
 specified for text transmission from the server. A response code
 indicating success or failure of the transferral of the article will
 be returned.

 This function differs from the POST command in that it is intended
 for use in transferring already-posted articles between hosts.
 Normally it will not be used when the client is a personal
 newsreading program.  In particular, this function will invoke the
 server's news posting program with the appropriate settings (flags,
 options, etc) to indicate that the forthcoming article is being
 forwarded from another host.

 The server may, however, elect not to post or forward the article if
 after further examination of the article it deems it inappropriate to
 do so.  The 436 or 437 error codes may be returned as appropriate to
 the situation.

 Reasons for such subsequent rejection of an article may include such
 problems as inappropriate newsgroups or distributions, disk space
 limitations, article lengths, garbled headers, and the like.  These
 are typically restrictions enforced by the server host's news
 software and not necessarily the NNTP server itself.

 Responses
 235 article transferred ok
 335 send article to be transferred.  End with <CR-LF>.<CR-LF>
 435 article not wanted - do not send it
 436 transfer failed - try again later
 437 article rejected - do not try again

 An implementation note:

 Because some host news posting software may not be able to decide
 immediately that an article is inappropriate for posting or
 forwarding, it is acceptable to acknowledge the successful transfer
 of the article and to later silently discard it.  Thus it is
 permitted to return the 235 acknowledgement code and later discard
 the received article.  This is not a fully satisfactory solution to
 the problem.  Perhaps some implementations will wish to send mail to
 the author of the article in certain of these cases.}
function TAlNNTPClient.IHave(const ArticleID: AnsiString; ArticleContent: AnsiString): Boolean;
Var LResponse: AnsiString;
    LStatusCode: integer;
    I: integer;
begin
  Result := True;
  LResponse := SendCmd('IHAVE' + ALNNTPClientEnclosedInAngleBrackets(ArticleID),[335,435], False);
  LStatusCode := GetStatusCodeFromResponse(LResponse);
  If LStatusCode = 435 then begin
    Result := False;
    exit;
  end;

  I := 2;
  while I <= Length(ArticleContent) Do begin
    If (ArticleContent[I] = '.') and (ArticleContent[I-1] = #10) and (ArticleContent[I-2] = #13) then Insert('.',ArticleContent,I);
    inc(I);
  end;

  SendCmd(ArticleContent + #13#10 + '.' + #13#10,[235], False);
end;

{***}
{POST

 If posting is allowed, response code 340 is returned to indicate that
 the article to be posted should be sent. Response code 440 indicates
 that posting is prohibited for some installation-dependent reason.

 If posting is permitted, the article should be presented in the
 format specified by RFC850, and should include all required header
 lines. After the article's header and body have been completely sent
 by the client to the server, a further response code will be returned
 to indicate success or failure of the posting attempt.

 The text forming the header and body of the message to be posted
 should be sent by the client using the conventions for text received
 from the news server:  A single period (".") on a line indicates the
 end of the text, with lines starting with a period in the original
 text having that period doubled during transmission.

 No attempt shall be made by the server to filter characters, fold or
 limit lines, or otherwise process incoming text.  It is our intent
 that the server just pass the incoming message to be posted to the
 server installation's news posting software, which is separate from
 this specification.  See RFC850 for more details.

 Since most installations will want the client news program to allow
 the user to prepare his message using some sort of text editor, and
 transmit it to the server for posting only after it is composed, the
 client program should take note of the herald message that greeted it
 when the connection was first established. This message indicates
 whether postings from that client are permitted or not, and can be
 used to caution the user that his access is read-only if that is the
 case. This will prevent the user from wasting a good deal of time
 composing a message only to find posting of the message was denied.
 The method and determination of which clients and hosts may post is
 installation dependent and is not covered by this specification.

 Responses
 240 article posted ok
 340 send article to be posted. End with <CR-LF>.<CR-LF>
 440 posting not allowed
 441 posting failed

 (for reference, one of the following codes will be sent upon initial
 connection; the client program should determine whether posting is
 generally permitted from these:) 200 server ready - posting allowed
 201 server ready - no posting allowed}
Procedure TAlNNTPClient.Post(ArticleContent: AnsiString);
Var I: integer;
begin
  SendCmd('POST',[340], False);

  I := 2;
  while I <= Length(ArticleContent) Do begin
    If (ArticleContent[I] = '.') and (ArticleContent[I-1] = #10) and (ArticleContent[I-2] = #13) then Insert('.',ArticleContent,I);
    inc(I);
  end;

  SendCmd(ArticleContent + #13#10 + '.' + #13#10,[240], False);
end;

{*************************************************************************}
procedure TAlNNTPClient.Post(const HeaderContent, BodyContent: AnsiString);
begin
  Post(ALTrim(HeaderContent) + #13#10#13#10 + BodyContent);
end;

{***********************************************************************************************}
procedure TAlNNTPClient.Post(HeaderContent: TALNewsArticleHeader; const BodyContent: AnsiString);
begin
  Post(HeaderContent.RawHeaderText, BodyContent);
end;

{*****************************************************************************}
procedure TAlNNTPClient.PostMultipartMixed(HeaderContent: TALNewsArticleHeader;
                                           const InlineText,
                                                 InlineTextContentType: AnsiString;
                                           Attachments: TALMultiPartMixedContents);
Var LMultipartMixedEncoder: TALMultipartMixedEncoder;
    Str: AnsiString;
begin
  LMultipartMixedEncoder := TALMultipartMixedEncoder.create;
  try
    LMultipartMixedEncoder.Encode(InlineText,
                                  InlineTextContentType,
                                  Attachments);
    with LMultipartMixedEncoder do begin
      HeaderContent.ContentType := 'multipart/mixed; boundary="' + DataStream.Boundary + '"';
      SetLength(Str,DataStream.size);
      DataStream.Position := 0;
      DataStream.ReadBuffer(pointer(str)^,DataStream.Size);
    end;
    Post(Headercontent.RawHeaderText, Str);
  finally
    LMultipartMixedEncoder.free;
  end;
end;

{*******************************************************************}
{Indicates to the server that this client connection is to a slave
 server, rather than a user.

 This command is intended for use in separating connections to single
 users from those to subsidiary ("slave") servers.  It may be used to
 indicate that priority should therefore be given to requests from
 this client, as it is presumably serving more than one person.  It
 might also be used to determine which connections to close when
 system load levels are exceeded, perhaps giving preference to slave
 servers.  The actual use this command is put to is entirely
 implementation dependent, and may vary from one host to another.  In
 NNTP servers which do not give priority to slave servers, this
 command must nonetheless be recognized and acknowledged.


 Responses
 202 slave status noted}
function TAlNNTPClient.Slave: AnsiString;
begin
  Result := SendCmd('SLAVE',[202], False);
end;

{*******************************************************************}
{The server process acknowledges the QUIT command and then closes the
 connection to the client.  This is the preferred method for a client
 to indicate that it has finished all its transactions with the NNTP
 server.

 If a client simply disconnects (or the connection times out, or some
 other fault occurs), the server should gracefully cease its attempts
 to service the client.


 Responses
 205 closing connection - goodbye!}
function TAlNNTPClient.Quit: AnsiString;
begin
  Result := SendCmd('QUIT',[205], False);
  Disconnect;
end;

{*************************************************************************************}
function TAlNNTPClient.GetStatusCodeFromResponse(const aResponse: AnsiString): Integer;

  {------------------------------------------------------}
  function Internalstpblk(PValue : PAnsiChar) : PAnsiChar;
  begin
    Result := PValue;
    while Result^ in [' ', #9, #10, #13] do Inc(Result);
  end;

  {-----------------------------------------------------------------------------}
  function InternalGetInteger(Data: PAnsiChar; var Number : Integer) : PAnsiChar;
  var bSign : Boolean;
  begin
    Number := 0;
    Result := InternalStpBlk(Data);
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

begin
  Result := 0;
  if aResponse='' then exit;
  InternalGetInteger(@aResponse[1], Result);
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
function TAlNNTPClient.SendCmd(aCmd: AnsiString;
                               const OkResponses: array of Word;
                               Const MultilineResponse: Boolean=False): AnsiString;
Var P: PAnsiChar;
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

  Result := GetResponse(OkResponses, MultilineResponse);
end;

{**********************************************}
{Responses are of two kinds, textual and status

 ///Text Responses///
 Text is sent only after a numeric status response line has been sent
 that indicates that text will follow.  Text is sent as a series of
 successive lines of textual matter, each terminated with CR-LF pair.
 A single line containing only a period (.) is sent to indicate the
 end of the text (i.e., the server will send a CR-LF pair at the end
 of the last line of text, a period, and another CR-LF pair).

 If the text contained a period as the first character of the text
 line in the original, that first period is doubled.  Therefore, the
 client must examine the first character of each line received, and
 for those beginning with a period, determine either that this is the
 end of the text or whether to collapse the doubled period to a single
 one.

 The intention is that text messages will usually be displayed on the
 user's terminal whereas command/status responses will be interpreted
 by the client program before any possible display is done.


 ///Status Responses///
 These are status reports from the server and indicate the response to
 the last command received from the client.

 Status response lines begin with a 3 digit numeric code which is
 sufficient to distinguish all responses.  Some of these may herald
 the subsequent transmission of text.

 The first digit of the response broadly indicates the success,
 failure, or progress of the previous command.

    1xx - Informative message
    2xx - Command ok
    3xx - Command ok so far, send the rest of it.
    4xx - Command was correct, but couldn't be performed for
          some reason.
    5xx - Command unimplemented, or incorrect, or a serious
          program error occurred.

 The next digit in the code indicates the function response category.

    x0x - Connection, setup, and miscellaneous messages
    x1x - Newsgroup selection
    x2x - Article selection
    x3x - Distribution functions
    x4x - Posting
    x8x - Nonstandard (private implementation) extensions
    x9x - Debugging output

 The exact response codes that should be expected from each command
 are detailed in the description of that command.  In addition, below
 is listed a general set of response codes that may be received at any
 time.

 Certain status responses contain parameters such as numbers and
 names. The number and type of such parameters is fixed for each
 response code to simplify interpretation of the response.

 Parameters are separated from the numeric response code and from each
 other by a single space. All numeric parameters are decimal, and may
 have leading zeros. All string parameters begin after the separating
 space, and end before the following separating space or the CR-LF
 pair at the end of the line. (String parameters may not, therefore,
 contain spaces.) All text, if any, in the response which is not a
 parameter of the response must follow and be separated from the last
 parameter by a space.  Also, note that the text following a response
 number may vary in different implementations of the server. The
 3-digit numeric code should be used to determine what response was
 sent.

 Response codes not specified in this standard may be used for any
 installation-specific additional commands also not specified. These
 should be chosen to fit the pattern of x8x specified above.  (Note
 that debugging is provided for explicitly in the x9x response codes.)
 The use of unspecified response codes for standard commands is
 prohibited.

 We have provided a response pattern x9x for debugging.  Since much
 debugging output may be classed as "informative messages", we would
 expect, therefore, that responses 190 through 199 would be used for
 various debugging outputs.  There is no requirement in this
 specification for debugging output, but if such is provided over the
 connected stream, it must use these response codes.  If appropriate
 to a specific implementation, other x9x codes may be used for
 debugging.  (An example might be to use e.g., 290 to acknowledge a
 remote debugging request.)}
function TAlNNTPClient.GetResponse(const OkResponses: array of Word; Const MultilineResponse: Boolean=False): AnsiString;
Var LBuffStr: AnsiString;
    LBuffStrLength: Integer;
    LResponseLength: Integer;
    LStatusCode: Integer;
    LGoodResponse: integer;
    I: integer;
begin
  {Read the response from the socket - end of the Multiline response is show by <CRLF>.<CRLF> and
   end of single line response is show by <CRLF>}
  Result := '';
  Setlength(LBuffStr,512);
  LGoodResponse := -1;
  While True do begin
    LBuffStrLength := SocketRead(LBuffStr[1], length(LBuffStr));
    If LBuffStrLength <= 0 then raise Exception.Create('Connection close gracefully!');
    Result := Result + AlCopyStr(LBuffStr,1,LBuffStrLength);
    LResponseLength := length(Result);

    If LGoodResponse = -1 then begin
      LStatusCode := GetStatusCodeFromResponse(Result);
      LGoodResponse := 0;
      for I := 0 to High(OkResponses) do
        if OkResponses[I] = LStatusCode then begin
          LGoodResponse := 1;
          Break;
        end;
    end;

    If (LGoodResponse = 0) or (not MultilineResponse) then begin
      If (LResponseLength > 1) and
         (Result[LResponseLength] = #10) and
         (Result[LResponseLength - 1] = #13) then Break;
    end
    else begin
      If (LResponseLength > 4) and
         (Result[LResponseLength] = #10) and
         (Result[LResponseLength - 1] = #13) and
         (Result[LResponseLength - 2] = '.') and
         (Result[LResponseLength - 3] = #10) and
         (Result[LResponseLength - 4] = #13) then Break;
    end;
  end;

  If LGoodResponse <> 1 then Raise Exception.Create(String(Result));
end;

{*******************************************************************}
Function TAlNNTPClient.SocketWrite(const Buf; len: Integer): Integer;
begin
  Result := Send(FSocketDescriptor,Buf,len,0);
  CheckError(Result =  SOCKET_ERROR);
end;

{****************************************************************}
function TAlNNTPClient.SocketRead(var buf; len: Integer): Integer;
begin
  Result := Recv(FSocketDescriptor,Buf,len,0);
  CheckError(Result = SOCKET_ERROR);
end;

{***********************************************************}
procedure TAlNNTPClient.SetSendTimeout(const Value: integer);
begin
  FSendTimeout := Value;
  if FConnected then CheckError(setsockopt(FSocketDescriptor,SOL_SOCKET,SO_SNDTIMEO,PAnsiChar(@FSendTimeout),SizeOf(FSendTimeout))=SOCKET_ERROR);
end;

{**************************************************************}
procedure TAlNNTPClient.SetReceiveTimeout(const Value: integer);
begin
  FReceiveTimeout := Value;
  if FConnected then CheckError(setsockopt(FSocketDescriptor,SOL_SOCKET,SO_RCVTIMEO,PAnsiChar(@FReceiveTimeout),SizeOf(FReceiveTimeout))=SOCKET_ERROR);
end;

{*******************************************************************************************************************}
// http://blogs.technet.com/b/nettracer/archive/2010/06/03/things-that-you-may-want-to-know-about-tcp-keepalives.aspx
procedure TAlNNTPClient.SetKeepAlive(const Value: boolean);
var LIntBool: integer;
begin
  FKeepAlive := Value;
  if FConnected then begin
    // warning the winsock seam buggy because the getSockOpt return optlen = 1 (byte) intead of 4 (dword)
    // so the getSockOpt work only if aIntBool = byte ! (i see this on windows vista)
    // but this is only for getSockOpt, for setsockopt it's seam to work OK so i leave it like this
    if FKeepAlive then LIntBool := 1
    else LIntBool := 0;
    CheckError(setsockopt(FSocketDescriptor,SOL_SOCKET,SO_KEEPALIVE,PAnsiChar(@LIntBool),SizeOf(LIntBool))=SOCKET_ERROR);
  end;
end;

{***************************************************************************************************************************************************************************************************************}
// https://access.redhat.com/site/documentation/en-US/Red_Hat_Enterprise_MRG/1.1/html/Realtime_Tuning_Guide/sect-Realtime_Tuning_Guide-Application_Tuning_and_Deployment-TCP_NODELAY_and_Small_Buffer_Writes.html
procedure TAlNNTPClient.SetTCPNoDelay(const Value: boolean);
var LIntBool: integer;
begin
  fTCPNoDelay := Value;
  if FConnected then begin
    // warning the winsock seam buggy because the getSockOpt return optlen = 1 (byte) intead of 4 (dword)
    // so the getSockOpt work only if aIntBool = byte ! (i see this on windows vista)
    // but this is only for getSockOpt, for setsockopt it's seam to work OK so i leave it like this
    if fTCPNoDelay then LIntBool := 1
    else LIntBool := 0;
    CheckError(setsockopt(FSocketDescriptor,SOL_SOCKET,TCP_NODELAY,PAnsiChar(@LIntBool),SizeOf(LIntBool))=SOCKET_ERROR);
  end;
end;

end.
