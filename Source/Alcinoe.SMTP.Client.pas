unit Alcinoe.SMTP.Client;

interface

{$I Alcinoe.inc}

Uses
  Alcinoe.Net.TLS.Client;

type

    {----------------------------}
    TAlSmtpClient = class(TObject)
    public
      type
        // ---------
        // TAuthType
        TAuthType = (
          Plain,
          Login);
        // ----------
        // TAuthTypes
        TAuthTypes = set of TAuthType;
    Private
      FTSLClient: TALTlsClient;
      FAuthTypesSupported: TAuthTypes;
      function GetSendTimeout: integer;
      procedure SetSendTimeout(const Value: integer);
      function GetReceiveTimeout: integer;
      procedure SetReceiveTimeout(const Value: integer);
      function GetKeepAlive: boolean;
      procedure SetKeepAlive(const Value: boolean);
      function GetTCPNoDelay: boolean;
      procedure SetTCPNoDelay(const Value: boolean);
      function GetConnected: Boolean;
      Function GetAuthTypeFromEhloResponse(const EhloResponse: AnsiString): TAuthTypes;
    protected
      Function SendCmd(const ACmd:AnsiString; const OkResponses: array of Word): AnsiString; virtual;
      Function GetResponse(const OkResponses: array of Word): AnsiString;
    public
      constructor Create; virtual;
      destructor Destroy; override;
      Function Connect(const AHost: AnsiString; const APort: integer; const AStartTls: Boolean = true): AnsiString; virtual;
      Function Helo: AnsiString; virtual;
      Function Ehlo: AnsiString; virtual;
      Function StartTls: AnsiString; virtual;
      Function Auth(const AUserName, APassword: AnsiString; const AAuthType: TAuthType): AnsiString; overload; virtual;
      Function Auth(const AUserName, APassword: AnsiString): AnsiString; overload; virtual;
      Function Vrfy(const aUserName: AnsiString): AnsiString; virtual;
      Function MailFrom(const ASenderEmail: AnsiString): AnsiString; virtual;
      Function RcptTo(const ARcptNames: TArray<AnsiString>): AnsiString; virtual;
      Function Data(const AData: AnsiString): AnsiString; overload; virtual;
      Function Data(const AHeader, ABody: AnsiString): AnsiString; overload; virtual;
      Function Quit: AnsiString; virtual;
      Function Rset: AnsiString; virtual;
      procedure SendMail(
                  const AHost: AnsiString;
                  const APort: integer;
                  const ASenderEmail: AnsiString;
                  const ARcptNames: TArray<AnsiString>;
                  const AUserName, APassword: AnsiString;
                  const AData: AnsiString); overload; virtual;
      procedure SendMail(
                  const AHost: AnsiString;
                  const APort: integer;
                  const ASenderEmail: AnsiString;
                  const ARcptNames: TArray<AnsiString>;
                  const AUserName, APassword: AnsiString;
                  const AHeader, ABody: AnsiString); overload; virtual;
      Procedure Disconnect; virtual;
      property Connected: Boolean read GetConnected;
      property SendTimeout: Integer read GetSendTimeout write SetSendTimeout;
      property ReceiveTimeout: Integer read GetReceiveTimeout write SetReceiveTimeout;
      property KeepAlive: Boolean read GetKeepAlive write SetKeepAlive;
      property TcpNoDelay: Boolean read GetTCPNoDelay write SetTCPNoDelay;
    end;

implementation

Uses
  System.SysUtils,
  System.AnsiStrings,
  Alcinoe.Net,
  Alcinoe.StringList,
  Alcinoe.Net.TLS.Client.SChannel,
  Alcinoe.Common,
  Alcinoe.StringUtils;

{*******************************}
constructor TAlSmtpClient.Create;
begin
  inherited;
  FTSLClient := TAlSChannelTlsClient.create;
  FTSLClient.SendTimeout := 60_000;
  FTSLClient.ReceiveTimeout := 60_000;
  FTSLClient.KeepAlive := True;
  FTSLClient.TCPNoDelay := True;
  FAuthTypesSupported:= [];
end;

{*******************************}
destructor TAlSmtpClient.Destroy;
begin
  Disconnect;
  ALFreeAndNil(FTSLClient);
  inherited;
end;

{*************************************************************************************************************************}
Function TAlSmtpClient.Connect(const AHost: AnsiString; const APort: integer; const AStartTls: Boolean = true): AnsiString;
begin
  FTSLClient.Connect(String(AHost), aPort, AStartTls);
  Try
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
  FTSLClient.Disconnect;
  FAuthTypesSupported := [];
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
Function TAlSmtpClient.GetAuthTypeFromEhloResponse(const EhloResponse: AnsiString): TAuthTypes;
begin
  Result := [];
  var LEhloResponse := AlUpperCase(EhloResponse);
  LEhloResponse := ALStringReplaceA(LEhloResponse, '=', ' ', [rfReplaceAll]);
  LEhloResponse := ALStringReplaceA(LEhloResponse, '250 ', '250-', [rfReplaceAll]);
  var LLines := TALStringListA.Create;
  Try
    LLines.Text := LEhloResponse;
    For var I := 0 to LLines.Count - 1 do begin
      var LLine := LLines[i];
      if AlposA('250-AUTH ', LLine) = 1 then begin
        var LAuthTypes := TALStringListA.Create;
        try
          LAuthTypes.LineBreak := ' ';
          LAuthTypes.Text := ALCopystr(LLine, 10{length('250-AUTH ')+1}, MaxInt);
          for var J := 0 to LAuthTypes.Count - 1 do begin
            var LAuthType := LAuthTypes[J];
            if LAuthType = 'PLAIN' then result := result + [TAuthType.Plain]
            else if LAuthType = 'LOGIN' then result := result + [TAuthType.Login];
          end;
        finally
          ALFreeAndNil(LAuthTypes);
        end;
      end;
    end;
  Finally
    ALFreeAndNil(LLines);
  End;
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
  Result := SendCmd('HELO '+ALGetLocalDnsIdentityA, [250]);
end;

{**************************************}
Function TAlSmtpClient.Ehlo: AnsiString;
begin
  result := SendCmd('EHLO '+ALGetLocalDnsIdentityA, [250]);
  FAuthTypesSupported := GetAuthTypeFromEhloResponse(Result);
end;

{******************************************}
Function TAlSmtpClient.StartTls: AnsiString;
begin
  Result := SendCmd('STARTTLS', [220]);
  FTSLClient.StartTls;
end;

{**********************************************************************************************************}
Function TAlSmtpClient.Auth(const AUserName, APassword: AnsiString; const AAuthType: TAuthType): AnsiString;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  Function _DoAuthPlain: AnsiString;
  begin
    If aUserName='' then raise EALException.Create('Username is empty');
    If aPassword='' then raise EALException.Create('Password is empty');
    var LAuthPlain: AnsiString := ALBase64EncodeString(aUserName + #0 + aUserName + #0 + aPassword);
    Result := SendCmd('AUTH PLAIN ' + LAuthPlain, [235]);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  Function _DoAuthLogin: AnsiString;
  begin
    If aUserName='' then raise EALException.Create('Username is empty');
    If aPassword='' then raise EALException.Create('Password is empty');
    SendCmd('AUTH LOGIN', [334]);
    SendCmd(ALBase64EncodeString(aUsername), [334]);
    Result := SendCmd(ALBase64EncodeString(aPassword), [235]);
  end;

begin
  case AAuthType of
    TAuthType.Plain : Result := _DoAuthPlain;
    TAuthType.Login : result := _DoAuthLogin;
    else raise EALException.Create('Error 78930E83-63E5-4CB2-9C20-1B37B46E3EE4');
  end;
end;

{******************************************************************************}
Function TAlSmtpClient.Auth(const AUserName, APassword: AnsiString): AnsiString;
begin
  var LAuthType: TAuthType;
  if FAuthTypesSupported = [] then Ehlo;
  if TAuthType.Plain in FAuthTypesSupported then LAuthType := TAuthType.Plain
  else if TAuthType.Login in FAuthTypesSupported then LAuthType := TAuthType.Login
  else raise EALException.Create('No supported authentication scheme found');
  result := Auth(AUserName, APassword, LAuthType);
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
Function TAlSmtpClient.MailFrom(const ASenderEmail: AnsiString): AnsiString;
begin
  If ASenderEmail = '' then raise EALException.Create('Sender email address is empty');
  If ALPosA(#13#10, ASenderEmail) > 0 then raise EALException.Create('Sender email address is invalid');
  Result := SendCmd('MAIL From:<'+ASenderEmail+'>', [250]);
end;

{*************************************************************************}
{This command is used to identify an individual recipient of the mail data;
 multiple recipients are specified by multiple use of this command.}
Function TAlSmtpClient.RcptTo(const ARcptNames: TArray<AnsiString>): AnsiString;
begin
  Result := '';
  if length(ARcptNames) <= 0 then raise EALException.Create('Recipient list is empty');
  For var I := low(ARcptNames) to high(ARcptNames) do begin
    var LRcptName := ARcptNames[I];
    If LRcptName = '' then raise EALException.Create('Recipient email address is empty');
    If (ALPosA(#13#10, LRcptName) > 0) then raise EALException.Create('Recipient email address is invalid');
    Result := Result + SendCmd('RCPT To:<'+LRcptName+'>', [250, 251]);
  end;
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
Function TAlSmtpClient.Data(const AData: AnsiString): AnsiString;
begin
  SendCmd('DATA', [354]);
  var LData := AData;
  LData := ALStringReplaceA(LData, #13#10'.', #13#10'..', [RFReplaceAll]);
  Result := SendCmd(LData + #13#10'.'#13#10, [250]);
end;

{************************************************************************}
Function TAlSmtpClient.Data(const AHeader, ABody: AnsiString): AnsiString;
begin
  result := Data(ALTrim(AHeader) + #13#10#13#10 + ABody);
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
  Result := SendCmd('QUIT', [221]);
  Disconnect;
end;

{*****************************************************************************}
{This command asks the receiver to confirm that the argument identifies a user.
 If it is a user name, the full name of the user (if known) and the fully
 specified mailbox are returned. This command has no effect on any of the
 reverse-path buffer, the forward-path buffer, or the mail data buffer.}
Function TAlSmtpClient.Vrfy(const aUserName: AnsiString): AnsiString;
begin
  Result := SendCmd('VRFY ' + aUserName, [250]);
end;

{*************************************************************}
{This command specifies that the current mail transaction is to
 be aborted. Any stored sender, recipients, and mail data must be
 discarded, and all buffers and state tables cleared. The receiver
 must send an OK reply.}
Function TAlSmtpClient.Rset: AnsiString;
begin
  Result := SendCmd('RSET', [250]);
end;

{*******************************}
procedure TAlSmtpClient.SendMail(
            const AHost: AnsiString;
            const APort: integer;
            const ASenderEmail: AnsiString;
            const ARcptNames: TArray<AnsiString>;
            const AUserName, APassword: AnsiString;
            const AData: AnsiString);
begin
  If Connected then Disconnect;
  Connect(AHost,APort);
  Try

    If (AUserName <> '') or (APassword <> '') then
      Auth(AUserName, APassword);
    mailFrom(ASenderEmail);
    RcptTo(ARcptNames);
    Data(AData);
    Quit;

  Finally
    Disconnect;
  end;
end;

{*******************************}
procedure TAlSmtpClient.SendMail(
            const AHost: AnsiString;
            const APort: integer;
            const ASenderEmail: AnsiString;
            const ARcptNames: TArray<AnsiString>;
            const AUserName, APassword: AnsiString;
            const AHeader, ABody: AnsiString);
begin
  If Connected then Disconnect;
  Connect(AHost,APort);
  Try

    If (AUserName <> '') or (APassword <> '') then
      Auth(AUserName, APassword);
    mailFrom(ASenderEmail);
    RcptTo(ARcptNames);
    Data(AHeader, ABody);
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
function TAlSmtpClient.SendCmd(const ACmd: AnsiString; const OkResponses: array of Word): AnsiString;
begin
  var LCmd := ACmd;
  If (length(LCmd) <= 1) or
     (LCmd[high(LCmd)] <> #10) or
     (LCmd[high(LCmd) - 1] <> #13)
  then LCmd := LCmd + #13#10;

  FTSLClient.SendAll(LCmd[low(LCmd)], length(LCmd));

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

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _stpblk(PValue: PAnsiChar): PAnsiChar;
  begin
    Result := PValue;
    while Result^ in [' ', #9, #10, #13] do Inc(Result);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _GetInteger(Data: PAnsiChar; var Number: Integer): PAnsiChar;
  begin
    Number := 0;
    Result := _StpBlk(Data);
    if (Result = nil) then Exit;
    { Remember the sign }
    var bSign : Boolean;
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
  Result := '';
  Var LBuffStr: AnsiString;
  // The maximum total length of a reply line including the reply code and
  // the <CRLF> is 512 characters. (http://www.freesoft.org/CIE/RFC/821/24.htm)
  Setlength(LBuffStr, 512);
  While true do begin

    {Read the response from the socket - end of the response is show by <CRLF>}
    var LResponse: AnsiString := '';
    While True do begin
      var LBuffStrLn: Integer := FTSLClient.Receive(Pointer(LBuffStr)^, length(LBuffStr));
      If LBuffStrLn <= 0 then raise EALException.Create('Connection close gracefully!');
      LResponse := LResponse + AlCopyStr(LBuffStr, 1, LBuffStrLn);
      If (length(LResponse) > 1) and
         (LResponse[High(LResponse)] = #10) and
         (LResponse[High(LResponse) - 1] = #13) then Break;
    end;
    Result := Result + LResponse;

    {The format for multiline replies requires that every line, except the last,
     begin with the reply code, followed immediately by a hyphen, "-" (also known as minus),
     followed by text. The last line will begin with the reply code, followed immediately
     by <SP>, optionally some text, and <CRLF>.}
    var LLst := TALStringListA.create;
    Try
      LLst.Text := LResponse;
      If LLst.count = 0 then raise EALException.Create('Emtpy response');
      For var j := 0 to LLst.count - 1 do begin

        var LStatusCode: Integer;
        var P: PAnsiChar := _GetInteger(PAnsiChar(LLst[j]), LStatusCode);
        var LGoodResponse := False;
        for var I := Low(OkResponses) to High(OkResponses) do
          if OkResponses[I] = LStatusCode then begin
            LGoodResponse := True;
            Break;
          end;

        If not LGoodResponse then Raise EALException.Create(LResponse);
        if p^ <> '-' then Begin
          If J <> LLst.count - 1 then Raise EALException.Create(LResponse);
          Exit;
        end;

      end;
    Finally
      LLst.Free;
    end;

  end;
end;

{*********************************************}
function TAlSmtpClient.GetSendTimeout: integer;
begin
  Result := FTSLClient.SendTimeout;
end;

{***********************************************************}
procedure TAlSmtpClient.SetSendTimeout(const Value: integer);
begin
  FTSLClient.SendTimeout := Value;
end;

{**************************************************************}
function TAlSmtpClient.GetReceiveTimeout: integer;
begin
  Result := FTSLClient.ReceiveTimeout;
end;

{**************************************************************}
procedure TAlSmtpClient.SetReceiveTimeout(const Value: integer);
begin
  FTSLClient.ReceiveTimeout := Value;
end;

{*********************************************************}
function TAlSmtpClient.GetKeepAlive: boolean;
begin
  Result := FTSLClient.KeepAlive;
end;

{*********************************************************}
procedure TAlSmtpClient.SetKeepAlive(const Value: boolean);
begin
  FTSLClient.KeepAlive := Value;
end;

{**********************************************************}
function TAlSmtpClient.GetTCPNoDelay: boolean;
begin
  Result := FTSLClient.TCPNoDelay;
end;

{**********************************************************}
procedure TAlSmtpClient.SetTCPNoDelay(const Value: boolean);
begin
  FTSLClient.TCPNoDelay := Value;
end;

{*******************************************}
function TAlSmtpClient.GetConnected: Boolean;
begin
  Result := FTSLClient.Connected;
end;

end.