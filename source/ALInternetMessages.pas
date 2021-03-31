unit ALInternetMessages;

interface

{$I Alcinoe.inc}

{$IFNDEF ALHideAnsiString}
uses
  system.Classes,
  AlStringList;
{$ENDIF}

{$IFNDEF ALHideAnsiString}

Type

  {-----------------------------}
  TALEMailHeader = Class(Tobject)
  Private
    fSendTo: AnsiString;
    fSender: AnsiString;
    fMessageID: AnsiString;
    fbcc: AnsiString;
    fContentTransferEncoding: AnsiString;
    fComments: AnsiString;
    fMIMEVersion: AnsiString;
    fPriority: AnsiString;
    fReplyTo: AnsiString;
    fSubject: AnsiString;
    fFrom: AnsiString;
    fDate: AnsiString;
    fDispositionNotificationTo: AnsiString;
    fReferences: AnsiString;
    fcc: AnsiString;
    fContentType: AnsiString;
    FCustomHeaders: TALStrings;
    Function GetRawHeaderText: AnsiString;
    procedure SetRawHeaderText(const aRawHeaderText: AnsiString);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;
    property From: AnsiString read fFrom write fFrom; {From: John Doe <jdoe@machine.example> - Author(s) or person(s) taking responsibility for the message 4.4.1; RFC 1123: 5.2.15-16, 5.3.7; RFC 1036: 2.1.1}
    property Sender: AnsiString read fSender write fSender; {Sender: Michael Jones <mjones@machine.example> - The person or agent submitting the message to the network, if other than shown by the From header RFC 822: 4.4.2; RFC 1123: 5.2.15-16, 5.3.7; RFC 1036: 2.1.1}
    property SendTo: AnsiString read fSendTo write fSendTo; {To: Mary Smith <mary@example.net> - Primary recipient(s) RFC 822: 4.5.1; RFC 1123: 5.2.15-16, 5.3.7;}
    property cc: AnsiString read fcc write fcc; {cc: <boss@nil.test>, "Giant; \"Big\" Box" <sysservices@example.net> - Secondary, informational recipient(s) RFC 822: 4.5.2; RFC 1123: 5.2.15-16, 5.3.7;}
    property bcc: AnsiString read fbcc write fbcc; {bcc: <boss@nil.test>, "Giant; \"Big\" Box" <sysservices@example.net> - Recipient(s) not to be disclosed to other recipients ("blind carbon copy") RFC 822: 4.5.3; RFC 1123: 5.2.15-16, 5.3.7;}
    property ReplyTo: AnsiString read fReplyTo write fReplyTo; {Reply-To: "Mary Smith: Personal Account" <smith@home.example> - Suggested E-mail address for replies RFC 822: 4.4.3; RFC 1036: 2.2.1}
    property Subject: AnsiString read fSubject write fSubject; {Subject: Saying Hello - Text that provides a summary, or indicates the nature, of the message RFC 822: 4.7.1; RFC 1036: 2.1.4}
    property MessageID: AnsiString read fMessageID write fMessageID; {Message-ID: <1234@local.machine.example> -	Unique ID for the message RFC 822: 4.6.1; RFC 1036: 2.1.5}
    property References: AnsiString read fReferences write fReferences; {References: <1234@local.machine.example> <3456@example.net> - In E-mail: reference to other related messages; in Usenet: reference to replied-to-articles RFC 822: 4.6.3; RFC 1036: 2.2.5}
    property Comments: AnsiString read fComments write fComments; {Comments: Authenticated sender is gboyd@netcom.com - Text comments added to the message RFC 822: 4.7.2}
    property Date: AnsiString read fDate write fDate; {Date: Fri, 21 Nov 1997 09:55:06 -0600 - The time when the message was written (or submitted) RFC 822: 5.1; RFC 1123: 5.2.14; RFC 1036: 2.1.2}
    property ContentType: AnsiString read fContentType write fContentType; {Content-Type: text/plain; charset="iso-8859-1" - Data type and format of content RFC 1049 (historic); RFC 1123: 5.2.13; RFC 2045: 5; RFC 1766: 4.1}
    property ContentTransferEncoding: AnsiString read fContentTransferEncoding write fContentTransferEncoding; {Content-Transfer-Encoding: 8bit - Coding method used in a MIME message body RFC 2045: 6;}
    property MIMEVersion: AnsiString read fMIMEVersion write fMIMEVersion; {MIME-Version: 1.0 - specifies the version of MIME that the message format complies with RFC 2045: 4}
    property Priority: AnsiString read fPriority write fPriority; {Priority: normal - Priority for message delivery ("normal" / "non-urgent" / "urgent") RFC 2156}
    property DispositionNotificationTo: AnsiString read fDispositionNotificationTo write fDispositionNotificationTo; {Disposition-Notification-To: boss@nil.test - Requests for notification when the message is received, and specifies the address for them RFC 2298}
    property CustomHeaders: TALStrings read FCustomHeaders;
    Property RawHeaderText: AnsiString read GetRawHeaderText write SetRawHeaderText;
  end;

  {-----------------------------------}
  TALNewsArticleHeader = Class(Tobject)
  Private
    FCustomHeaders: TALStrings;
    fExpires: AnsiString;
    fMessageID: AnsiString;
    fReplyTo: AnsiString;
    fOrganization: AnsiString;
    fDateReceived: AnsiString;
    fNNTPPostingHost: AnsiString;
    fContentTransferEncoding: AnsiString;
    fComments: AnsiString;
    fMIMEVersion: AnsiString;
    fSender: AnsiString;
    fNewsgroups: AnsiString;
    fReferences: AnsiString;
    fPostingVersion: AnsiString;
    fRelayVersion: AnsiString;
    fDate: AnsiString;
    fNNTPPostingDate: AnsiString;
    fPath: AnsiString;
    fDistribution: AnsiString;
    fContentType: AnsiString;
    fFollowupTo: AnsiString;
    fSubject: AnsiString;
    fControl: AnsiString;
    fFrom: AnsiString;
    Function GetRawHeaderText: AnsiString;
    procedure SetRawHeaderText(const aRawHeaderText: AnsiString);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;
    property RelayVersion: AnsiString read fRelayVersion write fRelayVersion; {Relay-Version: Version 1.7 PSU-NETNEWS 5/20/88; site MAINE.BITNET}
    property PostingVersion: AnsiString read fPostingVersion write fPostingVersion; {Posting-Version: Version 1.7 PSU-NETNEWS 5/20/88; site MAINE.BITNET}
    property From: AnsiString read fFrom write fFrom; {From: John Doe <jdoe@machine.example> - Author(s) or person(s) taking responsibility for the message 4.4.1; RFC 1123: 5.2.15-16, 5.3.7; RFC 1036: 2.1.1}
    property Date: AnsiString read fDate write fDate; {Date: Fri, 21 Nov 1997 09:55:06 -0600 - The time when the message was written (or submitted) RFC 822: 5.1; RFC 1123: 5.2.14; RFC 1036: 2.1.2}
    property Newsgroups: AnsiString read fNewsgroups write fNewsgroups; {Newsgroups: net.sport.football}
    property Subject: AnsiString read fSubject write fSubject; {Subject: Saying Hello - Text that provides a summary, or indicates the nature, of the message RFC 822: 4.7.1; RFC 1036: 2.1.4}
    property MessageID: AnsiString read fMessageID write fMessageID; {Message-ID: <1234@local.machine.example> -	Unique ID for the message RFC 822: 4.6.1; RFC 1036: 2.1.5}
    property Path: AnsiString read fPath write fPath; {Path: psuvm!cunyvm!maine.bitnet!michael}
    property ReplyTo: AnsiString read fReplyTo write fReplyTo; {Reply-To: "Mary Smith: Personal Account" <smith@home.example> - Suggested E-mail address for replies RFC 822: 4.4.3; RFC 1036: 2.2.1}
    property Sender: AnsiString read fSender write fSender; {Sender: Michael Jones <mjones@machine.example> - The person or agent submitting the message to the network, if other than shown by the From header RFC 822: 4.4.2; RFC 1123: 5.2.15-16, 5.3.7; RFC 1036: 2.1.1}
    property FollowupTo: AnsiString read fFollowupTo write fFollowupTo; {Followup-To: uk.legal,uk.misc}
    property DateReceived: AnsiString read fDateReceived write fDateReceived; {Date-Received: Fri, 21 Nov 1997 09:55:06 -0600}
    property Expires: AnsiString read fExpires write fExpires; {Expires: Fri, 21 Nov 1997 09:55:06 -0600}
    property References: AnsiString read fReferences write fReferences; {References: <1234@local.machine.example> <3456@example.net> - In E-mail: reference to other related messages; in Usenet: reference to replied-to-articles RFC 822: 4.6.3; RFC 1036: 2.2.5}
    property Control: AnsiString read fControl write fControl; {Control: cancel <xb8700A@twits.site.com>}
    property Distribution: AnsiString read fDistribution write fDistribution; {Distribution: nj.all}
    property Organization: AnsiString read fOrganization write fOrganization; {Organization: A poorly-installed InterNetNews site}
    property Comments: AnsiString read fComments write fComments; {Comments: Authenticated sender is gboyd@netcom.com - Text comments added to the message RFC 822: 4.7.2}
    property ContentType: AnsiString read fContentType write fContentType; {Content-Type: text/plain; charset="iso-8859-1" - Data type and format of content RFC 1049 (historic); RFC 1123: 5.2.13; RFC 2045: 5; RFC 1766: 4.1}
    property ContentTransferEncoding: AnsiString read fContentTransferEncoding write fContentTransferEncoding; {Content-Transfer-Encoding: 8bit - Coding method used in a MIME message body RFC 2045: 6;}
    property MIMEVersion: AnsiString read fMIMEVersion write fMIMEVersion; {MIME-Version: 1.0 - specifies the version of MIME that the message format complies with RFC 2045: 4}
    property NNTPPostingHost: AnsiString read fNNTPPostingHost write fNNTPPostingHost; {NNTP-Posting-Host: stc92-3-82-245-250-13.fbx.proxad.net}
    property NNTPPostingDate: AnsiString read fNNTPPostingDate write fNNTPPostingDate; {NNTP-Posting-Date: Sun, 30 Sep 2007 20:28:56 +0000 (UTC)}
    property CustomHeaders: TALStrings read FCustomHeaders;
    Property RawHeaderText: AnsiString read GetRawHeaderText write SetRawHeaderText;
  end;

function AlParseEmailAddress(FriendlyEmail: AnsiString; var RealName : AnsiString; Const decodeRealName: Boolean=True): AnsiString;
function AlExtractEmailAddress(const FriendlyEmail: AnsiString): AnsiString;
Function ALMakeFriendlyEmailAddress(const aRealName, aEmail: AnsiString): AnsiString;
Function ALEncodeRealName4FriendlyEmailAddress(const aRealName: AnsiString): AnsiString;
Function AlGenerateInternetMessageID: AnsiString; overload;
Function AlGenerateInternetMessageID(const ahostname: AnsiString): AnsiString; overload;
function AlIsValidEmail(const Value: AnsiString): boolean;

{$ENDIF !ALHideAnsiString}

function AlIsValidEmailU(const Value: String): boolean;

implementation

uses
  system.Sysutils,
  {$IFNDEF ALHideAnsiString}
  ALHttpClient,
  ALWinsock,
  {$ENDIF}
  ALCommon,
  ALString;

{$IFNDEF ALHideAnsiString}

{***************************************************************************}
{ FriendlyEmail                  RealName       Result                      }
{ ----------------------------   ------------   --------------              }
{ "my name" <name@domain.com>    my name        name@domain.com             }
{ myname <name@domain.com>       myname         name@domain.com             }
{ name@domain.com                empty          name@domain.com             }
{ <name@domain.com>              empty          name@domain.com             }
{                                                                           }
{ name@domain.com (myname)       myname         name@domain.com             }
{ (myname) name@domain.com       myname         name@domain.com             }
{ <name@domain.com> "my name"    my name        name@domain.com             }
function AlParseEmailAddress(FriendlyEmail: AnsiString; var RealName : AnsiString; Const decodeRealName: Boolean=True): AnsiString;

var P1, P2, P3, P4, P5: integer;
    S1: AnsiString;
    ln: integer;

begin

  //----------
  FriendlyEmail := ALTrim(FriendlyEmail);

  //----------
  Result := '';
  RealName := '';

  //----------
  ln := Length(FriendlyEmail);

  //----------
  if ALMatchesMask(FriendlyEmail, '* <*>') then P1 := Ln-1  // toto <toto@toto.com> | "toto" <toto@toto.com> | (toto) <toto@toto.com>
  else if ALMatchesMask(FriendlyEmail, '<*> *') then P1 := 2 // <toto@toto.com> toto.com | <toto@toto.com> "toto" | <toto@toto.com> (toto)
  else if ALMatchesMask(FriendlyEmail, '<*>') then P1 := 2 // <toto@toto.com>
  else if ALMatchesMask(FriendlyEmail, '* (*)') then P1 := 1 // toto@toto.com (toto)
  else Begin
    RealName := '';
    Result := FriendlyEmail;
    Exit;
  end;

  //----------
  Result := FriendlyEmail[P1];

  //----------
  P2 := P1-1;
  While (P2 > 0) and (FriendlyEmail[P2] in ['a'..'z','A'..'Z','0'..'9','_','-','.','@']) do begin
    Result := FriendlyEmail[P2] + Result;
    dec(P2);
  end;
  While (P2 > 0) and (FriendlyEmail[P2] in [' ',#9]) do dec(P2);

  P3 := P1+1;
  While (P3 <= Ln) and (FriendlyEmail[P3] in ['a'..'z','A'..'Z','0'..'9','_','-','.','@']) do begin
    Result := Result + FriendlyEmail[P3];
    inc(P3);
  end;
  While (P3 <= Ln) and (FriendlyEmail[P3] in [' ',#9]) do inc(P3);

  //----------
  If (P2 > 0) and
     (P3 <= Ln) then begin
    If (((FriendlyEmail[P2] = '<') and (FriendlyEmail[P3] = '>'))) then begin
      Dec(P2);
      Inc(P3);
    end
    else begin
      S1 := Result;

      P4 := P2;
      While (P4 > 0) and (FriendlyEmail[P4] <> '<') do
        if (FriendlyEmail[P4] = '>') then begin
          P4 := 0;
          Break;
        end
        else begin
          S1 := FriendlyEmail[P4] + S1;
          dec(P4);
        end;

      P5 := P3;
      While (P5 <= Ln) and (FriendlyEmail[P5] <> '>') do
        if (FriendlyEmail[P5] = '<') then begin
          P5 := LN + 1;
          Break;
        end
        else begin
          S1 := S1 + FriendlyEmail[P5];
          inc(P5);
        end;

      If (P4 > 0) and
         (P5 <= Ln) then begin
        P2 := P4-1;
        P3 := P5+1;
        Result := S1;
      end;
    end;
  end;

  //----------
  RealName := ALTrim(AlCopyStr(FriendlyEmail,1,P2) + AlCopyStr(FriendlyEmail,P3,Maxint));

  //----------
  If (RealName = '') then begin
    If (result <> '') then exit;
    RealName := ALTrim(FriendlyEmail);
  end;

  //----------
  ln := Length(RealName);
  if (decodeRealName) and
     (ln >= 2) and
     (RealName[1] = '"') and
     (RealName[ln] = '"') then begin
   RealName := alCopyStr(RealName,2,Ln-2);
   RealName := alStringReplace(RealName,'\\',#1,[rfIgnoreCase,RfReplaceAll]);
   RealName := alStringReplace(RealName,'\','',[rfIgnoreCase,RfReplaceAll]);
   RealName := alStringReplace(RealName,#1,'\',[rfIgnoreCase,RfReplaceAll]);
  end
  else if (ln >= 2) and
          (((RealName[1] = '(') and (RealName[ln] = ')')) or
           ((RealName[1] = '<') and (RealName[ln] = '>')) or
           ((RealName[1] = '"') and (RealName[ln] = '"'))) then RealName := alCopyStr(RealName,2,Ln-2)

end;

{**************************************************************************}
function AlExtractEmailAddress(const FriendlyEmail: AnsiString): AnsiString;
Var LRealName: AnsiString;
Begin
  Result := AlParseEmailAddress(FriendlyEmail, LRealName, False);
end;

{**************************************************************************}
{Return a new string with backslashes in str replaced by two backslashes and
 double quotes replaced by backslash-double quote.}
Function ALEncodeRealName4FriendlyEmailAddress(const aRealName: AnsiString): AnsiString;
var i, l, x: integer;
    Buf, P: PAnsiChar;
    ch: AnsiChar;
begin
  Result := '';
  L := Length(aRealName);
  if L = 0 then exit;
  GetMem(Buf, (L * 2) + 2); // to be on the *very* safe side
  try
    Ch := '"';
    P := Buf;
    ALMove(Ch, P^, 1);
    inc(P,1);
    for i := 1 to L do begin
      x := Ord(aRealName[i]);
      case x of
        34: begin // quot "
              ALStrMove('/"', P, 2);
              Inc(P, 2);
            end;
        47: begin // backslash /
              ALStrMove('//', P, 2);
              Inc(P, 2);
            end;
        else Begin
          P^:= AnsiChar(X);
          Inc(P);
        end;
      end;
    end;
    ALMove(Ch, P^, 1);
    inc(P,1);
    SetString(Result, Buf, P - Buf);
  finally
    FreeMem(Buf);
  end;
end;

{********************************************************************************}
{this takes a 2-tuple of the form (realname, email_address) and returns the string
 value suitable for a To: or Cc: header.}
Function ALMakeFriendlyEmailAddress(const aRealName, aEmail: AnsiString): AnsiString;
begin
  if aRealName <> '' then Result := ALEncodeRealName4FriendlyEmailAddress(aRealName) + ' <' + aEmail + '>'
  else result := aEmail;
end;

{***********************************************}
Function AlGenerateInternetMessageID: AnsiString;
Begin
  Result := AlStringReplace(ALNewGUIDString(true{WithoutBracket}),'-','',[rfReplaceAll]) + '@' + ALTrim(AlGetLocalHostName);
end;

{****************************************************************************}
Function AlGenerateInternetMessageID(const ahostname: AnsiString): AnsiString;
var LTmpHostName: ansiString;
Begin
  LTmpHostName := ALTrim(ahostname);
  If LTmpHostName <> '' then Result := AlStringReplace(ALNewGUIDString(true{WithoutBracket}),'-','',[rfReplaceAll]) + '@' + LTmpHostName
  else Result := AlGenerateInternetMessageID;
end;

{*****************************}
procedure TALEmailHeader.Clear;
begin
  fSendTo := '';
  fSender := '';
  fMessageID := '';
  fbcc := '';
  fContentTransferEncoding := '';
  fComments := '';
  fMIMEVersion := '';
  fPriority := '';
  fReplyTo := '';
  fSubject := '';
  fFrom := '';
  fDate := '';
  fDispositionNotificationTo := '';
  fReferences := '';
  fcc := '';
  fContentType := '';
  FCustomHeaders.Clear;
end;

{********************************}
constructor TALEmailHeader.Create;
begin
  inherited create;
  FCustomHeaders:= TALStringList.create;
  FCustomHeaders.NameValueSeparator := ':';
  clear;
  fMessageID := 'AUTO';
  fMIMEVersion := '1.0';
  fDate := 'NOW';
  fContentType := 'text/plain';
end;

{********************************}
destructor TALEmailHeader.Destroy;
begin
  FCustomHeaders.free;
  inherited;
end;

{***************************************************}
function TALEmailHeader.GetRawHeaderText: AnsiString;
Var I : integer;
    Str: AnsiString;
begin
  Result := '';
  If ALTrim(fFrom) <> '' then result := result + 'From: ' + ALTrim(fFrom) + #13#10;
  If ALTrim(fSender) <> '' then result := result + 'Sender: ' + ALTrim(fSender) + #13#10;
  If ALTrim(fSendTo) <> '' then result := result + 'To: ' + ALTrim(fSendTo) + #13#10;
  If ALTrim(fcc) <> '' then result := result + 'cc: ' + ALTrim(fcc) + #13#10;
  If ALTrim(fbcc) <> '' then result := result + 'bcc: ' + ALTrim(fbcc) + #13#10;
  If ALTrim(fReplyTo) <> '' then result := result + 'Reply-To: ' + ALTrim(fReplyTo) + #13#10;
  If ALTrim(fSubject) <> '' then result := result + 'Subject: ' + ALTrim(fSubject) + #13#10;
  Str := fMessageID;
  If ALTrim(str) <> '' then begin
    If ALSameText(Str, 'AUTO') then Str := '<' + AlGenerateInternetMessageID + '>';
    result := result + 'Message-ID: ' + ALTrim(str) + #13#10;
  end;
  If ALTrim(fReferences) <> '' then result := result + 'References: ' + ALTrim(fReferences) + #13#10;
  If ALTrim(fComments) <> '' then result := result + 'Comments: ' + ALTrim(fComments) + #13#10;
  Str := fDate;
  If ALTrim(str) <> '' then begin
    If ALSameText(Str, 'NOW') then Str := ALDateTimeToRfc822Str(Now);
    result := result + 'Date: ' + ALTrim(str) + #13#10;
  end;
  If ALTrim(fContentType) <> '' then result := result + 'Content-Type: ' + ALTrim(fContentType) + #13#10;
  If ALTrim(fContentTransferEncoding) <> '' then result := result + 'Content-Transfer-Encoding: ' + ALTrim(fContentTransferEncoding) + #13#10;
  If ALTrim(fMIMEVersion) <> '' then result := result + 'MIME-Version: ' + ALTrim(fMIMEVersion) + #13#10;
  If ALTrim(fPriority) <> '' then result := result + 'Priority: ' + ALTrim(fPriority) + #13#10;
  If ALTrim(fDispositionNotificationTo) <> '' then result := result + 'Disposition-Notification-To: ' + ALTrim(fDispositionNotificationTo) + #13#10;
  For I := 0 to FCustomHeaders.count - 1 do
    if (ALTrim(FCustomHeaders.names[I]) <> '') and (ALTrim(FCustomHeaders.ValueFromIndex[I]) <> '') then
      result := result + FCustomHeaders.names[I] + ': ' + ALTrim(FCustomHeaders.ValueFromIndex[I]) + #13#10;
end;

{**************************************************************************}
procedure TALEmailHeader.SetRawHeaderText(const aRawHeaderText: AnsiString);

Var LRawHeaderLst: TALStringList;

  {-----------------------------------------------------------}
  Function _extractHeader(const aName: AnsiString): AnsiString;
  Var I: Integer;
      Str: AnsiString;
  Begin
    I := LRawHeaderLst.IndexOfName(aName);
    If I >= 0 then Begin
      result := ALTrim(LRawHeaderLst.ValueFromIndex[I]);
      LRawHeaderLst.Delete(I);
      While True do begin
        If I >= LRawHeaderLst.Count then break;
        str := LRawHeaderLst[I];
        If (str = '') or
           (not (str[1] in [' ',#9])) then break; //(1) an empty line or (2) a line that does not start with a space, a tab, or a field name followed by a colon
        Result := ALTrim(result + ' ' + ALTrim(str));
        LRawHeaderLst.Delete(I);
      end;
    end
    else result := '';
  end;

Var Str1, Str2: AnsiString;
    J: integer;

begin
  Clear;
  LRawHeaderLst := TALStringList.create;
  try
    LRawHeaderLst.NameValueSeparator := ':';
    LRawHeaderLst.Text := aRawHeaderText;

    fFrom:= _extractHeader('From');
    fSender:= _extractHeader('Sender');
    fSendTo:= _extractHeader('To');
    fcc:= _extractHeader('cc');
    fbcc:= _extractHeader('bcc');
    fReplyTo:= _extractHeader('Reply-To');
    fSubject:= _extractHeader('Subject');
    fMessageID:= _extractHeader('Message-ID');
    fReferences:= _extractHeader('References');
    fComments:= _extractHeader('Comments');
    fDate:= _extractHeader('Date');
    fContentType:= _extractHeader('Content-Type');
    fContentTransferEncoding:= _extractHeader('Content-Transfer-Encoding');
    fMIMEVersion:= _extractHeader('MIME-Version');
    fPriority:= _extractHeader('Priority');
    fDispositionNotificationTo:= _extractHeader('Disposition-Notification-To');

    FCustomHeaders.clear;
    J := 0;
    while J <= LRawHeaderLst.count - 1 do begin
      Str1 := ALTrim(LRawHeaderLst.Names[J]);
      If (ALTrim(str1) <> '') and (not (str1[1] in [' ',#9])) then begin
        Str1 := ALTrim(Str1) + ': ' + ALTrim(LRawHeaderLst.ValueFromIndex[J]);
        inc(J);
        While True do begin
          If J >= LRawHeaderLst.Count then break;
          str2 := LRawHeaderLst[J];
          If (str2 = '') or
             (not (str2[1] in [' ',#9])) then break; //(1) an empty line or (2) a line that does not start with a space, a tab, or a field name followed by a colon
          Str1 := ALTrim(Str1 + ' ' + ALTrim(str2));
          inc(J);
        end;
        FCustomHeaders.Add(Str1);
      end
      else inc(J);
    end;

  finally
    LRawHeaderLst.Free;
  end;
end;

{***********************************}
procedure TALNewsArticleHeader.Clear;
begin
  fExpires := '';
  fMessageID := '';
  fReplyTo := '';
  fOrganization := '';
  fDateReceived := '';
  fNNTPPostingHost := '';
  fContentTransferEncoding := '';
  fComments := '';
  fMIMEVersion := '';
  fSender := '';
  fNewsgroups := '';
  fReferences := '';
  fPostingVersion := '';
  fRelayVersion := '';
  fDate := '';
  fNNTPPostingDate := '';
  fPath := '';
  fDistribution := '';
  fContentType := '';
  fFollowupTo := '';
  fSubject := '';
  fControl := '';
  fFrom := '';
  FCustomHeaders.clear;
end;

{**************************************}
constructor TALNewsArticleHeader.Create;
begin
  inherited create;
  FCustomHeaders:= TALStringList.create;
  FCustomHeaders.NameValueSeparator := ':';
  clear;
  fMessageID := 'AUTO';
  fMIMEVersion := '1.0';
  fDate := 'NOW';
  fContentType := 'text/plain';
end;

{**************************************}
destructor TALNewsArticleHeader.Destroy;
begin
  FCustomHeaders.free;
  inherited;
end;

{*********************************************************}
function TALNewsArticleHeader.GetRawHeaderText: AnsiString;
Var I : integer;
    Str: AnsiString;
begin
  Result := '';
  If ALTrim(fRelayVersion) <> '' then result := result + 'Relay-Version: ' + ALTrim(fRelayVersion) + #13#10;
  If ALTrim(fPostingVersion) <> '' then result := result + 'Posting-Version: ' + ALTrim(fPostingVersion) + #13#10;
  If ALTrim(fFrom) <> '' then result := result + 'From: ' + ALTrim(fFrom) + #13#10;
  Str := fDate;
  If ALTrim(str) <> '' then begin
    If ALSameText(Str, 'NOW') then Str := ALDateTimeToRfc822Str(Now);
    result := result + 'Date: ' + ALTrim(str) + #13#10;
  end;
  If ALTrim(fNewsgroups) <> '' then result := result + 'Newsgroups: ' + ALTrim(fNewsgroups) + #13#10;
  If ALTrim(fSubject) <> '' then result := result + 'Subject: ' + ALTrim(fSubject) + #13#10;
  Str := fMessageID;
  If ALTrim(str) <> '' then begin
    If ALSameText(Str, 'AUTO') then Str := '<' + AlGenerateInternetMessageID + '>';
    result := result + 'Message-ID: ' + ALTrim(str) + #13#10;
  end;
  If ALTrim(fPath) <> '' then result := result + 'Path: ' + ALTrim(fPath) + #13#10;
  If ALTrim(fReplyTo) <> '' then result := result + 'Reply-To: ' + ALTrim(fReplyTo) + #13#10;
  If ALTrim(fSender) <> '' then result := result + 'Sender: ' + ALTrim(fSender) + #13#10;
  If ALTrim(fFollowupTo) <> '' then result := result + 'Followup-To: ' + ALTrim(fFollowupTo) + #13#10;
  If ALTrim(fDateReceived) <> '' then result := result + 'Date-Received: ' + ALTrim(fDateReceived) + #13#10;
  If ALTrim(fExpires) <> '' then result := result + 'Expires: ' + ALTrim(fExpires) + #13#10;
  If ALTrim(fReferences) <> '' then result := result + 'References: ' + ALTrim(fReferences) + #13#10;
  If ALTrim(fControl) <> '' then result := result + 'Control: ' + ALTrim(fControl) + #13#10;
  If ALTrim(fDistribution) <> '' then result := result + 'Distribution: ' + ALTrim(fDistribution) + #13#10;
  If ALTrim(fOrganization) <> '' then result := result + 'Organization: ' + ALTrim(fOrganization) + #13#10;
  If ALTrim(fComments) <> '' then result := result + 'Comments: ' + ALTrim(fComments) + #13#10;
  If ALTrim(fContentType) <> '' then result := result + 'Content-Type: ' + ALTrim(fContentType) + #13#10;
  If ALTrim(fContentTransferEncoding) <> '' then result := result + 'Content-Transfer-Encoding: ' + ALTrim(fContentTransferEncoding) + #13#10;
  If ALTrim(fMIMEVersion) <> '' then result := result + 'MIME-Version: ' + ALTrim(fMIMEVersion) + #13#10;
  If ALTrim(fNNTPPostingHost) <> '' then result := result + 'NNTP-Posting-Host: ' + ALTrim(fNNTPPostingHost) + #13#10;
  If ALTrim(fNNTPPostingDate) <> '' then result := result + 'NNTP-Posting-Date: ' + ALTrim(fNNTPPostingDate) + #13#10;
  For I := 0 to FCustomHeaders.count - 1 do
    if (ALTrim(FCustomHeaders.names[I]) <> '') and (ALTrim(FCustomHeaders.ValueFromIndex[I]) <> '') then
      result := result + FCustomHeaders.names[I] + ': ' + ALTrim(FCustomHeaders.ValueFromIndex[I]) + #13#10;
end;

{********************************************************************************}
procedure TALNewsArticleHeader.SetRawHeaderText(const aRawHeaderText: AnsiString);

Var LRawHeaderLst: TALStringList;

  {-----------------------------------------------------------}
  Function _extractHeader(const aName: AnsiString): AnsiString;
  Var I: Integer;
      Str: AnsiString;
  Begin
    I := LRawHeaderLst.IndexOfName(aName);
    If I >= 0 then Begin
      result := ALTrim(LRawHeaderLst.ValueFromIndex[I]);
      LRawHeaderLst.Delete(I);
      While True do begin
        If I >= LRawHeaderLst.Count then break;
        str := LRawHeaderLst[I];
        If (str = '') or
           (not (str[1] in [' ',#9])) then break; //(1) an empty line or (2) a line that does not start with a space, a tab, or a field name followed by a colon
        Result := ALTrim(result + ' ' + ALTrim(str));
        LRawHeaderLst.Delete(I);
      end;
    end
    else result := '';
  end;

Var Str1, Str2: AnsiString;
    J: integer;

begin
  Clear;
  LRawHeaderLst := TALStringList.create;
  try
    LRawHeaderLst.NameValueSeparator := ':';
    LRawHeaderLst.Text := aRawHeaderText;

    fRelayVersion:= _extractHeader('Relay-Version');
    fPostingVersion:= _extractHeader('Posting-Version');
    fFrom:= _extractHeader('From');
    fDate:= _extractHeader('Date');
    fNewsgroups:= _extractHeader('Newsgroups');
    fSubject:= _extractHeader('Subject');
    fMessageID:= _extractHeader('Message-ID');
    fPath:= _extractHeader('Path');
    fReplyTo:= _extractHeader('Reply-To');
    fSender:= _extractHeader('Sender');
    fFollowupTo:= _extractHeader('Followup-To');
    fDateReceived:= _extractHeader('Date-Received');
    fExpires:= _extractHeader('Expires');
    fReferences:= _extractHeader('References');
    fControl:= _extractHeader('Control');
    fDistribution:= _extractHeader('Distribution');
    fOrganization:= _extractHeader('Organization');
    fComments:= _extractHeader('Comments');
    fContentType:= _extractHeader('Content-Type');
    fContentTransferEncoding:= _extractHeader('Content-Transfer-Encoding');
    fMIMEVersion:= _extractHeader('MIME-Version');
    fNNTPPostingHost:= _extractHeader('NNTP-Posting-Host');
    fNNTPPostingDate:= _extractHeader('NNTP-Posting-Date');

    FCustomHeaders.clear;
    J := 0;
    while J <= LRawHeaderLst.count - 1 do begin
      Str1 := ALTrim(LRawHeaderLst.Names[J]);
      If (ALTrim(str1) <> '') and (not (str1[1] in [' ',#9])) then begin
        Str1 := ALTrim(Str1) + ': ' + ALTrim(LRawHeaderLst.ValueFromIndex[J]);
        inc(J);
        While True do begin
          If J >= LRawHeaderLst.Count then break;
          str2 := LRawHeaderLst[J];
          If (str2 = '') or
             (not (str2[1] in [' ',#9])) then break; //(1) an empty line or (2) a line that does not start with a space, a tab, or a field name followed by a colon
          Str1 := ALTrim(Str1 + ' ' + ALTrim(str2));
          inc(J);
        end;
        FCustomHeaders.Add(Str1);
      end
      else inc(J);
    end;

  finally
    LRawHeaderLst.Free;
  end;
end;

{*********************}
{$ZEROBASEDSTRINGS OFF} // << the guy who introduce zero base string in delphi is just a mix of a Monkey and a Donkey !
{$WARN SYMBOL_DEPRECATED OFF}
function AlIsValidEmail(const Value: AnsiString): boolean;

 {------------------------------------------------------}
 function CheckAllowedName(const s: AnsiString): boolean;
 var I: integer;
 begin
   Result:= false;
   for I:= low(s) to high(s) do begin
     // illegal char in s -> no valid address
     if not CharInSet(s[I], ['a'..'z','A'..'Z','0'..'9','_','-','.','+']) then Exit;
   end;
   Result:= true;
 end;

 {----------------------------------------------------------}
 function CheckAllowedHostname(const s: AnsiString): boolean;
 var I: integer;
 begin
   Result:= false;
   for I:= low(s) to high(s) do begin
     // illegal char in s -> no valid address
     if not CharInSet(s[I], ['a'..'z','A'..'Z','0'..'9','-','.']) then Exit;
   end;
   Result:= true;
 end;

 {-----------------------------------------------------}
 function CheckAllowedExt(const s: AnsiString): boolean;
 var I: integer;
 begin
   Result:= false;
   for I:= low(s) to high(s) do begin
     // illegal char in s -> no valid address
     if not CharInSet(s[I], ['a'..'z','A'..'Z']) then Exit;
   end;
   Result:= true;
 end;

var I, J: integer;
    namePart, serverPart, extPart: AnsiString;

begin
  Result := false;

  // Value can not be < 6 char (ex: a@b.fr)
  if length(Value) < 6 then exit;

  // must have the '@' char inside
  I := AlPos('@', Value);
  if (I <= 1) or (I > length(Value)-4) then exit;

  //can not have @. or .@
  if (value[I-1] = '.') or (value[I+1] = '.') then exit;

  //can not have 2 ..
  If (ALpos('..', Value) > 0) then Exit;

  //extract namePart and serverPart
  namePart:= AlCopyStr(Value, 1, I - 1);
  serverPart:= AlCopyStr(Value, I + 1, Length(Value));

  // Extension (.fr, .com, etc..) must be betwen 2 to 6 char
  I:= AlPos('.', serverPart);
  J := 0;
  While I > 0 do begin
    J := I;
    I := AlPosEx('.', serverPart, I + 1);
  end;
  if (J <= 1) then Exit; // no dot at all so exit !
  extPart    := AlCopyStr(ServerPart,J+1,Maxint);
  serverPart := ALCopyStr(ServerPart, 1, J - 1);
  If not (Length(ExtPart) in [2..6]) then exit;

  Result:= CheckAllowedname(namePart) and
           CheckAllowedHostname(serverPart) and
           CheckAllowedExt(ExtPart) and
           (length(Value) <= 254); // http://stackoverflow.com/questions/386294/what-is-the-maximum-length-of-a-valid-email-address
end;
{$WARN SYMBOL_DEPRECATED ON}
{$IF defined(ALZeroBasedStringsON)}
  {$ZEROBASEDSTRINGS ON}
{$IFEND}

{$ENDIF !ALHideAnsiString}

{*********************}
{$ZEROBASEDSTRINGS OFF} // << the guy who introduce zero base string in delphi is just a mix of a Monkey and a Donkey !
{$WARN SYMBOL_DEPRECATED OFF}
function AlIsValidEmailU(const Value: String): boolean;

 {--------------------------------------------------}
 function CheckAllowedName(const s: String): boolean;
 var I: integer;
 begin
   Result:= false;
   for I:= low(s) to high(s) do begin
     // illegal char in s -> no valid address
     if not CharInSet(s[I], ['a'..'z','A'..'Z','0'..'9','_','-','.','+']) then Exit;
   end;
   Result:= true;
 end;

 {------------------------------------------------------}
 function CheckAllowedHostname(const s: String): boolean;
 var I: integer;
 begin
   Result:= false;
   for I:= low(s) to high(s) do begin
     // illegal char in s -> no valid address
     if not CharInSet(s[I], ['a'..'z','A'..'Z','0'..'9','-','.']) then Exit;
   end;
   Result:= true;
 end;

 {-------------------------------------------------}
 function CheckAllowedExt(const s: String): boolean;
 var I: integer;
 begin
   Result:= false;
   for I:= low(s) to high(s) do begin
     // illegal char in s -> no valid address
     if not CharInSet(s[I], ['a'..'z','A'..'Z']) then Exit;
   end;
   Result:= true;
 end;

var I, J: integer;
    namePart, serverPart, extPart: String;

begin
  Result := false;

  // Value can not be < 6 char (ex: a@b.fr)
  if length(Value) < 6 then exit;

  // must have the '@' char inside
  I := AlPosU('@', Value);
  if (I <= 1) or (I > length(Value)-4) then exit;

  //can not have @. or .@
  if (value[I-1] = '.') or (value[I+1] = '.') then exit;

  //can not have 2 ..
  If (ALposU('..', Value) > 0) then Exit;

  //extract namePart and serverPart
  namePart:= AlCopyStrU(Value, 1, I - 1);
  serverPart:= AlCopyStrU(Value, I + 1, Length(Value));

  // Extension (.fr, .com, etc..) must be betwen 2 to 6 char
  I:= AlPosU('.', serverPart);
  J := 0;
  While I > 0 do begin
    J := I;
    I := AlPosExU('.', serverPart, I + 1);
  end;
  if (J <= 1) then Exit; // no dot at all so exit !
  extPart    := AlCopyStrU(ServerPart,J+1,Maxint);
  serverPart := ALCopyStrU(ServerPart, 1, J - 1);
  If not (Length(ExtPart) in [2..6]) then exit;

  Result:= CheckAllowedname(namePart) and
           CheckAllowedHostname(serverPart) and
           CheckAllowedExt(ExtPart) and
           (length(Value) <= 254); // http://stackoverflow.com/questions/386294/what-is-the-maximum-length-of-a-valid-email-address
end;
{$WARN SYMBOL_DEPRECATED ON}
{$IF defined(ALZeroBasedStringsON)}
  {$ZEROBASEDSTRINGS ON}
{$IFEND}

end.
