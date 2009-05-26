{*************************************************************
www:          http://sourceforge.net/projects/alcinoe/              
Author(s):    Stéphane Vander Clock (svanderclock@arkadia.com)
Sponsor(s):   Arkadia SA (http://www.arkadia.com)
							
product:      Alcinoe Internet Messages Utilities
Version:      3.51

Description:  This unit contains utilities to manipulate
              Internet Messages Headers (EMAIL or NEWSNET messages)

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

History :     05/02/2008: add soft line break in ALDecodeQuotedPrintableString

Link :        http://www.faqs.org/rfcs/rfc2047.html

Please send all your feedback to svanderclock@arkadia.com
**************************************************************}
unit ALInternetMessageCommon;

interface

uses Classes;

Type

    {---------------------------------}
    TALEMailHeader = Class(Tpersistent)
    Private
      fSendTo: String;
      fSender: String;
      fMessageID: String;
      fbcc: String;
      fContentTransferEncoding: String;
      fComments: String;
      fMIMEVersion: String;
      fPriority: String;
      fReplyTo: String;
      fSubject: String;
      fFrom: String;
      fDate: String;
      fDispositionNotificationTo: String;
      fReferences: String;
      fcc: String;
      fContentType: String;
      FCustomHeaders: Tstrings;
      Function GetRawHeaderText: String;
      procedure SetRawHeaderText(const aRawHeaderText: string);
    protected
      procedure AssignTo(Dest: TPersistent); override;
    public
      constructor Create; virtual;
      destructor Destroy; override;
      procedure Clear;
    Published
      property From: String read fFrom write fFrom; {From: John Doe <jdoe@machine.example> - Author(s) or person(s) taking responsibility for the message 4.4.1; RFC 1123: 5.2.15-16, 5.3.7; RFC 1036: 2.1.1}
      property Sender: String read fSender write fSender; {Sender: Michael Jones <mjones@machine.example> - The person or agent submitting the message to the network, if other than shown by the From header RFC 822: 4.4.2; RFC 1123: 5.2.15-16, 5.3.7; RFC 1036: 2.1.1}
      property SendTo: String read fSendTo write fSendTo; {To: Mary Smith <mary@example.net> - Primary recipient(s) RFC 822: 4.5.1; RFC 1123: 5.2.15-16, 5.3.7;}
      property cc: String read fcc write fcc; {cc: <boss@nil.test>, "Giant; \"Big\" Box" <sysservices@example.net> - Secondary, informational recipient(s) RFC 822: 4.5.2; RFC 1123: 5.2.15-16, 5.3.7;}
      property bcc: String read fbcc write fbcc; {bcc: <boss@nil.test>, "Giant; \"Big\" Box" <sysservices@example.net> - Recipient(s) not to be disclosed to other recipients ("blind carbon copy") RFC 822: 4.5.3; RFC 1123: 5.2.15-16, 5.3.7;}
      property ReplyTo: String read fReplyTo write fReplyTo; {Reply-To: "Mary Smith: Personal Account" <smith@home.example> - Suggested E-mail address for replies RFC 822: 4.4.3; RFC 1036: 2.2.1}
      property Subject: String read fSubject write fSubject; {Subject: Saying Hello - Text that provides a summary, or indicates the nature, of the message RFC 822: 4.7.1; RFC 1036: 2.1.4}
      property MessageID: String read fMessageID write fMessageID; {Message-ID: <1234@local.machine.example> -	Unique ID for the message RFC 822: 4.6.1; RFC 1036: 2.1.5}
      property References: String read fReferences write fReferences; {References: <1234@local.machine.example> <3456@example.net> - In E-mail: reference to other related messages; in Usenet: reference to replied-to-articles RFC 822: 4.6.3; RFC 1036: 2.2.5}
      property Comments: String read fComments write fComments; {Comments: Authenticated sender is gboyd@netcom.com - Text comments added to the message RFC 822: 4.7.2}
      property Date: String read fDate write fDate; {Date: Fri, 21 Nov 1997 09:55:06 -0600 - The time when the message was written (or submitted) RFC 822: 5.1; RFC 1123: 5.2.14; RFC 1036: 2.1.2}
      property ContentType: String read fContentType write fContentType; {Content-Type: text/plain; charset="iso-8859-1" - Data type and format of content RFC 1049 (historic); RFC 1123: 5.2.13; RFC 2045: 5; RFC 1766: 4.1}
      property ContentTransferEncoding: String read fContentTransferEncoding write fContentTransferEncoding; {Content-Transfer-Encoding: 8bit - Coding method used in a MIME message body RFC 2045: 6;}
      property MIMEVersion: String read fMIMEVersion write fMIMEVersion; {MIME-Version: 1.0 - specifies the version of MIME that the message format complies with RFC 2045: 4}
      property Priority: String read fPriority write fPriority; {Priority: normal - Priority for message delivery ("normal" / "non-urgent" / "urgent") RFC 2156}
      property DispositionNotificationTo: String read fDispositionNotificationTo write fDispositionNotificationTo; {Disposition-Notification-To: boss@nil.test - Requests for notification when the message is received, and specifies the address for them RFC 2298}
      property CustomHeaders: Tstrings read FCustomHeaders;
      Property RawHeaderText: String read GetRawHeaderText write SetRawHeaderText;
    end;

    {---------------------------------------}
    TALNewsArticleHeader = Class(Tpersistent)
    Private
      FCustomHeaders: Tstrings;
      fExpires: String;
      fMessageID: String;
      fReplyTo: String;
      fOrganization: String;
      fDateReceived: String;
      fNNTPPostingHost: String;
      fContentTransferEncoding: String;
      fComments: String;
      fMIMEVersion: String;
      fSender: String;
      fNewsgroups: String;
      fReferences: String;
      fPostingVersion: String;
      fRelayVersion: String;
      fDate: String;
      fNNTPPostingDate: String;
      fPath: String;
      fDistribution: String;
      fContentType: String;
      fFollowupTo: String;
      fSubject: String;
      fControl: String;
      fFrom: String;
      Function GetRawHeaderText: String;
      procedure SetRawHeaderText(const aRawHeaderText: string);
    protected
      procedure AssignTo(Dest: TPersistent); override;
    public
      constructor Create; virtual;
      destructor Destroy; override;
      procedure Clear;
    Published
      property RelayVersion: String read fRelayVersion write fRelayVersion; {Relay-Version: Version 1.7 PSU-NETNEWS 5/20/88; site MAINE.BITNET}
      property PostingVersion: String read fPostingVersion write fPostingVersion; {Posting-Version: Version 1.7 PSU-NETNEWS 5/20/88; site MAINE.BITNET}
      property From: String read fFrom write fFrom; {From: John Doe <jdoe@machine.example> - Author(s) or person(s) taking responsibility for the message 4.4.1; RFC 1123: 5.2.15-16, 5.3.7; RFC 1036: 2.1.1}
      property Date: String read fDate write fDate; {Date: Fri, 21 Nov 1997 09:55:06 -0600 - The time when the message was written (or submitted) RFC 822: 5.1; RFC 1123: 5.2.14; RFC 1036: 2.1.2}
      property Newsgroups: String read fNewsgroups write fNewsgroups; {Newsgroups: net.sport.football}
      property Subject: String read fSubject write fSubject; {Subject: Saying Hello - Text that provides a summary, or indicates the nature, of the message RFC 822: 4.7.1; RFC 1036: 2.1.4}
      property MessageID: String read fMessageID write fMessageID; {Message-ID: <1234@local.machine.example> -	Unique ID for the message RFC 822: 4.6.1; RFC 1036: 2.1.5}
      property Path: String read fPath write fPath; {Path: psuvm!cunyvm!maine.bitnet!michael}
      property ReplyTo: String read fReplyTo write fReplyTo; {Reply-To: "Mary Smith: Personal Account" <smith@home.example> - Suggested E-mail address for replies RFC 822: 4.4.3; RFC 1036: 2.2.1}
      property Sender: String read fSender write fSender; {Sender: Michael Jones <mjones@machine.example> - The person or agent submitting the message to the network, if other than shown by the From header RFC 822: 4.4.2; RFC 1123: 5.2.15-16, 5.3.7; RFC 1036: 2.1.1}
      property FollowupTo: String read fFollowupTo write fFollowupTo; {Followup-To: uk.legal,uk.misc}
      property DateReceived: String read fDateReceived write fDateReceived; {Date-Received: Fri, 21 Nov 1997 09:55:06 -0600}
      property Expires: String read fExpires write fExpires; {Expires: Fri, 21 Nov 1997 09:55:06 -0600}
      property References: String read fReferences write fReferences; {References: <1234@local.machine.example> <3456@example.net> - In E-mail: reference to other related messages; in Usenet: reference to replied-to-articles RFC 822: 4.6.3; RFC 1036: 2.2.5}
      property Control: String read fControl write fControl; {Control: cancel <xb8700A@twits.site.com>}
      property Distribution: String read fDistribution write fDistribution; {Distribution: nj.all}
      property Organization: String read fOrganization write fOrganization; {Organization: A poorly-installed InterNetNews site}
      property Comments: String read fComments write fComments; {Comments: Authenticated sender is gboyd@netcom.com - Text comments added to the message RFC 822: 4.7.2}
      property ContentType: String read fContentType write fContentType; {Content-Type: text/plain; charset="iso-8859-1" - Data type and format of content RFC 1049 (historic); RFC 1123: 5.2.13; RFC 2045: 5; RFC 1766: 4.1}
      property ContentTransferEncoding: String read fContentTransferEncoding write fContentTransferEncoding; {Content-Transfer-Encoding: 8bit - Coding method used in a MIME message body RFC 2045: 6;}
      property MIMEVersion: String read fMIMEVersion write fMIMEVersion; {MIME-Version: 1.0 - specifies the version of MIME that the message format complies with RFC 2045: 4}
      property NNTPPostingHost: String read fNNTPPostingHost write fNNTPPostingHost; {NNTP-Posting-Host: stc92-3-82-245-250-13.fbx.proxad.net}
      property NNTPPostingDate: String read fNNTPPostingDate write fNNTPPostingDate; {NNTP-Posting-Date: Sun, 30 Sep 2007 20:28:56 +0000 (UTC)}
      property CustomHeaders: Tstrings read FCustomHeaders;
      Property RawHeaderText: String read GetRawHeaderText write SetRawHeaderText;
    end;

{---------------------------------------------------------------------------------------------------------------------}
function AlParseEmailAddress(FriendlyEmail: String; var RealName : String; Const decodeRealName: Boolean=True): String;
function AlExtractEmailAddress(FriendlyEmail: String): String;
Function ALMakeFriendlyEmailAddress(aRealName, aEmail: String): String;
Function ALEncodeRealName4FriendlyEmailAddress(aRealName: String): String;
Function AlGenerateInternetMessageID: String; overload;
Function AlGenerateInternetMessageID(ahostname: String): String; overload;
Function ALDecodeQuotedPrintableString(src: String): String;
Function AlDecodeInternetMessageHeaderInUTF8(aHeaderStr: String; aDefaultCodePage: Integer): UTF8String;

implementation

uses Sysutils,
     Masks,
     ALFcnMisc,
     AlFcnRfc,
     AlFcnMime,
     AlFcnWinsock,
     AlFcnUnicode,
     AlFcnString;

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
function AlParseEmailAddress(FriendlyEmail: String; var RealName : String; Const decodeRealName: Boolean=True): String;

var P1, P2, P3, P4, P5: integer;
    S1: String;
    ln: integer;

begin

  {----------}
  FriendlyEmail := trim(FriendlyEmail);

  {----------}
  Result := '';
  RealName := '';

  {----------}
  ln := Length(FriendlyEmail);

  {----------}
  if MatchesMask(FriendlyEmail, '* <*>') then P1 := Ln-1  // toto <toto@toto.com> | "toto" <toto@toto.com> | (toto) <toto@toto.com>
  else if MatchesMask(FriendlyEmail, '<*> *') then P1 := 2 // <toto@toto.com> toto.com | <toto@toto.com> "toto" | <toto@toto.com> (toto)
  else if MatchesMask(FriendlyEmail, '<*>') then P1 := 2 // <toto@toto.com>
  else if MatchesMask(FriendlyEmail, '* (*)') then P1 := 1 // toto@toto.com (toto)
  else Begin
    RealName := '';
    Result := FriendlyEmail;
    Exit;
  end;

  {----------}
  Result := FriendlyEmail[P1];

  {----------}
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

  {----------}
  If (P2 > 0) and
     (P3 <= Ln) then begin
    If (
        ((FriendlyEmail[P2] = '<') and (FriendlyEmail[P3] = '>'))
       ) then begin
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

  {----------}
  RealName := Trim(AlCopyStr(FriendlyEmail,1,P2) + AlCopyStr(FriendlyEmail,P3,Maxint));

  {----------}
  If (RealName = '') then begin
    If (result <> '') then exit;
    RealName := trim(FriendlyEmail);
  end;

  {----------}
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
          (
           ((RealName[1] = '(') and (RealName[ln] = ')')) or
           ((RealName[1] = '<') and (RealName[ln] = '>')) or
           ((RealName[1] = '"') and (RealName[ln] = '"'))
          ) then RealName := alCopyStr(RealName,2,Ln-2)

end;

{************************************************************}
function AlExtractEmailAddress(FriendlyEmail: String): String;
Var aRealName: String;
Begin
  Result := AlParseEmailAddress(FriendlyEmail, aRealName, False);
end;

{**************************************************************************}
{Return a new string with backslashes in str replaced by two backslashes and
 double quotes replaced by backslash-double quote.}
Function ALEncodeRealName4FriendlyEmailAddress(aRealName: String): String;
var i, l, x: integer;
    Buf, P: PChar;
    ch: Char;
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
              ALMove('/"', P^, 2);
              Inc(P, 2);
            end;
        47: begin // backslash /
              ALMove('//', P^, 2);
              Inc(P, 2);
            end;
        else Begin
          P^:= Char(X);
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
Function ALMakeFriendlyEmailAddress(aRealName, aEmail: String): String;
begin
  if aRealName <> '' then Result := ALEncodeRealName4FriendlyEmailAddress(aRealName) + ' <' + aEmail + '>'
  else result := aEmail;
end;

{*******************************************}
Function AlGenerateInternetMessageID: String;
Begin
  Result := AlStringReplace(ALMakeKeyStrByGUID,'-','',[rfReplaceAll]) + '@' + trim(AlGetLocalHostName);
end;

{**************************************************************}
Function AlGenerateInternetMessageID(ahostname: String): String;
Begin
  ahostname := trim(ahostname);
  If ahostname <> '' then Result := AlStringReplace(ALMakeKeyStrByGUID,'-','',[rfReplaceAll]) + '@' + ahostname
  else Result := AlGenerateInternetMessageID;
end;

{**********************************************************}
Function ALDecodeQuotedPrintableString(src: String): String;
var CurrentSrcPos, CurrentResultPos : Integer;
    Entity : String;
    SrcLength: integer;
    in1: integer;

    {--------------------------------------}
    procedure CopyCurrentSrcPosCharToResult;
    Begin
      result[CurrentResultPos] := src[CurrentSrcPos];
      inc(CurrentResultPos);
      inc(CurrentSrcPos);
    end;

    {-----------------------------------------------------------------}
    procedure CopyCharToResult(aChar: Char; NewCurrentSrcPos: integer);
    Begin
      result[CurrentResultPos] := aChar;
      inc(CurrentResultPos);
      CurrentSrcPos := NewCurrentSrcPos;
    end;

begin
  {remove soft line break}
  Src := AlStringReplace(Src,'='#13#10,'',[rfReplaceAll]);

  {init var}
  CurrentSrcPos := 1;
  CurrentResultPos := 1;
  SrcLength := Length(src);
  Entity := '';
  SetLength(Result,SrcLength);

  {start loop}
  while (CurrentSrcPos <= SrcLength) do begin

    {Encoded entity detected}
    If src[CurrentSrcPos]= '=' then begin

      {Encoded entity is valid in length}
      If (CurrentSrcPos+2<=SrcLength) then Begin

        Entity := ALCopyStr(
                            Src,
                            CurrentSrcPos+1,
                            2
                           );

        If tryStrtoint(alUpperCase('$'+Entity),in1) and (in1 <= 255) and (in1 >= 0) then CopyCharToResult(Char(in1), CurrentSrcPos+3)
        else CopyCurrentSrcPosCharToResult;

      end
      else CopyCurrentSrcPosCharToResult;

    end
    else If src[CurrentSrcPos]= '_' then CopyCharToResult(' ', CurrentSrcPos+1)
    else CopyCurrentSrcPosCharToResult;

  end;

  setLength(Result,CurrentResultPos-1);
end;

{******************************************************************************************************}
Function AlDecodeInternetMessageHeaderInUTF8(aHeaderStr: String; aDefaultCodePage: Integer): UTF8String;

Var P1, P2, P3, P4: integer;
    I: integer;
    aCharSet: String;
    aEncoding: String;
    aencodedText: String;
    aCodePage: integer;
    LstEncodedWord: TstringList;

Begin
  // encoded-word = "=?" charset "?" encoding "?" encoded-text "?="
  // =?iso-8859-1?q?=20this=20is=20some=20text?=
  Result := '';
  LstEncodedWord := TstringList.Create;
  Try

    P1 := 1;
    While True do begin
      P2 := AlPosEx('=?',aHeaderStr,P1);
      If P2 <= 0 then Break;
      P3 := ALCharPosEX('?', aHeaderStr, 3, P2+2);
      If (P3 <= 0) or (P3=Length(aHeaderStr)) or (aHeaderStr[P3+1] <> '=') then begin
        LstEncodedWord.Add(AlCopyStr(aHeaderStr,P1,P2+2-P1));
        P1 := P1 + 2;
      end
      else begin
        LstEncodedWord.Add(AlCopyStr(aHeaderStr,P1,P2-P1));
        LstEncodedWord.Add(AlCopyStr(aHeaderStr,P2,P3+2-P2));
        P1 := P3+2;
      end;
    end;
    LstEncodedWord.Add(AlCopyStr(aHeaderStr,P1,MaxInt));

    For i := 0 to LstEncodedWord.Count - 1 do begin
      aencodedText := LstEncodedWord[i];
      aCodePage := aDefaultCodePage;
      LstEncodedWord.Objects[i] := Pointer(0);
      If aEncodedText <> '' then begin
        P1 := AlPos('=?', aEncodedText);
        If P1 = 1 then begin
          P2 := AlCharPosEx('?',aEncodedText,P1+2);
          If (P2 > 0) then Begin
            P3 := AlCharPosEx('?',aEncodedText,P2+1);
            If P3 > 0 then begin
              P4 := AlPosEx('?=',aEncodedText,P3+1);
              If P4 > 0 then Begin
                aEncoding := alLowerCase(AlcopyStr(aEncodedText,P2+1,P3-P2-1)); //q
                If (aEncoding='q') or (aEncoding='b') then begin
                  aCharSet := AlcopyStr(aEncodedText,P1+2,P2-P1-2); //iso-8859-1
                  aCodePage := ALGetCodePageFromName(Acharset); //28591
                  aencodedText := AlcopyStr(aEncodedText,P3+1,P4-P3-1); //this=20is=20some=20text
                  If (aEncoding='b') then aencodedText := ALMimeBase64DecodeString(aencodedText)
                  else aencodedText := ALDecodeQuotedPrintableString(aencodedText);
                  LstEncodedWord.Objects[i] := Pointer(1);
                end;
              end;
            end;
          end;
        end;
        If aCodePage <> 65001 then aencodedText := ALUTF8Encode(aEncodedText, aCodePage);
        LstEncodedWord[i] := aencodedText;
        If (I >= 2) and
           (LstEncodedWord.Objects[i] = Pointer(1)) and
           (LstEncodedWord.Objects[i-2] = Pointer(1)) and
           (Trim(LstEncodedWord[i-1]) = '') then LstEncodedWord[i-1] := '';
      end;
    end;
    For i := 0 to LstEncodedWord.Count - 1 do Result := Result + LstEncodedWord[i];

  finally
    LstEncodedWord.Free;
  end;
end;




/////////////////////////////////////////
////////// TALEmailHeader //////////
/////////////////////////////////////////

{***************************************************}
procedure TALEmailHeader.AssignTo(Dest: TPersistent);
begin
  if Dest is TALEmailHeader then begin
    with Dest as TALEmailHeader do begin
      fSendTo := self.fSendTo;
      fSender := self.fSender;
      fMessageID := self.fMessageID;
      fbcc := self.fbcc;
      fContentTransferEncoding := self.fContentTransferEncoding;
      fComments := self.fComments;
      fMIMEVersion := self.fMIMEVersion;
      fPriority := self.fPriority;
      fReplyTo := self.fReplyTo;
      fSubject := self.fSubject;
      fFrom := self.fFrom;
      fDate := self.fDate;
      fDispositionNotificationTo := self.fDispositionNotificationTo;
      fReferences := self.fReferences;
      fcc := self.fcc;
      fContentType := self.fContentType;
      FCustomHeaders.Assign(Self.FCustomHeaders);
    end;
  end
  else inherited AssignTo(Dest);
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
  FCustomHeaders:= TstringList.create;
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

{***********************************************}
function TALEmailHeader.GetRawHeaderText: String;
Var i : integer;
    Str: String;
begin
  Result := '';
  If Trim(fFrom) <> '' then result := result + 'From: ' + trim(fFrom) + #13#10;
  If Trim(fSender) <> '' then result := result + 'Sender: ' + trim(fSender) + #13#10;
  If Trim(fSendTo) <> '' then result := result + 'To: ' + trim(fSendTo) + #13#10;
  If Trim(fcc) <> '' then result := result + 'cc: ' + trim(fcc) + #13#10;
  If Trim(fbcc) <> '' then result := result + 'bcc: ' + trim(fbcc) + #13#10;
  If Trim(fReplyTo) <> '' then result := result + 'Reply-To: ' + trim(fReplyTo) + #13#10;
  If Trim(fSubject) <> '' then result := result + 'Subject: ' + trim(fSubject) + #13#10;
  Str := fMessageID;
  If Trim(str) <> '' then begin
    If sametext(Str, 'AUTO') then Str := '<' + AlGenerateInternetMessageID + '>';
    result := result + 'Message-ID: ' + trim(str) + #13#10;
  end;
  If Trim(fReferences) <> '' then result := result + 'References: ' + trim(fReferences) + #13#10;
  If Trim(fComments) <> '' then result := result + 'Comments: ' + trim(fComments) + #13#10;
  Str := fDate;
  If Trim(str) <> '' then begin
    If sametext(Str, 'NOW') then Str := ALDateTimeToRfc822Str(Now);
    result := result + 'Date: ' + trim(str) + #13#10;
  end;
  If Trim(fContentType) <> '' then result := result + 'Content-Type: ' + trim(fContentType) + #13#10;
  If Trim(fContentTransferEncoding) <> '' then result := result + 'Content-Transfer-Encoding: ' + trim(fContentTransferEncoding) + #13#10;
  If Trim(fMIMEVersion) <> '' then result := result + 'MIME-Version: ' + trim(fMIMEVersion) + #13#10;
  If Trim(fPriority) <> '' then result := result + 'Priority: ' + trim(fPriority) + #13#10;
  If Trim(fDispositionNotificationTo) <> '' then result := result + 'Disposition-Notification-To: ' + trim(fDispositionNotificationTo) + #13#10;
  For i := 0 to FCustomHeaders.count - 1 do
    if (trim(FCustomHeaders.names[i]) <> '') and (trim(FCustomHeaders.ValueFromIndex[i]) <> '') then
      result := result + FCustomHeaders.names[i] + ': ' + trim(FCustomHeaders.ValueFromIndex[i]) + #13#10;
end;

{**********************************************************************}
procedure TALEmailHeader.SetRawHeaderText(const aRawHeaderText: string);
Var aRawHeaderLst: TstringList;

  {-------------------------------------}
  Function AlG001(aName: String): String;
  Var i: Integer;
      Str: String;
  Begin
    I := aRawHeaderLst.IndexOfName(aName);
    If I >= 0 then Begin
      result := Trim(aRawHeaderLst.ValueFromIndex[i]);
      aRawHeaderLst.Delete(i);
      While True do begin
        If i >= aRawHeaderLst.Count then break;
        str := aRawHeaderLst[i];
        If (str = '') or
           (not (str[1] in [' ',#9])) then break; //(1) an empty line or (2) a line that does not start with a space, a tab, or a field name followed by a colon
        Result := trim(result + ' ' + trim(str));
        aRawHeaderLst.Delete(i);
      end;
    end
    else result := '';
  end;

Var Str1, Str2: String;
    j: integer;
begin
  Clear;
  aRawHeaderLst := TstringList.create;
  try
    aRawHeaderLst.NameValueSeparator := ':';
    aRawHeaderLst.Text := aRawHeaderText;

    fFrom:= Alg001('From');
    fSender:= Alg001('Sender');
    fSendTo:= Alg001('To');
    fcc:= Alg001('cc');
    fbcc:= Alg001('bcc');
    fReplyTo:= Alg001('Reply-To');
    fSubject:= Alg001('Subject');
    fMessageID:= Alg001('Message-ID');
    fReferences:= Alg001('References');
    fComments:= Alg001('Comments');
    fDate:= Alg001('Date');
    fContentType:= Alg001('Content-Type');
    fContentTransferEncoding:= Alg001('Content-Transfer-Encoding');
    fMIMEVersion:= Alg001('MIME-Version');
    fPriority:= Alg001('Priority');
    fDispositionNotificationTo:= Alg001('Disposition-Notification-To');

    FCustomHeaders.clear;
    J := 0;
    while j <= aRawHeaderLst.count - 1 do begin
      Str1 := trim(aRawHeaderLst.Names[j]);
      If (trim(str1) <> '') and (not (str1[1] in [' ',#9])) then begin
        Str1 := trim(Str1) + ': ' + trim(aRawHeaderLst.ValueFromIndex[j]);
        inc(j);
        While True do begin
          If j >= aRawHeaderLst.Count then break;
          str2 := aRawHeaderLst[j];
          If (str2 = '') or
             (not (str2[1] in [' ',#9])) then break; //(1) an empty line or (2) a line that does not start with a space, a tab, or a field name followed by a colon
          Str1 := trim(Str1 + ' ' + trim(str2));
          inc(j);
        end;
        FCustomHeaders.Add(Str1);
      end
      else inc(j);
    end;

  finally
    aRawHeaderLst.Free;
  end;
end;




//////////////////////////////////
////// TALNewsArticleHeader //////
//////////////////////////////////

{*********************************************************}
procedure TALNewsArticleHeader.AssignTo(Dest: TPersistent);
begin
  if Dest is TALNewsArticleHeader then begin
    with Dest as TALNewsArticleHeader do begin
      fExpires := self.fExpires;
      fMessageID := self.fMessageID;
      fReplyTo := self.fReplyTo;
      fOrganization := self.fOrganization;
      fDateReceived := self.fDateReceived;
      fNNTPPostingHost := self.fNNTPPostingHost;
      fContentTransferEncoding := self.fContentTransferEncoding;
      fComments := self.fComments;
      fMIMEVersion := self.fMIMEVersion;
      fSender := self.fSender;
      fNewsgroups := self.fNewsgroups;
      fReferences := self.fReferences;
      fPostingVersion := self.fPostingVersion;
      fRelayVersion := self.fRelayVersion;
      fDate := self.fDate;
      fNNTPPostingDate := self.fNNTPPostingDate;
      fPath := self.fPath;
      fDistribution := self.fDistribution;
      fContentType := self.fContentType;
      fFollowupTo := self.fFollowupTo;
      fSubject := self.fSubject;
      fControl := self.fControl;
      fFrom := self.fFrom;
      FCustomHeaders.assign(Self.FCustomHeaders);
    end;
  end
  else inherited AssignTo(Dest);
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
  FCustomHeaders:= TstringList.create;
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

{*****************************************************}
function TALNewsArticleHeader.GetRawHeaderText: String;
Var i : integer;
    Str: String;
begin
  Result := '';
  If Trim(fRelayVersion) <> '' then result := result + 'Relay-Version: ' + trim(fRelayVersion) + #13#10;
  If Trim(fPostingVersion) <> '' then result := result + 'Posting-Version: ' + trim(fPostingVersion) + #13#10;
  If Trim(fFrom) <> '' then result := result + 'From: ' + trim(fFrom) + #13#10;
  Str := fDate;
  If Trim(str) <> '' then begin
    If sametext(Str, 'NOW') then Str := ALDateTimeToRfc822Str(Now);
    result := result + 'Date: ' + trim(str) + #13#10;
  end;
  If Trim(fNewsgroups) <> '' then result := result + 'Newsgroups: ' + trim(fNewsgroups) + #13#10;
  If Trim(fSubject) <> '' then result := result + 'Subject: ' + trim(fSubject) + #13#10;
  Str := fMessageID;
  If Trim(str) <> '' then begin
    If sametext(Str, 'AUTO') then Str := '<' + AlGenerateInternetMessageID + '>';
    result := result + 'Message-ID: ' + trim(str) + #13#10;
  end;
  If Trim(fPath) <> '' then result := result + 'Path: ' + trim(fPath) + #13#10;
  If Trim(fReplyTo) <> '' then result := result + 'Reply-To: ' + trim(fReplyTo) + #13#10;
  If Trim(fSender) <> '' then result := result + 'Sender: ' + trim(fSender) + #13#10;
  If Trim(fFollowupTo) <> '' then result := result + 'Followup-To: ' + trim(fFollowupTo) + #13#10;
  If Trim(fDateReceived) <> '' then result := result + 'Date-Received: ' + trim(fDateReceived) + #13#10;
  If Trim(fExpires) <> '' then result := result + 'Expires: ' + trim(fExpires) + #13#10;
  If Trim(fReferences) <> '' then result := result + 'References: ' + trim(fReferences) + #13#10;
  If Trim(fControl) <> '' then result := result + 'Control: ' + trim(fControl) + #13#10;
  If Trim(fDistribution) <> '' then result := result + 'Distribution: ' + trim(fDistribution) + #13#10;
  If Trim(fOrganization) <> '' then result := result + 'Organization: ' + trim(fOrganization) + #13#10;
  If Trim(fComments) <> '' then result := result + 'Comments: ' + trim(fComments) + #13#10;
  If Trim(fContentType) <> '' then result := result + 'Content-Type: ' + trim(fContentType) + #13#10;
  If Trim(fContentTransferEncoding) <> '' then result := result + 'Content-Transfer-Encoding: ' + trim(fContentTransferEncoding) + #13#10;
  If Trim(fMIMEVersion) <> '' then result := result + 'MIME-Version: ' + trim(fMIMEVersion) + #13#10;
  If Trim(fNNTPPostingHost) <> '' then result := result + 'NNTP-Posting-Host: ' + trim(fNNTPPostingHost) + #13#10;
  If Trim(fNNTPPostingDate) <> '' then result := result + 'NNTP-Posting-Date: ' + trim(fNNTPPostingDate) + #13#10;
  For i := 0 to FCustomHeaders.count - 1 do
    if (trim(FCustomHeaders.names[i]) <> '') and (trim(FCustomHeaders.ValueFromIndex[i]) <> '') then
      result := result + FCustomHeaders.names[i] + ': ' + trim(FCustomHeaders.ValueFromIndex[i]) + #13#10;
end;

{****************************************************************************}
procedure TALNewsArticleHeader.SetRawHeaderText(const aRawHeaderText: string);
Var aRawHeaderLst: TstringList;

  {-------------------------------------}
  Function AlG001(aName: String): String;
  Var i: Integer;
      Str: String;
  Begin
    I := aRawHeaderLst.IndexOfName(aName);
    If I >= 0 then Begin
      result := Trim(aRawHeaderLst.ValueFromIndex[i]);
      aRawHeaderLst.Delete(i);
      While True do begin
        If i >= aRawHeaderLst.Count then break;
        str := aRawHeaderLst[i];
        If (str = '') or
           (not (str[1] in [' ',#9])) then break; //(1) an empty line or (2) a line that does not start with a space, a tab, or a field name followed by a colon
        Result := trim(result + ' ' + trim(str));
        aRawHeaderLst.Delete(i);
      end;
    end
    else result := '';
  end;

Var Str1, Str2: String;
    j: integer;
begin
  Clear;
  aRawHeaderLst := TstringList.create;
  try
    aRawHeaderLst.NameValueSeparator := ':';
    aRawHeaderLst.Text := aRawHeaderText;

    fRelayVersion:= Alg001('Relay-Version');
    fPostingVersion:= Alg001('Posting-Version');
    fFrom:= Alg001('From');
    fDate:= Alg001('Date');
    fNewsgroups:= Alg001('Newsgroups');
    fSubject:= Alg001('Subject');
    fMessageID:= Alg001('Message-ID');
    fPath:= Alg001('Path');
    fReplyTo:= Alg001('Reply-To');
    fSender:= Alg001('Sender');
    fFollowupTo:= Alg001('Followup-To');
    fDateReceived:= Alg001('Date-Received');
    fExpires:= Alg001('Expires');
    fReferences:= Alg001('References');
    fControl:= Alg001('Control');
    fDistribution:= Alg001('Distribution');
    fOrganization:= Alg001('Organization');
    fComments:= Alg001('Comments');
    fContentType:= Alg001('Content-Type');
    fContentTransferEncoding:= Alg001('Content-Transfer-Encoding');
    fMIMEVersion:= Alg001('MIME-Version');
    fNNTPPostingHost:= Alg001('NNTP-Posting-Host');
    fNNTPPostingDate:= Alg001('NNTP-Posting-Date');

    FCustomHeaders.clear;
    J := 0;
    while j <= aRawHeaderLst.count - 1 do begin
      Str1 := trim(aRawHeaderLst.Names[j]);
      If (trim(str1) <> '') and (not (str1[1] in [' ',#9])) then begin
        Str1 := trim(Str1) + ': ' + trim(aRawHeaderLst.ValueFromIndex[j]);
        inc(j);
        While True do begin
          If j >= aRawHeaderLst.Count then break;
          str2 := aRawHeaderLst[j];
          If (str2 = '') or
             (not (str2[1] in [' ',#9])) then break; //(1) an empty line or (2) a line that does not start with a space, a tab, or a field name followed by a colon
          Str1 := trim(Str1 + ' ' + trim(str2));
          inc(j);
        end;
        FCustomHeaders.Add(Str1);
      end
      else inc(j);
    end;

  finally
    aRawHeaderLst.Free;
  end;
end;

end.
 