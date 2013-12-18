{*************************************************************
www:          http://sourceforge.net/projects/alcinoe/              
svn:          svn checkout svn://svn.code.sf.net/p/alcinoe/code/ alcinoe-code              
Author(s):    St�phane Vander Clock (alcinoe@arkadia.com)
Sponsor(s):   Arkadia SA (http://www.arkadia.com)
							
product:      Alcinoe Internet Messages Utilities
Version:      4.00

Description:  This unit contains utilities to manipulate
              Internet Messages Headers (EMAIL or NEWSNET messages)

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

History :     05/02/2008: add soft line break in ALDecodeQuotedPrintableString
              26/06/2012: Add xe2 support

Link :        http://www.faqs.org/rfcs/rfc2047.html

* Please send all your feedback to alcinoe@arkadia.com
* If you have downloaded this source from a website different from 
  sourceforge.net, please get the last version on http://sourceforge.net/projects/alcinoe/
* Please, help us to keep the development of these components free by 
  promoting the sponsor on http://static.arkadia.com/html/alcinoe_like.html
**************************************************************}
unit ALInternetMessages;

interface

{$IF CompilerVersion >= 25} {Delphi XE4}
  {$LEGACYIFEND ON} // http://docwiki.embarcadero.com/RADStudio/XE4/en/Legacy_IFEND_(Delphi)
{$IFEND}

uses {$IF CompilerVersion >= 23} {Delphi XE2}
     system.Classes,
     {$ELSE}
     Classes,
     {$IFEND}
     AlStringList;

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
Function AlGenerateInternetMessageID(ahostname: AnsiString): AnsiString; overload;
Function ALDecodeQuotedPrintableString(src: AnsiString): AnsiString;
Function AlDecodeInternetMessageHeaderInUTF8(const aHeaderStr: AnsiString; aDefaultCodePage: Integer): AnsiString;
function AlIsValidEmail(const Value: AnsiString): boolean;

implementation

uses {$IF CompilerVersion >= 23} {Delphi XE2}
     system.Sysutils,
     {$ELSE}
     Sysutils,
     {$IFEND}
     ALHttpClient,
     ALMime,
     ALWinsock,
     ALString;

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

  {----------}
  FriendlyEmail := ALTrim(FriendlyEmail);

  {----------}
  Result := '';
  RealName := '';

  {----------}
  ln := Length(FriendlyEmail);

  {----------}
  if ALMatchesMask(FriendlyEmail, '* <*>') then P1 := Ln-1  // toto <toto@toto.com> | "toto" <toto@toto.com> | (toto) <toto@toto.com>
  else if ALMatchesMask(FriendlyEmail, '<*> *') then P1 := 2 // <toto@toto.com> toto.com | <toto@toto.com> "toto" | <toto@toto.com> (toto)
  else if ALMatchesMask(FriendlyEmail, '<*>') then P1 := 2 // <toto@toto.com>
  else if ALMatchesMask(FriendlyEmail, '* (*)') then P1 := 1 // toto@toto.com (toto)
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

  {----------}
  RealName := ALTrim(AlCopyStr(FriendlyEmail,1,P2) + AlCopyStr(FriendlyEmail,P3,Maxint));

  {----------}
  If (RealName = '') then begin
    If (result <> '') then exit;
    RealName := ALTrim(FriendlyEmail);
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
          (((RealName[1] = '(') and (RealName[ln] = ')')) or
           ((RealName[1] = '<') and (RealName[ln] = '>')) or
           ((RealName[1] = '"') and (RealName[ln] = '"'))) then RealName := alCopyStr(RealName,2,Ln-2)

end;

{**************************************************************************}
function AlExtractEmailAddress(const FriendlyEmail: AnsiString): AnsiString;
Var aRealName: AnsiString;
Begin
  Result := AlParseEmailAddress(FriendlyEmail, aRealName, False);
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
  Result := AlStringReplace(ALMakeKeyStrByGUID,'-','',[rfReplaceAll]) + '@' + ALTrim(AlGetLocalHostName);
end;

{**********************************************************************}
Function AlGenerateInternetMessageID(ahostname: AnsiString): AnsiString;
Begin
  ahostname := ALTrim(ahostname);
  If ahostname <> '' then Result := AlStringReplace(ALMakeKeyStrByGUID,'-','',[rfReplaceAll]) + '@' + ahostname
  else Result := AlGenerateInternetMessageID;
end;

{******************************************************************}
Function ALDecodeQuotedPrintableString(src: AnsiString): AnsiString;
var CurrentSrcPos, CurrentResultPos : Integer;
    Entity : AnsiString;
    SrcLength: integer;
    in1: integer;

    {--------------------------------------}
    procedure CopyCurrentSrcPosCharToResult;
    Begin
      result[CurrentResultPos] := src[CurrentSrcPos];
      inc(CurrentResultPos);
      inc(CurrentSrcPos);
    end;

    {---------------------------------------------------------------------}
    procedure CopyCharToResult(aChar: AnsiChar; NewCurrentSrcPos: integer);
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

        Entity := ALCopyStr(Src,
                            CurrentSrcPos+1,
                            2);

        If ALTryStrToInt(alUpperCase('$'+Entity),in1) and (in1 <= 255) and (in1 >= 0) then CopyCharToResult(AnsiChar(in1), CurrentSrcPos+3)
        else CopyCurrentSrcPosCharToResult;

      end
      else CopyCurrentSrcPosCharToResult;

    end
    else If src[CurrentSrcPos]= '_' then CopyCharToResult(' ', CurrentSrcPos+1)
    else CopyCurrentSrcPosCharToResult;

  end;

  setLength(Result,CurrentResultPos-1);
end;

{****************************************************************************************************************}
Function AlDecodeInternetMessageHeaderInUTF8(const aHeaderStr: AnsiString; aDefaultCodePage: Integer): AnsiString;

Var P1, P2, P3, P4: integer;
    I: integer;
    aCharSet: AnsiString;
    aEncoding: AnsiString;
    aencodedText: AnsiString;
    aCodePage: integer;
    LstEncodedWord: TALStringList;

Begin
  // encoded-word = "=?" charset "?" encoding "?" encoded-text "?="
  // =?iso-8859-1?q?=20this=20is=20some=20text?=
  Result := '';
  LstEncodedWord := TALStringList.Create;
  Try

    P1 := 1;
    While True do begin
      P2 := AlPosEx('=?',aHeaderStr,P1);
      If P2 <= 0 then Break;
      P3 := ALPosEX('?', aHeaderStr, P2+2);
      if P3 > 0 then P3 := ALPosEX('?', aHeaderStr, P3+1);
      if P3 > 0 then P3 := ALPosEX('?', aHeaderStr, P3+1);
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
          P2 := AlPosEx('?',aEncodedText,P1+2);
          If (P2 > 0) then Begin
            P3 := AlPosEx('?',aEncodedText,P2+1);
            If P3 > 0 then begin
              P4 := AlPosEx('?=',aEncodedText,P3+1);
              If P4 > 0 then Begin
                aEncoding := alLowerCase(AlcopyStr(aEncodedText,P2+1,P3-P2-1)); //q
                If (aEncoding='q') or (aEncoding='b') then begin
                  aCharSet := AlcopyStr(aEncodedText,P1+2,P2-P1-2); //iso-8859-1
                  aCodePage := ALGetCodePageFromCharSetName(Acharset); //28591
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
           (ALTrim(LstEncodedWord[i-1]) = '') then LstEncodedWord[i-1] := '';
      end;
    end;
    For i := 0 to LstEncodedWord.Count - 1 do Result := Result + LstEncodedWord[i];

  finally
    LstEncodedWord.Free;
  end;
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
Var i : integer;
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
  For i := 0 to FCustomHeaders.count - 1 do
    if (ALTrim(FCustomHeaders.names[i]) <> '') and (ALTrim(FCustomHeaders.ValueFromIndex[i]) <> '') then
      result := result + FCustomHeaders.names[i] + ': ' + ALTrim(FCustomHeaders.ValueFromIndex[i]) + #13#10;
end;

{**************************************************************************}
procedure TALEmailHeader.SetRawHeaderText(const aRawHeaderText: AnsiString);
Var aRawHeaderLst: TALStringList;

  {---------------------------------------------}
  Function AlG001(aName: AnsiString): AnsiString;
  Var i: Integer;
      Str: AnsiString;
  Begin
    I := aRawHeaderLst.IndexOfName(aName);
    If I >= 0 then Begin
      result := ALTrim(aRawHeaderLst.ValueFromIndex[i]);
      aRawHeaderLst.Delete(i);
      While True do begin
        If i >= aRawHeaderLst.Count then break;
        str := aRawHeaderLst[i];
        If (str = '') or
           (not (str[1] in [' ',#9])) then break; //(1) an empty line or (2) a line that does not start with a space, a tab, or a field name followed by a colon
        Result := ALTrim(result + ' ' + ALTrim(str));
        aRawHeaderLst.Delete(i);
      end;
    end
    else result := '';
  end;

Var Str1, Str2: AnsiString;
    j: integer;
begin
  Clear;
  aRawHeaderLst := TALStringList.create;
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
      Str1 := ALTrim(aRawHeaderLst.Names[j]);
      If (ALTrim(str1) <> '') and (not (str1[1] in [' ',#9])) then begin
        Str1 := ALTrim(Str1) + ': ' + ALTrim(aRawHeaderLst.ValueFromIndex[j]);
        inc(j);
        While True do begin
          If j >= aRawHeaderLst.Count then break;
          str2 := aRawHeaderLst[j];
          If (str2 = '') or
             (not (str2[1] in [' ',#9])) then break; //(1) an empty line or (2) a line that does not start with a space, a tab, or a field name followed by a colon
          Str1 := ALTrim(Str1 + ' ' + ALTrim(str2));
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
Var i : integer;
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
  For i := 0 to FCustomHeaders.count - 1 do
    if (ALTrim(FCustomHeaders.names[i]) <> '') and (ALTrim(FCustomHeaders.ValueFromIndex[i]) <> '') then
      result := result + FCustomHeaders.names[i] + ': ' + ALTrim(FCustomHeaders.ValueFromIndex[i]) + #13#10;
end;

{********************************************************************************}
procedure TALNewsArticleHeader.SetRawHeaderText(const aRawHeaderText: AnsiString);
Var aRawHeaderLst: TALStringList;

  {---------------------------------------------}
  Function AlG001(aName: AnsiString): AnsiString;
  Var i: Integer;
      Str: AnsiString;
  Begin
    I := aRawHeaderLst.IndexOfName(aName);
    If I >= 0 then Begin
      result := ALTrim(aRawHeaderLst.ValueFromIndex[i]);
      aRawHeaderLst.Delete(i);
      While True do begin
        If i >= aRawHeaderLst.Count then break;
        str := aRawHeaderLst[i];
        If (str = '') or
           (not (str[1] in [' ',#9])) then break; //(1) an empty line or (2) a line that does not start with a space, a tab, or a field name followed by a colon
        Result := ALTrim(result + ' ' + ALTrim(str));
        aRawHeaderLst.Delete(i);
      end;
    end
    else result := '';
  end;

Var Str1, Str2: AnsiString;
    j: integer;
begin
  Clear;
  aRawHeaderLst := TALStringList.create;
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
      Str1 := ALTrim(aRawHeaderLst.Names[j]);
      If (ALTrim(str1) <> '') and (not (str1[1] in [' ',#9])) then begin
        Str1 := ALTrim(Str1) + ': ' + ALTrim(aRawHeaderLst.ValueFromIndex[j]);
        inc(j);
        While True do begin
          If j >= aRawHeaderLst.Count then break;
          str2 := aRawHeaderLst[j];
          If (str2 = '') or
             (not (str2[1] in [' ',#9])) then break; //(1) an empty line or (2) a line that does not start with a space, a tab, or a field name followed by a colon
          Str1 := ALTrim(Str1 + ' ' + ALTrim(str2));
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

{********************************************************}
function AlIsValidEmail(const Value: AnsiString): boolean;

 {------------------------------------------------------}
 function CheckAllowedName(const s: AnsiString): boolean;
 var i: integer;
 begin
   Result:= false;
   for i:= 1 to Length(s) do begin
     // illegal char in s -> no valid address
     if not (s[i] in ['a'..'z','A'..'Z','0'..'9','_','-','.','+']) then Exit;
   end;
   Result:= true;
 end;

 {----------------------------------------------------------}
 function CheckAllowedHostname(const s: AnsiString): boolean;
 var i: integer;
 begin
   Result:= false;
   for i:= 1 to Length(s) do begin
     // illegal char in s -> no valid address
     if not (s[i] in ['a'..'z','A'..'Z','0'..'9','-','.']) then Exit;
   end;
   Result:= true;
 end;

 {-----------------------------------------------------}
 function CheckAllowedExt(const s: AnsiString): boolean;
 var i: integer;
 begin
   Result:= false;
   for i:= 1 to Length(s) do begin
     // illegal char in s -> no valid address
     if not (s[i] in ['a'..'z','A'..'Z']) then Exit;
   end;
   Result:= true;
 end;

var i, j: integer;
    namePart, serverPart, extPart: AnsiString;
begin
  Result := false;

  // Value can not be < 6 char (ex: a@b.fr)
  if length(Value) < 6 then exit;

  // must have the '@' char inside
  i := AlPos('@', Value);
  if (i <= 1) or (i > length(Value)-4) then exit;

  //can not have @. or .@
  if (value[i-1] = '.') or (value[i+1] = '.') then exit;

  //can not have 2 ..
  If (ALpos('..', Value) > 0) then Exit;

  //extract namePart and serverPart
  namePart:= AlCopyStr(Value, 1, i - 1);
  serverPart:= AlCopyStr(Value, i + 1, Length(Value));

  // Extension (.fr, .com, etc..) must be betwen 2 to 6 char
  i:= AlPos('.', serverPart);
  j := 0;
  While I > 0 do begin
    j := i;
    I := AlPosEx('.', serverPart, i + 1);
  end;
  if (j <= 1) then Exit; // no dot at all so exit !
  extPart    := AlCopyStr(ServerPart,J+1,Maxint);
  serverPart := ALCopyStr(ServerPart, 1, J - 1);
  If not (Length(ExtPart) in [2..6]) then exit;

  Result:= CheckAllowedname(namePart) and
           CheckAllowedHostname(serverPart) and
           CheckAllowedExt(ExtPart);
end;

end.
