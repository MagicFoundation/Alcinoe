{*******************************************************************************
The TAlGSMComm component implements SMS text messaging
through the text-mode interface defined in the GSM
Technical Specification 07.05, version 5.1.0, dated
December 1996.  There are several variations of this spec,
used throughout Nokia, Siemens, Ericsson, etc models.
We have tested the Nokia 6230 in-house, but the Nokia 7190,
8890, 6210 and 9110 models should work as well.  Phones
from other manufacturers will also work, as long as they
implement the text-mode interface.  About 1/4 of the
current phones are capable of being connected to a PC
(through IR or serial cable), about 1/3 of those are
text-mode only, 1/3 are PDU mode only, and the other 1/3
support both text and PDU mode.  Some phones (such as the
Nokia 5190) support SMS, but they use a proprietary protocol,
which TALGSMComm does not support.

To test your phone, connect the phone to your PC through
the serial cable or IR device (consult your phone's documentation
for details on how to connect). Enter "AT"<CR> into a terminal
window to verify the connection is established (you should receive
"OK" from the phone), then enter "AT+CMGF=?"<CR>. The response
should contain a "1", indicating that it supports text-mode.
If both of these tests pass, then your phone meets the basic
requirements.

http://www.nobbi.com/pduspy.htm
http://rednaxela.net/pdu.php
http://www.dreamfabric.com/sms/
*******************************************************************************}

unit ALGSMComm;

interface

uses
  Winapi.Windows,
  AlStringList;

Type

  {-------------------------}
  TAlGSMComm = Class(Tobject)
  Private
    FSerial: THandle;
    FConnected: Boolean;
    FBaudRate: Dword;
    Ftimeout: Cardinal;
    Procedure InitCommTimeouts;
    procedure InitCommState;
    procedure SetBaudRate(const Value: Dword);
    procedure Settimeout(const Value: Cardinal);
  protected
    procedure CheckError(Error: Boolean);
    Function SerialWrite(Var Buffer; Count: Longint): Longint; Virtual;
    Function SerialRead(var Buffer; Count: Longint): Longint; Virtual;
  Public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Connect(const Serial: AnsiString); virtual;
    Procedure Disconnect; virtual;
    procedure SendCmd(const aCmd: AnsiString); Virtual;
    function GetResponse: AnsiString; virtual;
    Procedure GetATCmdOkResponse(Const ErrorMsg: AnsiString); overload;
    Procedure GetATCmdOkResponse(Var Response: AnsiString; Const ErrorMsg: AnsiString); overload;
    Procedure GetATCmdlinefeedResponse(Const ErrorMsg: AnsiString);
    procedure SendSMSinPDUMode(const aSMSCenter, aSMSAddress: ansiString; aMessage: AnsiString; const EncodeMessageInPDU: Boolean=True);
    procedure SendSMSinTextMode(const aSMSCenter, aSMSAddress, aMessage, aCharset: AnsiString);
    procedure ListAllSMSinPDUMode(aLstMessage: TALStrings; MemStorage: AnsiString);
    procedure DeleteSMS(aIndex: integer; MemStorage: AnsiString);
    property Connected: Boolean read FConnected;
    Property BaudRate: Dword read FBaudRate write SetBaudRate default CBR_2400;
    Property Timeout: Cardinal read Ftimeout write Settimeout default 60000;
  end;

{---------------------------------------------------------------------------------------------------------------}
function  AlGSMComm_BuildPDUMessage(aSMSCenter, aSMSAddress: ansiString; const aMessage: AnsiString): AnsiString;
Procedure AlGSMComm_DecodePDUMessage(const aPDUMessage: AnsiString; Var aSMSCenter, aSMSAddress, AMessage: AnsiString);
function  AlGSMComm_UnicodeToGSM7BitDefaultAlphabet(const aMessage: WideString): AnsiString;
function  AlGSMComm_GSM7BitDefaultAlphabetToUnicode(const aMessage: AnsiString; Const UseGreekAlphabet: Boolean= False): Widestring;

implementation

uses
  System.SysUtils,
  ALString;

{*****************************************************************************************}
function AlGSMComm_UnicodeToGSM7BitDefaultAlphabet(const aMessage: WideString): AnsiString;

  {-------------------------------------------------}
  Function InternalLookupChar(aUnicodeChar: WideChar;
                              Var aGSMString: AnsiString;
                              Var aGSMStringCurrentIndex: integer): Boolean;
  Begin
    Result := True;
    Case ord(aUnicodeChar) of
      $0040: aGSMString[aGSMStringCurrentIndex] := AnsiChar($00); // COMMERCIAL AT
      $0000: aGSMString[aGSMStringCurrentIndex] := AnsiChar($00); // NULL (see note above)
      $00A3: aGSMString[aGSMStringCurrentIndex] := AnsiChar($01); // POUND SIGN
      $0024: aGSMString[aGSMStringCurrentIndex] := AnsiChar($02); // DOLLAR SIGN
      $00A5: aGSMString[aGSMStringCurrentIndex] := AnsiChar($03); // YEN SIGN
      $00E8: aGSMString[aGSMStringCurrentIndex] := AnsiChar($04); // LATIN SMALL LETTER E WITH GRAVE
      $00E9: aGSMString[aGSMStringCurrentIndex] := AnsiChar($05); // LATIN SMALL LETTER E WITH ACUTE
      $00F9: aGSMString[aGSMStringCurrentIndex] := AnsiChar($06); // LATIN SMALL LETTER U WITH GRAVE
      $00EC: aGSMString[aGSMStringCurrentIndex] := AnsiChar($07); // LATIN SMALL LETTER I WITH GRAVE
      $00F2: aGSMString[aGSMStringCurrentIndex] := AnsiChar($08); // LATIN SMALL LETTER O WITH GRAVE
      $00E7: aGSMString[aGSMStringCurrentIndex] := AnsiChar($09); // LATIN SMALL LETTER C WITH CEDILLA
      $00C7: aGSMString[aGSMStringCurrentIndex] := AnsiChar($09); // LATIN CAPITAL LETTER C WITH CEDILLA (see note above)
      $000A: aGSMString[aGSMStringCurrentIndex] := AnsiChar($0A); // LINE FEED
      $00D8: aGSMString[aGSMStringCurrentIndex] := AnsiChar($0B); // LATIN CAPITAL LETTER O WITH STROKE
      $00F8: aGSMString[aGSMStringCurrentIndex] := AnsiChar($0C); // LATIN SMALL LETTER O WITH STROKE
      $000D: aGSMString[aGSMStringCurrentIndex] := AnsiChar($0D); // CARRIAGE RETURN
      $00C5: aGSMString[aGSMStringCurrentIndex] := AnsiChar($0E); // LATIN CAPITAL LETTER A WITH RING ABOVE
      $00E5: aGSMString[aGSMStringCurrentIndex] := AnsiChar($0F); // LATIN SMALL LETTER A WITH RING ABOVE
      $0394: aGSMString[aGSMStringCurrentIndex] := AnsiChar($10); // GREEK CAPITAL LETTER DELTA
      $005F: aGSMString[aGSMStringCurrentIndex] := AnsiChar($11); // LOW LINE
      $03A6: aGSMString[aGSMStringCurrentIndex] := AnsiChar($12); // GREEK CAPITAL LETTER PHI
      $0393: aGSMString[aGSMStringCurrentIndex] := AnsiChar($13); // GREEK CAPITAL LETTER GAMMA
      $039B: aGSMString[aGSMStringCurrentIndex] := AnsiChar($14); // GREEK CAPITAL LETTER LAMDA
      $03A9: aGSMString[aGSMStringCurrentIndex] := AnsiChar($15); // GREEK CAPITAL LETTER OMEGA
      $03A0: aGSMString[aGSMStringCurrentIndex] := AnsiChar($16); // GREEK CAPITAL LETTER PI
      $03A8: aGSMString[aGSMStringCurrentIndex] := AnsiChar($17); // GREEK CAPITAL LETTER PSI
      $03A3: aGSMString[aGSMStringCurrentIndex] := AnsiChar($18); // GREEK CAPITAL LETTER SIGMA
      $0398: aGSMString[aGSMStringCurrentIndex] := AnsiChar($19); // GREEK CAPITAL LETTER THETA
      $039E: aGSMString[aGSMStringCurrentIndex] := AnsiChar($1A); // GREEK CAPITAL LETTER XI
      $00A0: aGSMString[aGSMStringCurrentIndex] := AnsiChar($1B); // ESCAPE TO EXTENSION TABLE (or displayed as NBSP, see note above)
      $000C: Begin
               aGSMString[aGSMStringCurrentIndex] := AnsiChar($1B); // ESCAPE TO EXTENSION TABLE
               inc(aGSMStringCurrentIndex);
               aGSMString[aGSMStringCurrentIndex] := AnsiChar($0A); // FORM FEED
             end;
      $005E: Begin
               aGSMString[aGSMStringCurrentIndex] := AnsiChar($1B); // ESCAPE TO EXTENSION TABLE
               inc(aGSMStringCurrentIndex);
               aGSMString[aGSMStringCurrentIndex] := AnsiChar($14); // CIRCUMFLEX ACCENT
             end;
      $007B: Begin
               aGSMString[aGSMStringCurrentIndex] := AnsiChar($1B); // ESCAPE TO EXTENSION TABLE
               inc(aGSMStringCurrentIndex);
               aGSMString[aGSMStringCurrentIndex] := AnsiChar($28); // LEFT CURLY BRACKET
             end;
      $007D: Begin
               aGSMString[aGSMStringCurrentIndex] := AnsiChar($1B); // ESCAPE TO EXTENSION TABLE
               inc(aGSMStringCurrentIndex);
               aGSMString[aGSMStringCurrentIndex] := AnsiChar($29); // RIGHT CURLY BRACKET
             end;
      $005C: Begin
               aGSMString[aGSMStringCurrentIndex] := AnsiChar($1B); // ESCAPE TO EXTENSION TABLE
               inc(aGSMStringCurrentIndex);
               aGSMString[aGSMStringCurrentIndex] := AnsiChar($2F); // REVERSE SOLIDUS
             end;
      $005B: Begin
               aGSMString[aGSMStringCurrentIndex] := AnsiChar($1B); // ESCAPE TO EXTENSION TABLE
               inc(aGSMStringCurrentIndex);
               aGSMString[aGSMStringCurrentIndex] := AnsiChar($3C); // LEFT SQUARE BRACKET
             end;
      $007E: Begin
               aGSMString[aGSMStringCurrentIndex] := AnsiChar($1B); // ESCAPE TO EXTENSION TABLE
               inc(aGSMStringCurrentIndex);
               aGSMString[aGSMStringCurrentIndex] := AnsiChar($3D); // TILDE
             end;
      $005D: Begin
               aGSMString[aGSMStringCurrentIndex] := AnsiChar($1B); // ESCAPE TO EXTENSION TABLE
               inc(aGSMStringCurrentIndex);
               aGSMString[aGSMStringCurrentIndex] := AnsiChar($3E); // RIGHT SQUARE BRACKET
             end;
      $007C: Begin
               aGSMString[aGSMStringCurrentIndex] := AnsiChar($1B); // ESCAPE TO EXTENSION TABLE
               inc(aGSMStringCurrentIndex);
               aGSMString[aGSMStringCurrentIndex] := AnsiChar($40); // VERTICAL LINE
             end;
      $20AC: Begin
               aGSMString[aGSMStringCurrentIndex] := AnsiChar($1B); // ESCAPE TO EXTENSION TABLE
               inc(aGSMStringCurrentIndex);
               aGSMString[aGSMStringCurrentIndex] := AnsiChar($65); // EURO SIGN
             end;
      $00C6: aGSMString[aGSMStringCurrentIndex] := AnsiChar($1C); // LATIN CAPITAL LETTER AE
      $00E6: aGSMString[aGSMStringCurrentIndex] := AnsiChar($1D); // LATIN SMALL LETTER AE
      $00DF: aGSMString[aGSMStringCurrentIndex] := AnsiChar($1E); // LATIN SMALL LETTER SHARP S (German)
      $00C9: aGSMString[aGSMStringCurrentIndex] := AnsiChar($1F); // LATIN CAPITAL LETTER E WITH ACUTE
      $0020: aGSMString[aGSMStringCurrentIndex] := AnsiChar($20); // SPACE
      $0021: aGSMString[aGSMStringCurrentIndex] := AnsiChar($21); // EXCLAMATION MARK
      $0022: aGSMString[aGSMStringCurrentIndex] := AnsiChar($22); // QUOTATION MARK
      $0023: aGSMString[aGSMStringCurrentIndex] := AnsiChar($23); // NUMBER SIGN
      $00A4: aGSMString[aGSMStringCurrentIndex] := AnsiChar($24); // CURRENCY SIGN
      $0025: aGSMString[aGSMStringCurrentIndex] := AnsiChar($25); // PERCENT SIGN
      $0026: aGSMString[aGSMStringCurrentIndex] := AnsiChar($26); // AMPERSAND
      $0027: aGSMString[aGSMStringCurrentIndex] := AnsiChar($27); // APOSTROPHE
      $0028: aGSMString[aGSMStringCurrentIndex] := AnsiChar($28); // LEFT PARENTHESIS
      $0029: aGSMString[aGSMStringCurrentIndex] := AnsiChar($29); // RIGHT PARENTHESIS
      $002A: aGSMString[aGSMStringCurrentIndex] := AnsiChar($2A); // ASTERISK
      $002B: aGSMString[aGSMStringCurrentIndex] := AnsiChar($2B); // PLUS SIGN
      $002C: aGSMString[aGSMStringCurrentIndex] := AnsiChar($2C); // COMMA
      $002D: aGSMString[aGSMStringCurrentIndex] := AnsiChar($2D); // HYPHEN-MINUS
      $002E: aGSMString[aGSMStringCurrentIndex] := AnsiChar($2E); // FULL STOP
      $002F: aGSMString[aGSMStringCurrentIndex] := AnsiChar($2F); // SOLIDUS
      $0030: aGSMString[aGSMStringCurrentIndex] := AnsiChar($30); // DIGIT ZERO
      $0031: aGSMString[aGSMStringCurrentIndex] := AnsiChar($31); // DIGIT ONE
      $0032: aGSMString[aGSMStringCurrentIndex] := AnsiChar($32); // DIGIT TWO
      $0033: aGSMString[aGSMStringCurrentIndex] := AnsiChar($33); // DIGIT THREE
      $0034: aGSMString[aGSMStringCurrentIndex] := AnsiChar($34); // DIGIT FOUR
      $0035: aGSMString[aGSMStringCurrentIndex] := AnsiChar($35); // DIGIT FIVE
      $0036: aGSMString[aGSMStringCurrentIndex] := AnsiChar($36); // DIGIT SIX
      $0037: aGSMString[aGSMStringCurrentIndex] := AnsiChar($37); // DIGIT SEVEN
      $0038: aGSMString[aGSMStringCurrentIndex] := AnsiChar($38); // DIGIT EIGHT
      $0039: aGSMString[aGSMStringCurrentIndex] := AnsiChar($39); // DIGIT NINE
      $003A: aGSMString[aGSMStringCurrentIndex] := AnsiChar($3A); // COLON
      $003B: aGSMString[aGSMStringCurrentIndex] := AnsiChar($3B); // SEMICOLON
      $003C: aGSMString[aGSMStringCurrentIndex] := AnsiChar($3C); // LESS-THAN SIGN
      $003D: aGSMString[aGSMStringCurrentIndex] := AnsiChar($3D); // EQUALS SIGN
      $003E: aGSMString[aGSMStringCurrentIndex] := AnsiChar($3E); // GREATER-THAN SIGN
      $003F: aGSMString[aGSMStringCurrentIndex] := AnsiChar($3F); // QUESTION MARK
      $00A1: aGSMString[aGSMStringCurrentIndex] := AnsiChar($40); // INVERTED EXCLAMATION MARK
      $0041: aGSMString[aGSMStringCurrentIndex] := AnsiChar($41); // LATIN CAPITAL LETTER A
      $0391: aGSMString[aGSMStringCurrentIndex] := AnsiChar($41); // GREEK CAPITAL LETTER ALPHA
      $0042: aGSMString[aGSMStringCurrentIndex] := AnsiChar($42); // LATIN CAPITAL LETTER B
      $0392: aGSMString[aGSMStringCurrentIndex] := AnsiChar($42); // GREEK CAPITAL LETTER BETA
      $0043: aGSMString[aGSMStringCurrentIndex] := AnsiChar($43); // LATIN CAPITAL LETTER C
      $0044: aGSMString[aGSMStringCurrentIndex] := AnsiChar($44); // LATIN CAPITAL LETTER D
      $0045: aGSMString[aGSMStringCurrentIndex] := AnsiChar($45); // LATIN CAPITAL LETTER E
      $0395: aGSMString[aGSMStringCurrentIndex] := AnsiChar($45); // GREEK CAPITAL LETTER EPSILON
      $0046: aGSMString[aGSMStringCurrentIndex] := AnsiChar($46); // LATIN CAPITAL LETTER F
      $0047: aGSMString[aGSMStringCurrentIndex] := AnsiChar($47); // LATIN CAPITAL LETTER G
      $0048: aGSMString[aGSMStringCurrentIndex] := AnsiChar($48); // LATIN CAPITAL LETTER H
      $0397: aGSMString[aGSMStringCurrentIndex] := AnsiChar($48); // GREEK CAPITAL LETTER ETA
      $0049: aGSMString[aGSMStringCurrentIndex] := AnsiChar($49); // LATIN CAPITAL LETTER I
      $0399: aGSMString[aGSMStringCurrentIndex] := AnsiChar($49); // GREEK CAPITAL LETTER IOTA
      $004A: aGSMString[aGSMStringCurrentIndex] := AnsiChar($4A); // LATIN CAPITAL LETTER J
      $004B: aGSMString[aGSMStringCurrentIndex] := AnsiChar($4B); // LATIN CAPITAL LETTER K
      $039A: aGSMString[aGSMStringCurrentIndex] := AnsiChar($4B); // GREEK CAPITAL LETTER KAPPA
      $004C: aGSMString[aGSMStringCurrentIndex] := AnsiChar($4C); // LATIN CAPITAL LETTER L
      $004D: aGSMString[aGSMStringCurrentIndex] := AnsiChar($4D); // LATIN CAPITAL LETTER M
      $039C: aGSMString[aGSMStringCurrentIndex] := AnsiChar($4D); // GREEK CAPITAL LETTER MU
      $004E: aGSMString[aGSMStringCurrentIndex] := AnsiChar($4E); // LATIN CAPITAL LETTER N
      $039D: aGSMString[aGSMStringCurrentIndex] := AnsiChar($4E); // GREEK CAPITAL LETTER NU
      $004F: aGSMString[aGSMStringCurrentIndex] := AnsiChar($4F); // LATIN CAPITAL LETTER O
      $039F: aGSMString[aGSMStringCurrentIndex] := AnsiChar($4F); // GREEK CAPITAL LETTER OMICRON
      $0050: aGSMString[aGSMStringCurrentIndex] := AnsiChar($50); // LATIN CAPITAL LETTER P
      $03A1: aGSMString[aGSMStringCurrentIndex] := AnsiChar($50); // GREEK CAPITAL LETTER RHO
      $0051: aGSMString[aGSMStringCurrentIndex] := AnsiChar($51); // LATIN CAPITAL LETTER Q
      $0052: aGSMString[aGSMStringCurrentIndex] := AnsiChar($52); // LATIN CAPITAL LETTER R
      $0053: aGSMString[aGSMStringCurrentIndex] := AnsiChar($53); // LATIN CAPITAL LETTER S
      $0054: aGSMString[aGSMStringCurrentIndex] := AnsiChar($54); // LATIN CAPITAL LETTER T
      $03A4: aGSMString[aGSMStringCurrentIndex] := AnsiChar($54); // GREEK CAPITAL LETTER TAU
      $0055: aGSMString[aGSMStringCurrentIndex] := AnsiChar($55); // LATIN CAPITAL LETTER U
      $03A5: aGSMString[aGSMStringCurrentIndex] := AnsiChar($55); // GREEK CAPITAL LETTER UPSILON
      $0056: aGSMString[aGSMStringCurrentIndex] := AnsiChar($56); // LATIN CAPITAL LETTER V
      $0057: aGSMString[aGSMStringCurrentIndex] := AnsiChar($57); // LATIN CAPITAL LETTER W
      $0058: aGSMString[aGSMStringCurrentIndex] := AnsiChar($58); // LATIN CAPITAL LETTER X
      $03A7: aGSMString[aGSMStringCurrentIndex] := AnsiChar($58); // GREEK CAPITAL LETTER CHI
      $0059: aGSMString[aGSMStringCurrentIndex] := AnsiChar($59); // LATIN CAPITAL LETTER Y
      $005A: aGSMString[aGSMStringCurrentIndex] := AnsiChar($5A); // LATIN CAPITAL LETTER Z
      $0396: aGSMString[aGSMStringCurrentIndex] := AnsiChar($5A); // GREEK CAPITAL LETTER ZETA
      $00C4: aGSMString[aGSMStringCurrentIndex] := AnsiChar($5B); // LATIN CAPITAL LETTER A WITH DIAERESIS
      $00D6: aGSMString[aGSMStringCurrentIndex] := AnsiChar($5C); // LATIN CAPITAL LETTER O WITH DIAERESIS
      $00D1: aGSMString[aGSMStringCurrentIndex] := AnsiChar($5D); // LATIN CAPITAL LETTER N WITH TILDE
      $00DC: aGSMString[aGSMStringCurrentIndex] := AnsiChar($5E); // LATIN CAPITAL LETTER U WITH DIAERESIS
      $00A7: aGSMString[aGSMStringCurrentIndex] := AnsiChar($5F); // SECTION SIGN
      $00BF: aGSMString[aGSMStringCurrentIndex] := AnsiChar($60); // INVERTED QUESTION MARK
      $0061: aGSMString[aGSMStringCurrentIndex] := AnsiChar($61); // LATIN SMALL LETTER A
      $0062: aGSMString[aGSMStringCurrentIndex] := AnsiChar($62); // LATIN SMALL LETTER B
      $0063: aGSMString[aGSMStringCurrentIndex] := AnsiChar($63); // LATIN SMALL LETTER C
      $0064: aGSMString[aGSMStringCurrentIndex] := AnsiChar($64); // LATIN SMALL LETTER D
      $0065: aGSMString[aGSMStringCurrentIndex] := AnsiChar($65); // LATIN SMALL LETTER E
      $0066: aGSMString[aGSMStringCurrentIndex] := AnsiChar($66); // LATIN SMALL LETTER F
      $0067: aGSMString[aGSMStringCurrentIndex] := AnsiChar($67); // LATIN SMALL LETTER G
      $0068: aGSMString[aGSMStringCurrentIndex] := AnsiChar($68); // LATIN SMALL LETTER H
      $0069: aGSMString[aGSMStringCurrentIndex] := AnsiChar($69); // LATIN SMALL LETTER I
      $006A: aGSMString[aGSMStringCurrentIndex] := AnsiChar($6A); // LATIN SMALL LETTER J
      $006B: aGSMString[aGSMStringCurrentIndex] := AnsiChar($6B); // LATIN SMALL LETTER K
      $006C: aGSMString[aGSMStringCurrentIndex] := AnsiChar($6C); // LATIN SMALL LETTER L
      $006D: aGSMString[aGSMStringCurrentIndex] := AnsiChar($6D); // LATIN SMALL LETTER M
      $006E: aGSMString[aGSMStringCurrentIndex] := AnsiChar($6E); // LATIN SMALL LETTER N
      $006F: aGSMString[aGSMStringCurrentIndex] := AnsiChar($6F); // LATIN SMALL LETTER O
      $0070: aGSMString[aGSMStringCurrentIndex] := AnsiChar($70); // LATIN SMALL LETTER P
      $0071: aGSMString[aGSMStringCurrentIndex] := AnsiChar($71); // LATIN SMALL LETTER Q
      $0072: aGSMString[aGSMStringCurrentIndex] := AnsiChar($72); // LATIN SMALL LETTER R
      $0073: aGSMString[aGSMStringCurrentIndex] := AnsiChar($73); // LATIN SMALL LETTER S
      $0074: aGSMString[aGSMStringCurrentIndex] := AnsiChar($74); // LATIN SMALL LETTER T
      $0075: aGSMString[aGSMStringCurrentIndex] := AnsiChar($75); // LATIN SMALL LETTER U
      $0076: aGSMString[aGSMStringCurrentIndex] := AnsiChar($76); // LATIN SMALL LETTER V
      $0077: aGSMString[aGSMStringCurrentIndex] := AnsiChar($77); // LATIN SMALL LETTER W
      $0078: aGSMString[aGSMStringCurrentIndex] := AnsiChar($78); // LATIN SMALL LETTER X
      $0079: aGSMString[aGSMStringCurrentIndex] := AnsiChar($79); // LATIN SMALL LETTER Y
      $007A: aGSMString[aGSMStringCurrentIndex] := AnsiChar($7A); // LATIN SMALL LETTER Z
      $00E4: aGSMString[aGSMStringCurrentIndex] := AnsiChar($7B); // LATIN SMALL LETTER A WITH DIAERESIS
      $00F6: aGSMString[aGSMStringCurrentIndex] := AnsiChar($7C); // LATIN SMALL LETTER O WITH DIAERESIS
      $00F1: aGSMString[aGSMStringCurrentIndex] := AnsiChar($7D); // LATIN SMALL LETTER N WITH TILDE
      $00FC: aGSMString[aGSMStringCurrentIndex] := AnsiChar($7E); // LATIN SMALL LETTER U WITH DIAERESIS
      $00E0: aGSMString[aGSMStringCurrentIndex] := AnsiChar($7F); // LATIN SMALL LETTER A WITH GRAVE
      else Result := False;
    end;
    If Result then inc(aGSMStringCurrentIndex);
  end;

Var LWideString: WideString;
    LResultCurrentIndex: integer;
    i: Integer;
Begin
  SetLength(result,length(aMessage) * 2);
  LResultCurrentIndex := 1;

  For i := 1 to length(aMessage) do begin
    If not InternalLookupChar(aMessage[i],
                              Result,
                              LResultCurrentIndex) then begin
      LWideString := ALWideRemoveDiacritic(aMessage[i]);
      If (LWideString = '') or
         (not InternalLookupChar(LWideString[1],
                                 Result,
                                 LResultCurrentIndex))
      then Begin
        Result[LResultCurrentIndex] := AnsiChar($20); // SPACE
        inc(LResultCurrentIndex);
      end
    end;
  end;

  SetLength(result,LResultCurrentIndex - 1);
end;

{****************************************************************************}
function AlGSMComm_GSM7BitDefaultAlphabetToUnicode(const aMessage: AnsiString;
                                                   Const UseGreekAlphabet: Boolean= False): Widestring;

  {-------------------------------------------------------}
  Function InternalLookupChar(const aGSMString: AnsiString;
                              Var aGSMStringCurrentIndex: integer;
                              Var aUnicodeString: WideString;
                              Var aUnicodeStringCurrentIndex: integer): Boolean;
  Begin
    Result := True;
    Case ord(aGSMString[aGSMStringCurrentIndex]) of
      $00: Begin
             If aGSMStringCurrentIndex=length(aGSMString) then aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($0000) // NULL (see note above)
             else aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($0040); // COMMERCIAL AT
           end;
      $01: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($00A3); // POUND SIGN
      $02: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($0024); // DOLLAR SIGN
      $03: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($00A5); // YEN SIGN
      $04: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($00E8); // LATIN SMALL LETTER E WITH GRAVE
      $05: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($00E9); // LATIN SMALL LETTER E WITH ACUTE
      $06: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($00F9); // LATIN SMALL LETTER U WITH GRAVE
      $07: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($00EC); // LATIN SMALL LETTER I WITH GRAVE
      $08: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($00F2); // LATIN SMALL LETTER O WITH GRAVE
      $0A: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($000A); // LINE FEED
      $0B: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($00D8); // LATIN CAPITAL LETTER O WITH STROKE
      $0C: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($00F8); // LATIN SMALL LETTER O WITH STROKE
      $0D: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($000D); // CARRIAGE RETURN
      $0E: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($00C5); // LATIN CAPITAL LETTER A WITH RING ABOVE
      $0F: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($00E5); // LATIN SMALL LETTER A WITH RING ABOVE
      $10: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($0394); // GREEK CAPITAL LETTER DELTA
      $11: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($005F); // LOW LINE
      $12: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($03A6); // GREEK CAPITAL LETTER PHI
      $13: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($0393); // GREEK CAPITAL LETTER GAMMA
      $14: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($039B); // GREEK CAPITAL LETTER LAMDA
      $15: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($03A9); // GREEK CAPITAL LETTER OMEGA
      $16: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($03A0); // GREEK CAPITAL LETTER PI
      $17: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($03A8); // GREEK CAPITAL LETTER PSI
      $18: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($03A3); // GREEK CAPITAL LETTER SIGMA
      $19: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($0398); // GREEK CAPITAL LETTER THETA
      $1A: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($039E); // GREEK CAPITAL LETTER XI
      $1B: Begin
             If (aGSMStringCurrentIndex < length(aGSMString)) then begin
               inc(aGSMStringCurrentIndex);
               case ord(aGSMString[aGSMStringCurrentIndex]) of
                 $0A: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($000C); // FORM FEED
                 $14: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($005E); // CIRCUMFLEX ACCENT
                 $28: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($007B); // LEFT CURLY BRACKET
                 $29: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($007D); // RIGHT CURLY BRACKET
                 $2F: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($005C); // REVERSE SOLIDUS
                 $3C: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($005B); // LEFT SQUARE BRACKET
                 $3D: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($007E); // TILDE
                 $3E: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($005D); // RIGHT SQUARE BRACKET
                 $40: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($007C); // VERTICAL LINE
                 $65: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($20AC); // EURO SIGN
                 else begin
                   dec(aGSMStringCurrentIndex);
                   aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($00A0); // ESCAPE TO EXTENSION TABLE (or displayed as NBSP, see note above)
                 end;
               end;
             end
             else aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($00A0); // ESCAPE TO EXTENSION TABLE (or displayed as NBSP, see note above)
           end;
      $1C: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($00C6); // LATIN CAPITAL LETTER AE
      $1D: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($00E6); // LATIN SMALL LETTER AE
      $1E: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($00DF); // LATIN SMALL LETTER SHARP S (German)
      $1F: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($00C9); // LATIN CAPITAL LETTER E WITH ACUTE
      $20: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($0020); // SPACE
      $21: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($0021); // EXCLAMATION MARK
      $22: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($0022); // QUOTATION MARK
      $23: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($0023); // NUMBER SIGN
      $24: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($00A4); // CURRENCY SIGN
      $25: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($0025); // PERCENT SIGN
      $26: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($0026); // AMPERSAND
      $27: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($0027); // APOSTROPHE
      $28: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($0028); // LEFT PARENTHESIS
      $29: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($0029); // RIGHT PARENTHESIS
      $2A: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($002A); // ASTERISK
      $2B: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($002B); // PLUS SIGN
      $2C: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($002C); // COMMA
      $2D: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($002D); // HYPHEN-MINUS
      $2E: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($002E); // FULL STOP
      $2F: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($002F); // SOLIDUS
      $30: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($0030); // DIGIT ZERO
      $31: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($0031); // DIGIT ONE
      $32: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($0032); // DIGIT TWO
      $33: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($0033); // DIGIT THREE
      $34: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($0034); // DIGIT FOUR
      $35: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($0035); // DIGIT FIVE
      $36: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($0036); // DIGIT SIX
      $37: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($0037); // DIGIT SEVEN
      $38: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($0038); // DIGIT EIGHT
      $39: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($0039); // DIGIT NINE
      $3A: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($003A); // COLON
      $3B: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($003B); // SEMICOLON
      $3C: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($003C); // LESS-THAN SIGN
      $3D: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($003D); // EQUALS SIGN
      $3E: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($003E); // GREATER-THAN SIGN
      $3F: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($003F); // QUESTION MARK
      $40: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($00A1); // INVERTED EXCLAMATION MARK
      $43: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($0043); // LATIN CAPITAL LETTER C
      $44: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($0044); // LATIN CAPITAL LETTER D
      $46: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($0046); // LATIN CAPITAL LETTER F
      $47: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($0047); // LATIN CAPITAL LETTER G
      $4A: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($004A); // LATIN CAPITAL LETTER J
      $4C: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($004C); // LATIN CAPITAL LETTER L
      $51: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($0051); // LATIN CAPITAL LETTER Q
      $52: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($0052); // LATIN CAPITAL LETTER R
      $53: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($0053); // LATIN CAPITAL LETTER S
      $56: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($0056); // LATIN CAPITAL LETTER V
      $57: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($0057); // LATIN CAPITAL LETTER W
      $59: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($0059); // LATIN CAPITAL LETTER Y
      $5B: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($00C4); // LATIN CAPITAL LETTER A WITH DIAERESIS
      $5C: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($00D6); // LATIN CAPITAL LETTER O WITH DIAERESIS
      $5D: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($00D1); // LATIN CAPITAL LETTER N WITH TILDE
      $5E: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($00DC); // LATIN CAPITAL LETTER U WITH DIAERESIS
      $5F: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($00A7); // SECTION SIGN
      $60: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($00BF); // INVERTED QUESTION MARK
      $61: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($0061); // LATIN SMALL LETTER A
      $62: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($0062); // LATIN SMALL LETTER B
      $63: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($0063); // LATIN SMALL LETTER C
      $64: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($0064); // LATIN SMALL LETTER D
      $65: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($0065); // LATIN SMALL LETTER E
      $66: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($0066); // LATIN SMALL LETTER F
      $67: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($0067); // LATIN SMALL LETTER G
      $68: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($0068); // LATIN SMALL LETTER H
      $69: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($0069); // LATIN SMALL LETTER I
      $6A: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($006A); // LATIN SMALL LETTER J
      $6B: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($006B); // LATIN SMALL LETTER K
      $6C: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($006C); // LATIN SMALL LETTER L
      $6D: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($006D); // LATIN SMALL LETTER M
      $6E: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($006E); // LATIN SMALL LETTER N
      $6F: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($006F); // LATIN SMALL LETTER O
      $70: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($0070); // LATIN SMALL LETTER P
      $71: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($0071); // LATIN SMALL LETTER Q
      $72: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($0072); // LATIN SMALL LETTER R
      $73: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($0073); // LATIN SMALL LETTER S
      $74: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($0074); // LATIN SMALL LETTER T
      $75: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($0075); // LATIN SMALL LETTER U
      $76: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($0076); // LATIN SMALL LETTER V
      $77: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($0077); // LATIN SMALL LETTER W
      $78: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($0078); // LATIN SMALL LETTER X
      $79: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($0079); // LATIN SMALL LETTER Y
      $7A: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($007A); // LATIN SMALL LETTER Z
      $7B: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($00E4); // LATIN SMALL LETTER A WITH DIAERESIS
      $7C: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($00F6); // LATIN SMALL LETTER O WITH DIAERESIS
      $7D: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($00F1); // LATIN SMALL LETTER N WITH TILDE
      $7E: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($00FC); // LATIN SMALL LETTER U WITH DIAERESIS
      $7F: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($00E0); // LATIN SMALL LETTER A WITH GRAVE
      $09: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($00E7); // LATIN SMALL LETTER C WITH CEDILLA
      //$09: aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($00C7); // LATIN CAPITAL LETTER C WITH CEDILLA (see note above)
      $41: begin
             if not UseGreekAlphabet then aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($0041) // LATIN CAPITAL LETTER A
             else aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($0391); // GREEK CAPITAL LETTER ALPHA
           end;
      $42: begin
             if not UseGreekAlphabet then aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($0042) // LATIN CAPITAL LETTER B
             else aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($0392); // GREEK CAPITAL LETTER BETA
           end;
      $45: begin
             if not UseGreekAlphabet then aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($0045) // LATIN CAPITAL LETTER E
             else aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($0395); // GREEK CAPITAL LETTER EPSILON
           end;
      $48: begin
             if not UseGreekAlphabet then aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($0048) // LATIN CAPITAL LETTER H
             else aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($0397); // GREEK CAPITAL LETTER ETA
           end;
      $49: begin
             if not UseGreekAlphabet then aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($0049) // LATIN CAPITAL LETTER I
             else aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($0399); // GREEK CAPITAL LETTER IOTA
           end;
      $4B: begin
             if not UseGreekAlphabet then aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($004B) // LATIN CAPITAL LETTER K
             else aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($039A); // GREEK CAPITAL LETTER KAPPA
           end;
      $4D: begin
             if not UseGreekAlphabet then aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($004D) // LATIN CAPITAL LETTER M
             else aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($039C); // GREEK CAPITAL LETTER MU
           end;
      $4E: begin
             if not UseGreekAlphabet then aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($004E) // LATIN CAPITAL LETTER N
             else aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($039D); // GREEK CAPITAL LETTER NU
           end;
      $4F: begin
             if not UseGreekAlphabet then aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($004F) // LATIN CAPITAL LETTER O
             else aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($039F); // GREEK CAPITAL LETTER OMICRON
           end;
      $50: begin
             if not UseGreekAlphabet then aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($0050) // LATIN CAPITAL LETTER P
             else aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($03A1); // GREEK CAPITAL LETTER RHO
           end;
      $54: begin
             if not UseGreekAlphabet then aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($0054) // LATIN CAPITAL LETTER T
             else aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($03A4); // GREEK CAPITAL LETTER TAU
           end;
      $55: begin
             if not UseGreekAlphabet then aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($0055) // LATIN CAPITAL LETTER U
             else aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($03A5); // GREEK CAPITAL LETTER UPSILON
           end;
      $58: begin
             if not UseGreekAlphabet then aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($0058) // LATIN CAPITAL LETTER X
             else aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($03A7); // GREEK CAPITAL LETTER CHI
           end;
      $5A: begin
             if not UseGreekAlphabet then aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($005A) // LATIN CAPITAL LETTER Z
             else aUnicodeString[aUnicodeStringCurrentIndex] := WideChar($0396); // GREEK CAPITAL LETTER ZETA
           end;
      else Result := False;
    end;
    If Result then inc(aUnicodeStringCurrentIndex);
  end;

Var LResultCurrentIndex: integer;
    LMessageCurrentIndex: Integer;

Begin
  SetLength(result,length(aMessage));
  LResultCurrentIndex := 1;
  LMessageCurrentIndex := 1;

  While LMessageCurrentIndex <= length(aMessage) do begin
    If not InternalLookupChar(aMessage,
                              LMessageCurrentIndex,
                              Result,
                              LResultCurrentIndex) then begin
      Result[LResultCurrentIndex] := WideChar($0020); // SPACE
      inc(LResultCurrentIndex);
    end;
    inc(LMessageCurrentIndex);
  end;

  SetLength(result,LResultCurrentIndex - 1);
end;

{****************************************}
{aMessage need to be in GSM 7 bit charset}
function AlGSMComm_BuildPDUMessage(aSMSCenter, aSMSAddress: ansiString; const aMessage: AnsiString): AnsiString;

  {------------------------------------------------------------}
  function InternalStringToPDU(const v: AnsiString): AnsiString;
  var I, InLen, OutLen, OutPos : Integer;
      RoundUp : Boolean;
      TempByte, NextByte : Byte;
  begin
    { Check for empty input }
    if v = '' then Exit;

    { Init OutPos }
    OutPos := 1;

    { Set length of output string }
    InLen := Length(v);
    If InLen > 160 then raise EALException.Create('Input string greater than 160 characters!');
    RoundUp := (InLen * 7 mod 8) <> 0;
    OutLen := InLen * 7 div 8;
    if RoundUp then Inc(OutLen);
    SetLength(Result, OutLen);

    { Encode output string }
    for I := 1 to InLen do begin
      TempByte := Byte(v[I]);
      if ((TempByte and $80) <> 0) then raise EALException.Create('Input string contains 8-bit data!');
      if (I < InLen) then NextByte := Byte(v[I+1])
      else NextByte := 0;
      TempByte := TempByte shr ((I-1) mod 8);
      NextByte := NextByte shl (8 - ((I) mod 8));
      TempByte := TempByte or NextByte;
      Result[OutPos] := AnsiChar(TempByte);
      if I mod 8 <> 0 then Inc(OutPos);
    end;
  end;

var LLength, I: Integer;
    S: AnsiString;
begin
  {clean the aSMSCenter and aSMSAddress from unwanted char}
  If (aSMSCenter <> '') and (aSMSCenter[1] = '+') then delete(aSMSCenter,1,1);
  If (aSMSAddress <> '') and (aSMSAddress[1] = '+') then delete(aSMSAddress,1,1);

  {write the SMSC information}
  if aSMSCenter = '' then result := '00' {write Length of SMSC information. Here the length is 0, which means that the SMSC stored in the phone should be used.}
  else begin
    LLength := Length(aSMSCenter);
    if Odd(LLength) then LLength := LLength + 1; {If The length of the phone number is odd (11), so a trailing F has been added to form proper octets}
    LLength := 1 + (LLength Div 2); {Length of the SMSC information in octect}

    {Write the Length of the SMSC information}
    If LLength < 10 then result := '0' + ALIntToStr(LLength)
    else result := ALIntToStr(LLength);

    {write the Type-of-address of the SMSC. (91 means international format of the phone number)}
    result := result + '91';

    {Write SMSCenter to PDU message (in decimal semi-octets)}
    LLength := Length(aSMSCenter);
    I := 1;
    while I < LLength do begin
      result := result + aSMSCenter[I+1] + aSMSCenter[I];
      I := I + 2;
    end;

    {If The length of the phone number is odd (11), so a trailing F has been added to form proper octets.}
    if Odd(LLength) then result := result + 'F' + aSMSCenter[LLength];
  end;

  {Write the First octet of the SMS-SUBMIT message.}
  result := result + '11';

  {Write the TP-Message-Reference. The "00" value here lets the phone set the message reference number itself.}
  result := result + '00';

  {Write the Address-Length. Length of phone number (11) }
  result := result + ALFormat ('%02.2x', [Length(aSMSAddress)]);

  {Write the Type-of-Address. (91 indicates international format of the phone number).}
  result := result + '91';

  {Write The phone number in semi octet}
  LLength := Length(aSMSAddress);
  I := 1;
  while I < LLength do begin
    result := result + aSMSAddress[I+1] + aSMSAddress[I];
    I := I + 2;
  end;

  {If The length of the phone number is odd (11), therefore a trailing F has been added}
  if Odd(LLength) then result := result + 'F' + aSMSAddress[LLength];

  {TP-PID. Protocol identifier}
  result := result + '00';

  {TP-DCS. Data coding scheme.This message is coded according to the 7bit default alphabet.
   Having "04" instead of "00" here, would indicate that the TP-User-Data field of this message
   should be interpreted as 8bit rather than 7bit (used in e.g. smart messaging, OTA provisioning etc).}
  result := result + '00';

  {TP-Validity-Period. "AA" means 4 days. Note: This octet is optional, see bits 4 and 3 of the first octet}
  result := result + 'AA';

  {Length of SMS message converted to HEX}
  result := result + ALFormat ('%02.2x', [Length(aMessage)]);

  {Add SMSMessage after transformation to PDU string}
  S := InternalStringToPDU(aMessage);
  for I := 1 to Length(S) do result := result + ALIntToHex(Byte(S[I]), 2);
end;

{****************************************}
{aMessage need to be in GSM 7 bit charset}
Procedure AlGSMComm_DecodePDUMessage(const aPDUMessage: AnsiString; Var aSMSCenter, aSMSAddress, AMessage: AnsiString);

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _7bitPDUToString(Pdu: AnsiString; UdhLen: integer): AnsiString;
  var I, InLen, OutLen, OutPos : Integer;
      TempByte, PrevByte : Byte;
      PaddingBits: Byte;
      PduIdx: Integer;
      PduLen: Integer;
      Octet: Byte;
      ShiftedOctet: Byte;
      NextOctet: Byte;
  begin
    { Check for empty input }
    if Pdu = '' then Exit;

    // The following code removes padding at the end of the PDU by shifting it
    // *right* by <PaddingBits>. It does this by taking the least significant
    // <PaddingBits> from the following PDU byte and moving them to the most
    // significant the current PDU byte.
    if UdhLen > 0 then begin
      PduLen := Length(Pdu);
      PaddingBits := 7 - ((UdhLen * 8) mod 7);
      for PduIdx := 1 to PduLen do
      begin
        Octet := Byte(Pdu[PduIdx]);
        if (PduIdx = PduLen) then
          ShiftedOctet := Byte(Octet shr PaddingBits)
        else
        begin
          NextOctet := Byte(Pdu[PduIdx + 1]);
          ShiftedOctet := Byte(Octet shr PaddingBits) or
            Byte(NextOctet shl (8 - PaddingBits));
        end;
        Byte(Pdu[PduIdx]) := ShiftedOctet;
      end;
    end;

    { Init variables }
    PrevByte := 0;
    OutPos := 1;

    { Set length of output string }
    InLen := Length(Pdu);
    Assert(InLen <= 140, 'Input string greater than 140 characters');
    OutLen := (InLen * 8) div 7;
    SetLength(Result, OutLen);

    { Encode output string }
    for I := 1 to InLen do begin
      TempByte := Byte(Pdu[I]);
      TempByte := TempByte and not ($FF shl (7-((I-1) mod 7)));
      TempByte := TempByte shl ((I-1) mod 7);
      TempByte := TempByte or PrevByte;
      Result[OutPos] := AnsiChar(TempByte);
      Inc(OutPos);

      { Set PrevByte for next round (or directly put it to Result) }
      PrevByte := Byte(Pdu[I]);
      PrevByte := PrevByte shr (7-((I-1) mod 7));
      if (I mod 7) = 0 then begin
        Result[OutPos] := AnsiChar(PrevByte);
        Inc(OutPos);
        PrevByte := 0;
      end;
    end;
    if Result[Length(Result)] = #0 then Result := ALCopyStr(Result, 1, pred(Length(Result)));
  end;

var LLength, I: Integer;
    LFirstOctet: ansiString;
    LDSCOctet: ansiString;
    LUdhLength: Integer;

begin

  {init}
  aSMSCenter := '';
  aSMSAddress := '';
  AMessage := '';
  I := 1; //07913386094000F0040B913386184131F900006040722172728007F43AA87D0AC301

  {Length of the SMSC information (in this case 7 octets)}
  LLength := ALStrToInt('$' + AlCopyStr(aPDUMessage, i, 2)); //07
  inc(I,2); //913386094000F0040B913386184131F900006040722172728007F43AA87D0AC301
  if LLength > 0 then begin

    //Type-of-address of the SMSC. (91 means international format of the phone number)
    inc(I,2); //3386094000F0040B913386184131F900006040722172728007F43AA87D0AC301

    //Service center number(in decimal semi-octets). The length of the phone number is odd (11),
    //so a trailing F has been added to form proper octets. The phone number of this service center is "+33689004000".
    while I < (LLength*2 + 3) do begin
      if aPDUMessage[I] = 'F' then aSMSCenter := aSMSCenter + aPDUMessage[I+1]
      else aSMSCenter := aSMSCenter + aPDUMessage[I+1] + aPDUMessage[I]; //33689004000
      inc(I,2); //040B913386184131F900006040722172728007F43AA87D0AC301
    end;

  end;

  //First octet of this SMS-DELIVER message.
  //Bit no	  7	       6	      5	       4	        3	       2	     1	    0
  //Name	  TP-RP	  TP-UDHI  	TP-SRI	(unused)	(unused)	TP-MMS	TP-MTI	TP-MTI
  LFirstOctet := ALIntToBit(ALStrToInt('$'+AlCopyStr(aPDUMessage,I,2)), 8);
  inc(I,2); //0B913386184131F900006040722172728007F43AA87D0AC301

  //Address-Length. Length of the sender number (0B hex = 11 dec)
  LLength := (ALStrToInt('$' + AlCopyStr(aPDUMessage,I,2))); //11
  inc(I,2); //913386184131F900006040722172728007F43AA87D0AC301

  //Type-of-address of the sender number
  inc(I,2); //3386184131F900006040722172728007F43AA87D0AC301

  //Change aLength to Octets
  if Odd(LLength) then inc(LLength,1); //12
  LLength := LLength + I;
  while I < (LLength) do begin
    if aPDUMessage[I] = 'F' then aSMSAddress := aSMSAddress + aPDUMessage[I+1]
    else aSMSAddress := aSMSAddress + aPDUMessage[I+1] + aPDUMessage[I]; //33688114139
    inc(I,2); //00006040722172728007F43AA87D0AC301
  end;

  //TP-PID. Protocol identifier.
  inc(I,2); //006040722172728007F43AA87D0AC301

  //TP-DCS Data coding scheme
  LDSCOctet := ALIntToBit(ALStrToInt('$'+AlCopyStr(aPDUMessage,I,2)), 8);
  inc(I,2); //6040722172728007F43AA87D0AC301

  //TP-SCTS. Time stamp (semi-octets)
  inc(I,14); //07F43AA87D0AC301

  //TP-UDL. User data length, length of message. The TP-DCS field indicated 7-bit data,
  //so the length here is the number of septets (10). If the TP-DCS field were set to
  //indicate 8-bit data or Unicode, the length would be the number of octets (9).
  inc(I,2); //F43AA87D0AC301

  //TP-UDHI
  if LFirstOctet[2] = '1' then begin
    LUdhLength := (ALStrToInt('$' + AlCopyStr(aPDUMessage,I,2))) + 1; //6
    inc(i,LUdhLength * 2);
  end
  else LUdhLength := 0;

  // NumLength is the length of the message
  LLength := Length(aPDUMessage); // 14

  // 7-bit alphabet
  aMessage := '';
  if (LDSCOctet[5] = '0') and (LDSCOctet[6] = '0') then begin
    while I <= LLength - 1 do begin
      aMessage := aMessage + AnsiChar(ALStrToInt(ansiChar('$') + ansiChar(aPDUMessage[I]) + ansiChar(aPDUMessage[I+1])));
      inc(I,2);
    end;
    aMessage := ansiString(AlGSMComm_GSM7BitDefaultAlphabetToUnicode(_7bitPDUToString(aMessage, LUdhLength)))
  end
  // 8-bit alphabet
  else if (LDSCOctet[5] = '0') and (LDSCOctet[6] = '1') then begin
    while I <= LLength - 1 do begin
      aMessage := aMessage + AnsiChar(ALStrToInt(ansiChar('$') + ansiChar(aPDUMessage[I]) + ansiChar(aPDUMessage[I+1])));
      inc(I,2);
    end;
  end
  // 16-bit alphabet
  else if (LDSCOctet[5] = '1') and (LDSCOctet[6] = '0') then begin
    while I <= LLength - 3 do begin
      aMessage := aMessage + ansiString( WideChar(ALStrToInt(ansiChar('$') +
                                                             ansiChar(aPDUMessage[I]) +
                                                             ansiChar(aPDUMessage[I+1]) +
                                                             ansiChar(aPDUMessage[I+2]) +
                                                             ansiChar(aPDUMessage[I+3]))));
      inc(I,4);
    end;
  end;

end;

{**********************************************}
procedure TAlGSMComm.CheckError(Error: Boolean);
begin
  if Error then RaiseLastOSError;
end;

{*****************************************************}
procedure TAlGSMComm.Connect(const Serial: AnsiString);
begin
  if FConnected then raise EALException.Create('Already connected');

  Try

    {Set Fconnected to True}
    Fconnected := True;

    {Create the file}
    fSerial := CreateFileA(PAnsiChar(Serial),
                           GENERIC_READ or GENERIC_WRITE,
                           0, (* comm devices must be opened w/exclusive-access *)
                           NIL, (* no security attrs *)
                           OPEN_EXISTING, (* comm devices must use OPEN_EXISTING *)
                           0, (* Ansynchronous or Synchronous I/O *)
                           0); (* hTemplate must be NULL for comm devices *)
    CheckError(fSerial=INVALID_HANDLE_VALUE);

    {init the timeout for serial}
    InitCommTimeouts;

    {init the comm State for serial}
    InitCommState;

  except
    On E: Exception do begin
      Disconnect;
      E.Message := 'Impossible to establish connection to the device. ' + E.Message;
      Raise;
    end;
  end;
end;

{******************************}
procedure TAlGSMComm.Disconnect;
begin
  If Fconnected then begin
    CloseHandle(fSerial);
    fSerial := INVALID_HANDLE_VALUE;
    Fconnected:= False;
  end;
end;

{****************************}
constructor TAlGSMComm.Create;
begin
  FSerial := INVALID_HANDLE_VALUE;
  FBaudRate:= CBR_2400;
  Fconnected:= False;
  Ftimeout:= 60000;
end;

{****************************}
destructor TAlGSMComm.Destroy;
begin
  If Fconnected then Disconnect;
  inherited;
end;

{******************************************************************}
function TAlGSMComm.SerialRead(var Buffer; Count: Integer): Longint;
var NumberOfBytesRead : DWORD;
begin
  CheckError(not ReadFile(fSerial,
                          Buffer,
                          Count,
                          NumberOfBytesRead,
                          NIL));
  Result := NumberOfBytesRead;
end;

{*******************************************************************}
function TAlGSMComm.SerialWrite(var Buffer; Count: Integer): Longint;
Var NumberOfBytesWritten: Dword;
begin
  checkError(Not WriteFile(fSerial,
                           buffer,
                           count,
                           NumberOfBytesWritten,
                           NIL));
  Result := NumberOfBytesWritten;
end;

{*********************************}
procedure TAlGSMComm.InitCommState;
var MyDCB : TDCB;
begin
  CheckError(not GetCommState(fSerial, MyDCB));
  MyDCB.BaudRate := FBaudRate;
  MyDCB.Parity := NOPARITY;
  MyDCB.StopBits := ONESTOPBIT;
  MyDCB.ByteSize := 8;
  CheckError(not SetCommState(fSerial, MyDCB));
end;


{************************************}
procedure TAlGSMComm.InitCommTimeouts;
var CommTimeouts : TCommTimeouts;
begin
  {A value of MAXDWORD, combined with zero values for both the ReadTotalTimeoutConstant and ReadTotalTimeoutMultiplier members,
   specifies that the read operation is to return immediately with the characters that have already been received, even if no
   characters have been received.}
  CommTimeouts.ReadIntervalTimeout := MAXDWORD;
  CommTimeouts.ReadTotalTimeoutMultiplier := 0;
  CommTimeouts.ReadTotalTimeoutConstant := 0;
  CommTimeouts.WriteTotalTimeoutMultiplier := 0;
  CommTimeouts.WriteTotalTimeoutConstant := Ftimeout;
  CheckError(not SetCommTimeouts(fSerial, CommTimeouts));
end;

{***************************************************}
procedure TAlGSMComm.SetBaudRate(const Value: Dword);
begin
  IF FbaudRate <> Value then Begin
    Disconnect;
    FBaudRate := Value;
  end;
end;

{*****************************************************}
procedure TAlGSMComm.Settimeout(const Value: Cardinal);
begin
  IF Ftimeout <> Value then Begin
    Disconnect;
    Ftimeout := Value;
  end;
end;

{*****************************************************}
procedure TAlGSMComm.SendSMSinPDUMode(const aSMSCenter, //Service Center Address. leave empty to use the default configuration
                                            aSMSAddress: AnsiString; //phone number of the recipient
                                      aMessage: AnsiString; //The body of the message, can be in PDU (EncodeMessageInPDU need to be false) or in GSM 7 bit charset (EncodeMessageInPDU need to be true)
                                      const EncodeMessageInPDU: Boolean=True); //if we need to encode the aMessage in PDU
Var LLength: Integer;
    Str: AnsiString;
begin
  If Not Fconnected then raise EALException.Create('Not Connected!');

  {to reset completely previous request}
  SendCmd(#27);

  {starting}
  SendCmd('AT'#13);
  GetATCmdOkResponse('AT command error');

  {Message Format}
  SendCmd('AT+CMGF=0'#13); //pdu mode
  GetATCmdOkResponse('AT+CMGF command error!');

  {Service Center Address}
  If aSMSCenter <> '' then begin
    SendCmd('AT+CSCA="'+aSMSCenter+'"'#13);
    GetATCmdOkResponse('AT+CSCA command error!');
  end;

  {Send Message}
  If EncodeMessageInPDU then aMessage := AlGSMComm_BuildPDUMessage(aSMSCenter,
                                                                   aSMSAddress,
                                                                   aMessage);

  Str := AlCopyStr(aMessage,1,2);
  If not ALTryStrToInt(str,LLength) then LLength := 0;
  LLength := (Length(aMessage) div 2) - LLength - 1;

  SendCmd('AT+CMGS='+ALIntToStr(LLength)+#13);
  GetATCmdlinefeedResponse('AT+CMGS command error!');
  SendCmd(aMessage + #26);
  GetATCmdOkResponse('AT+CMGS command error!');
end;

{******************************************************}
procedure TAlGSMComm.SendSMSinTextMode(const aSMSCenter,  //Service Center Address. leave empty to use the default configuration
                                             aSMSAddress, //phone number of the recipient
                                             aMessage,    //The body of the message
                                             aCharset: AnsiString); //The Charset use in the message. leave empty to use the default charset
begin
  If Not Fconnected then raise EALException.Create('Not Connected!');

  {to reset completely previous request}
  SendCmd(#27);

  {starting}
  SendCmd('AT'#13);
  GetATCmdOkResponse('AT command error');

  {Message Format}
  SendCmd('AT+CMGF=1'#13); //Text mode
  GetATCmdOkResponse('AT+CMGF command error!');

  {Service Center Address}
  If aSMSCenter <> '' then begin
    SendCmd('AT+CSCA="'+aSMSCenter+'"'#13);
    GetATCmdOkResponse('AT+CSCA command error!');
  end;

  {Charset}
  If aCharset <> '' then begin
    SendCmd('AT+CSCS="'+aCharset+'"'#13);
    GetATCmdOkResponse('AT+CSCS Command error!');
  end;

  {Send Message}
  SendCmd('AT+CMGS="'+aSMSAddress+'"'+#13);
  GetATcmdlinefeedResponse('AT+CMGS command error!');
  SendCmd(aMessage + #26);
  GetATCmdOkResponse('AT+CMGS command error!');
end;

{***********************************************}
{give result with on each lines Index=PDUMessage}
procedure TAlGSMComm.ListAllSMSinPDUMode(aLstMessage: TALStrings; MemStorage: AnsiString);

  {-----------------------------------}
  Procedure InternalFulfillLstMessage;
  Var P1, P2: Integer;
      LIndex: Integer;
      LStr: AnsiString;
      LStat: integer;
  Begin

    //AT+CMGL
    //0 "REC UNREAD"   received unread message (i.e. new message)
    //1 "REC READ"     received read message
    //2 "STO UNSENT"   stored unsent message (only applicable to SMs)
    //3 "STO SENT"     stored sent message (only applicable to SMs)
    //4 "ALL"          all messages (only applicable to +CMGL command)
    //
    // normally i must say AT+CMGL=4 but i don't know why on some phone (B2100i for exemple)
    // their is no AT+CMGL=4 (instead AT+CMGL=5)

    LStat := 0;
    while LStat <= 1 do begin

      {Receive Message}
      SendCmd('AT+CMGL='+alinttostr(LStat)+#13);
      GetATCmdOkResponse(LStr, 'AT+CMGL command error!');

      {fullfill aLstMessage}
      P1 := AlPos('+CMGL: ',LStr);
      While P1 > 0 do begin
        Inc(P1,7);
        P2 := AlPosEx(',',LStr,P1);
        If P2 <= 0 then raise EALException.Create('AT+CMGL parse error!');
        LIndex := ALStrToInt(AlCopyStr(LStr,P1,P2-P1));
        P1 := ALPosEx(#13#10,LStr,P2);
        If P1 <= 0 then raise EALException.Create('AT+CMGL parse error!');
        P1 := P1 + 2;
        P2 := ALPosEx(#13#10,LStr,P1);
        If P2 <= 0 then raise EALException.Create('AT+CMGL parse error!');
        if aLstMessage.IndexOfName(ALIntToStr(LIndex)) < 0 then aLstMessage.Add(ALIntToStr(LIndex)+aLstMessage.NameValueSeparator+ALTrim(AlCopyStr(LStr,P1,P2-P1)));
        P1 := AlPosEx('+CMGL: ',LStr,P2);
      end;
      inc(LStat);

    end;
  end;

begin
  If Not Fconnected then raise EALException.Create('Not Connected!');

  {to reset completely previous request}
  SendCmd(#27);

  {starting}
  SendCmd('AT'#13);
  GetATCmdOkResponse('AT command error');

  {Message Format}
  SendCmd('AT+CMGF=0'#13); //pdu mode
  GetATCmdOkResponse('AT+CMGF command error!');

  {storage}
  If MemStorage = '' then MemStorage := '"SM"' //sim
  else if MemStorage[1] <> '"' then MemStorage := '"'+MemStorage+'"';
  SendCmd('AT+CPMS='+MemStorage+#13);
  GetATCmdOkResponse('AT+CPMS command error!');

  {list the SMS}
  InternalFulfillLstMessage;
end;

{**********************************************************************}
procedure TAlGSMComm.DeleteSMS(aIndex: integer; MemStorage: AnsiString);
begin
  If Not Fconnected then raise EALException.Create('Not Connected!');

  {to reset completely previous request}
  SendCmd(#27);

  {starting}
  SendCmd('AT'#13);
  GetATCmdOkResponse('AT command error');

  {storage}
  If MemStorage = '' then MemStorage := '"SM"' //sim
  else if MemStorage[1] <> '"' then MemStorage := '"'+MemStorage+'"';
  SendCmd('AT+CPMS='+MemStorage+#13);
  GetATCmdOkResponse('AT+CPMS command error!');

  {Message Format}
  SendCmd('AT+CMGD='+ALIntToStr(aIndex)+#13);
  GetATCmdOkResponse('AT+CMGD command error!');
end;

{***************************************************}
Procedure TAlGSMComm.SendCmd(const aCmd: AnsiString);
Var P: PAnsiChar;
    L: Integer;
    LByteSent: integer;
    LStart: Cardinal;
Begin
  LStart := GetTickCount;
  If aCmd <> '' then begin
    p:=@aCmd[1]; // pchar
    l:=length(aCmd);
    while l>0 do begin
      if (LStart + Ftimeout) < GetTickCount then raise EALException.Create('Timeout!');
      LByteSent:=SerialWrite(p^,l);
      inc(p,LByteSent);
      dec(l,LByteSent);
    end;
  end;
end;

{******************************************}
function TAlGSMComm.GetResponse: AnsiString;
Var LBuffStr: AnsiString;
    LBuffStrLength: Integer;
    LStart: Cardinal;
Begin
  LStart := GetTickCount;
  Result := '';
  repeat
    if (LStart + Ftimeout) < GetTickCount then raise EALException.Create('Timeout!');
    Setlength(LBuffStr,512); //The maximum total length
    LBuffStrLength := SerialRead(LBuffStr[1], length(LBuffStr));
    Result := Result + AlCopyStr(LBuffStr,1,LBuffStrLength);
  until LBuffStrLength <> 512;
end;

{************************************************************************}
procedure TAlGSMComm.GetATCmdlinefeedResponse(const ErrorMsg: AnsiString);
Var LResponse: AnsiString;
    LStart: Cardinal;
Begin
  LStart := GetTickCount;
  LResponse := alUppercase(GetResponse);
  While (Alpos('>'#32, LResponse) <= 0) do Begin
    if (LStart + Ftimeout) < GetTickCount then Raise EALException.Create('Timeout!')
    else if (Alpos(#13#10'ERROR'#13#10, LResponse) > 0) then raise EALException.Create(ErrorMsg + ' (' + ALTrim(AlStringReplace(LResponse, #13#10, ' ', [rfReplaceALL])) + ')' );
    LResponse := LResponse + alUppercase(GetResponse);
  end;
end;

{******************************************************************}
procedure TAlGSMComm.GetATCmdOkResponse(const ErrorMsg: AnsiString);
Var LResponse: AnsiString;
Begin
  GetATCmdOkResponse(LResponse, ErrorMsg);
end;

{********************************************************************************************}
Procedure TAlGSMComm.GetATCmdOkResponse(var Response: AnsiString; Const ErrorMsg: AnsiString);
Var LStart: Cardinal;
    LTmpErrorMsg: AnsiString;
    ln: integer;
    P1: Integer;
Begin
  LStart := GetTickCount;
  Response := alUppercase(GetResponse);
  While (Alpos(#13#10'OK'#13#10, Response) <= 0) do Begin
    if (LStart + Ftimeout) < GetTickCount then Raise EALException.Create('Timeout!')
    else if (Alpos(#13#10'ERROR'#13#10, Response) > 0) or ALMatchesMask(Response, '*'#13#10'+CMS ERROR: *'#13#10'*') then begin
      P1 := AlPos(#13#10'+CMS ERROR:', Response);
      if P1 > 0 then LTmpErrorMsg := ErrorMsg + ' (' +ALTrim(AlCopyStr(Response, P1, Maxint)) + ')' // +CMS ERROR: 38
      else LTmpErrorMsg := ErrorMsg;
      raise EALException.Create(LTmpErrorMsg);
    end;
    ln := length(Response);
    Response := Response + alUppercase(GetResponse);
    if ln = length(Response) then sleep(1); // to not have 100% CPU usage
  end;
end;

end.

