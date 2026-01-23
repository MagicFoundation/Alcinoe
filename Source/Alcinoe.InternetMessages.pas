unit Alcinoe.InternetMessages;

interface

{$I Alcinoe.inc}

uses
  Alcinoe.StringList;

Type

  {------------------------------}
  TALMailHeadersA = Class(TObject)
  private
    FKnownHeaders: array[0..39] of AnsiString;
    FUnknownHeaders: TALStringsA;
    FAutoFillDefaults: Boolean;
  protected
    function PropertyIndexToName(const AIndex: Integer): AnsiString; virtual;
    function NameToPropertyIndex(const AName: AnsiString): Integer; virtual;
    function GetUnknownHeaders: TALStringsA; virtual;
    function GetHeaderValueByPropertyIndex(const Index: Integer): AnsiString; virtual;
    procedure SetHeaderValueByPropertyIndex(const Index: Integer; const Value: AnsiString); virtual;
    function GetHeaderValueByName(const AName: AnsiString): AnsiString; virtual;
    procedure SetHeaderValueByName(const AName: AnsiString; const AValue: AnsiString); virtual;
    Function GetRawHeaderText: AnsiString; virtual;
    procedure SetRawHeaderText(const ARawHeaderText: AnsiString); virtual;
    function GetContentCharset: AnsiString; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear; virtual;
    property RawHeaderText: AnsiString read GetRawHeaderText write SetRawHeaderText;
    property From: AnsiString index 0 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {From: John Doe <jdoe@machine.example>}
    property Sender: AnsiString index 1 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Sender: Michael Jones <mjones@machine.example>}
    property &To: AnsiString index 2 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {To: Mary Smith <mary@example.net>}
    property Cc: AnsiString index 3 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {cc: <boss@nil.test>, "Giant; \"Big\" Box" <sysservices@example.net>}
    property Bcc: AnsiString index 4 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {bcc: <boss@nil.test>, "Giant; \"Big\" Box" <sysservices@example.net>}
    property ReplyTo: AnsiString index 5 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Reply-To: "Mary Smith: Personal Account" <smith@home.example>}
    property Subject: AnsiString index 6 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Subject: Saying Hello}
    property MessageID: AnsiString index 7 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Message-ID: <1234@local.machine.example>}
    property InReplyTo: AnsiString index 8 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {In-Reply-To: <1234@local.machine.example>}
    property References: AnsiString index 9 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {References: <1234@local.machine.example> <3456@example.net>}
    property Comments: AnsiString index 10 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Comments: Authenticated sender is gboyd@netcom.com}
    property Date: AnsiString index 11 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Date: Fri, 21 Nov 1997 09:55:06 -0600}
    property ContentType: AnsiString index 12 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Content-Type: text/plain; charset="iso-8859-1"}
    property ContentTransferEncoding: AnsiString index 13 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Content-Transfer-Encoding: 8bit}
    property MIMEVersion: AnsiString index 14 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {MIME-Version: 1.0}
    property Importance: AnsiString index 15 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Importance: normal}
    property DispositionNotificationTo: AnsiString index 16 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Disposition-Notification-To: boss@nil.test}
    property UnknownHeaders: TALStringsA read GetUnknownHeaders;
    property Values[const AName: AnsiString]: AnsiString read GetHeaderValueByName write SetHeaderValueByName; default;
    property AutoFillDefaults: Boolean read FAutoFillDefaults write FAutoFillDefaults;
  end;

function AlParseEmailAddress(FriendlyEmail: AnsiString; var RealName : AnsiString; Const decodeRealName: Boolean=True): AnsiString;
function AlExtractEmailAddress(const FriendlyEmail: AnsiString): AnsiString;
Function ALMakeFriendlyEmailAddress(const aRealName, aEmail: AnsiString): AnsiString;
Function ALEncodeRealName4FriendlyEmailAddress(const aRealName: AnsiString): AnsiString;
Function AlGenerateInternetMessageID: AnsiString; overload;
Function AlGenerateInternetMessageID(const ahostname: AnsiString): AnsiString; overload;
function AlIsValidEmail(const Value: AnsiString): boolean; overload;
function AlIsValidEmail(const Value: String): boolean; overload;
procedure ALParseEmlMessage(const AFileName: String; const AHeaders: TALMailHeadersA; out ABody: AnsiString);

implementation

uses
  system.Sysutils,
  System.AnsiStrings,
  Alcinoe.Http,
  Alcinoe.Common,
  Alcinoe.net,
  Alcinoe.StringUtils;

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
begin

  //----------
  FriendlyEmail := ALTrim(FriendlyEmail);

  //----------
  Result := '';
  RealName := '';

  //----------
  var ln: integer := Length(FriendlyEmail);

  //----------
  var P1: Integer;
  if ALMatchesMaskA(FriendlyEmail, '* <*>') then P1 := Ln-1  // toto <toto@toto.com> | "toto" <toto@toto.com> | (toto) <toto@toto.com>
  else if ALMatchesMaskA(FriendlyEmail, '<*> *') then P1 := 2 // <toto@toto.com> toto.com | <toto@toto.com> "toto" | <toto@toto.com> (toto)
  else if ALMatchesMaskA(FriendlyEmail, '<*>') then P1 := 2 // <toto@toto.com>
  else if ALMatchesMaskA(FriendlyEmail, '* (*)') then P1 := 1 // toto@toto.com (toto)
  else Begin
    RealName := '';
    Result := FriendlyEmail;
    Exit;
  end;

  //----------
  Result := FriendlyEmail[P1];

  //----------
  var P2: Integer := P1-1;
  While (P2 > 0) and (FriendlyEmail[P2] in ['a'..'z','A'..'Z','0'..'9','_','-','.','@']) do begin
    Result := FriendlyEmail[P2] + Result;
    dec(P2);
  end;
  While (P2 > 0) and (FriendlyEmail[P2] in [' ',#9]) do dec(P2);

  var P3: Integer := P1+1;
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
      var S1: AnsiString := Result;

      var P4: Integer := P2;
      While (P4 > 0) and (FriendlyEmail[P4] <> '<') do
        if (FriendlyEmail[P4] = '>') then begin
          P4 := 0;
          Break;
        end
        else begin
          S1 := FriendlyEmail[P4] + S1;
          dec(P4);
        end;

      var P5: Integer := P3;
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
   RealName := ALStringReplaceA(RealName,'\\',#1,[rfIgnoreCase,RfReplaceAll]);
   RealName := ALStringReplaceA(RealName,'\','',[rfIgnoreCase,RfReplaceAll]);
   RealName := ALStringReplaceA(RealName,#1,'\',[rfIgnoreCase,RfReplaceAll]);
  end
  else if (ln >= 2) and
          (((RealName[1] = '(') and (RealName[ln] = ')')) or
           ((RealName[1] = '<') and (RealName[ln] = '>')) or
           ((RealName[1] = '"') and (RealName[ln] = '"'))) then RealName := alCopyStr(RealName,2,Ln-2)

end;

{**************************************************************************}
function AlExtractEmailAddress(const FriendlyEmail: AnsiString): AnsiString;
Begin
  Var LRealName: AnsiString;
  Result := AlParseEmailAddress(FriendlyEmail, LRealName, False);
end;

{**************************************************************************}
{Return a new string with backslashes in str replaced by two backslashes and
 double quotes replaced by backslash-double quote.}
Function ALEncodeRealName4FriendlyEmailAddress(const aRealName: AnsiString): AnsiString;
begin
  Result := '';
  var L: integer := Length(aRealName);
  if L = 0 then exit;
  var Buf: PAnsiChar;
  GetMem(Buf, (L * 2) + 2); // to be on the *very* safe side
  try
    var Ch: AnsiChar := '"';
    var P: PAnsiChar := Buf;
    ALMove(Ch, P^, 1);
    inc(P,1);
    for var i := 1 to L do begin
      var x: integer := Ord(aRealName[i]);
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
  Result := ALStringReplaceA(ALNewGUIDStringA(true{WithoutBracket}),'-','',[rfReplaceAll]) {$IF defined(MSWINDOWS)}+ '@' + ALGetLocalDnsIdentityA{$ENDIF};
end;

{****************************************************************************}
Function AlGenerateInternetMessageID(const ahostname: AnsiString): AnsiString;
Begin
  var LTmpHostName: ansiString := ALTrim(ahostname);
  If LTmpHostName <> '' then Result := ALStringReplaceA(ALNewGUIDStringA(true{WithoutBracket}),'-','',[rfReplaceAll]) + '@' + LTmpHostName
  else Result := AlGenerateInternetMessageID;
end;

{*********************}
{$ZEROBASEDSTRINGS OFF}
{$WARN SYMBOL_DEPRECATED OFF}
function AlIsValidEmail(const Value: AnsiString): boolean;

 {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
 function CheckAllowedName(const s: AnsiString): boolean;
 begin
   Result:= false;
   for var I := low(s) to high(s) do begin
     // illegal char in s -> no valid address
     if not CharInSet(s[I], ['a'..'z','A'..'Z','0'..'9','_','-','.','+']) then Exit;
   end;
   Result:= true;
 end;

 {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
 function CheckAllowedHostname(const s: AnsiString): boolean;
 begin
   Result:= false;
   for var I := low(s) to high(s) do begin
     // illegal char in s -> no valid address
     if not CharInSet(s[I], ['a'..'z','A'..'Z','0'..'9','-','.']) then Exit;
   end;
   Result:= true;
 end;

 {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
 function CheckAllowedExt(const s: AnsiString): boolean;
 begin
   Result:= false;
   for var I := low(s) to high(s) do begin
     // illegal char in s -> no valid address
     if not CharInSet(s[I], ['a'..'z','A'..'Z']) then Exit;
   end;
   Result:= true;
 end;

begin
  Result := false;

  // Value can not be < 6 char (ex: a@b.fr)
  if length(Value) < 6 then exit;

  // must have the '@' char inside
  var I: integer := ALPosA('@', Value);
  if (I <= 1) or (I > length(Value)-4) then exit;

  //can not have @. or .@
  if (value[I-1] = '.') or (value[I+1] = '.') then exit;

  //can not have 2 ..
  If (ALPosA('..', Value) > 0) then Exit;

  //extract namePart and serverPart
  var namePart: AnsiString := AlCopyStr(Value, 1, I - 1);
  var serverPart: AnsiString := AlCopyStr(Value, I + 1, Length(Value));

  // Extension (.fr, .com, etc..) must be betwen 2 to 6 char
  I:= ALPosA('.', serverPart);
  var J: Integer := 0;
  While I > 0 do begin
    J := I;
    I := ALPosA('.', serverPart, I + 1);
  end;
  if (J <= 1) then Exit; // no dot at all so exit !
  var extPart: AnsiString := AlCopyStr(ServerPart,J+1,Maxint);
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
{$ENDIF}

{*********************}
{$ZEROBASEDSTRINGS OFF}
{$WARN SYMBOL_DEPRECATED OFF}
function AlIsValidEmail(const Value: String): boolean;

 {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
 function CheckAllowedName(const s: String): boolean;
 begin
   Result:= false;
   for var I := low(s) to high(s) do begin
     // illegal char in s -> no valid address
     if not CharInSet(s[I], ['a'..'z','A'..'Z','0'..'9','_','-','.','+']) then Exit;
   end;
   Result:= true;
 end;

 {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
 function CheckAllowedHostname(const s: String): boolean;
 begin
   Result:= false;
   for var I := low(s) to high(s) do begin
     // illegal char in s -> no valid address
     if not CharInSet(s[I], ['a'..'z','A'..'Z','0'..'9','-','.']) then Exit;
   end;
   Result:= true;
 end;

 {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
 function CheckAllowedExt(const s: String): boolean;
 begin
   Result:= false;
   for var I := low(s) to high(s) do begin
     // illegal char in s -> no valid address
     if not CharInSet(s[I], ['a'..'z','A'..'Z']) then Exit;
   end;
   Result:= true;
 end;

begin
  Result := false;

  // Value can not be < 6 char (ex: a@b.fr)
  if length(Value) < 6 then exit;

  // must have the '@' char inside
  var I: Integer := ALPosW('@', Value);
  if (I <= 1) or (I > length(Value)-4) then exit;

  //can not have @. or .@
  if (value[I-1] = '.') or (value[I+1] = '.') then exit;

  //can not have 2 ..
  If (ALPosW('..', Value) > 0) then Exit;

  //extract namePart and serverPart
  var namePart: String := ALCopyStr(Value, 1, I - 1);
  var serverPart: String := ALCopyStr(Value, I + 1, Length(Value));

  // Extension (.fr, .com, etc..) must be betwen 2 to 6 char
  I:= ALPosW('.', serverPart);
  var J: Integer := 0;
  While I > 0 do begin
    J := I;
    I := ALPosW('.', serverPart, I + 1);
  end;
  if (J <= 1) then Exit; // no dot at all so exit !
  var extPart: String := ALCopyStr(ServerPart,J+1,Maxint);
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
{$ENDIF}

{***********************************************************************************************************}
procedure ALParseEmlMessage(const AFileName: String; const AHeaders: TALMailHeadersA; out ABody: AnsiString);
begin
  Var LEml := ALGetStringFromFile(AFileName);
  var P1 := ALPosA(#13#10#13#10, LEml);
  If P1 <= 0 then raise Exception.Create('Invalid EML format: header/body separator not found');
  AHeaders.RawHeaderText := AlCopystr(LEml, 1, P1-1);
  ABody := AlCopystr(LEml, P1+4, MaxInt);
end;

{*********************************}
constructor TALMailHeadersA.Create;
Begin
  inherited;
  //for var i := Low(FKnownHeaders) to High(FKnownHeaders) do
  //  FKnownHeaders[i] := '';
  FUnknownHeaders := nil;
  FAutoFillDefaults := True;
end;

{*********************************}
destructor TALMailHeadersA.Destroy;
begin
  AlFreeAndNil(FUnknownHeaders);
  inherited;
end;

{******************************************************************************}
function TALMailHeadersA.PropertyIndexToName(const AIndex: Integer): AnsiString;
begin
  Case AIndex of
    0: Result := 'From';
    1: Result := 'Sender';
    2: Result := 'To';
    3: Result := 'Cc';
    4: Result := 'Bcc';
    5: Result := 'Reply-To';
    6: Result := 'Subject';
    7: Result := 'Message-ID';
    8: Result := 'In-Reply-To';
    9: Result := 'References';
    10: Result := 'Comments';
    11: Result := 'Date';
    12: Result := 'Content-Type';
    13: Result := 'Content-Transfer-Encoding';
    14: Result := 'MIME-Version';
    15: Result := 'Importance';
    16: Result := 'Disposition-Notification-To';
    else
      Raise Exception.Create('Error D5AF7D49-0552-4570-9AA4-9407588DC529')
  End;
end;

{*****************************************************************************}
function TALMailHeadersA.NameToPropertyIndex(const AName: AnsiString): Integer;
begin
  var LLowerName := ALLowerCase(AName);
       if LLowerName = 'from' then Result := 0
  else if LLowerName = 'sender' then Result := 1
  else if LLowerName = 'to' then Result := 2
  else if LLowerName = 'cc' then Result := 3
  else if LLowerName = 'bcc' then Result := 4
  else if LLowerName = 'reply-to' then Result := 5
  else if LLowerName = 'subject' then Result := 6
  else if LLowerName = 'message-id' then Result := 7
  else if LLowerName = 'in-reply-to' then Result := 8
  else if LLowerName = 'references' then Result := 9
  else if LLowerName = 'comments' then Result := 10
  else if LLowerName = 'date' then Result := 11
  else if LLowerName = 'content-type' then Result := 12
  else if LLowerName = 'content-transfer-encoding' then Result := 13
  else if LLowerName = 'mime-version' then Result := 14
  else if LLowerName = 'importance' then Result := 15
  else if LLowerName = 'disposition-notification-to' then Result := 16
  else Result := -1;
end;

{*********************************************************************************}
function TALMailHeadersA.GetHeaderValueByName(const AName: AnsiString): AnsiString;
begin
  Var LIndex := NameToPropertyIndex(AName);
  If LIndex >= 0 then Result := GetHeaderValueByPropertyIndex(LIndex)
  else Result := UnknownHeaders.Values[AName];
end;

{************************************************************************************************}
procedure TALMailHeadersA.SetHeaderValueByName(const AName: AnsiString; const AValue: AnsiString);
begin
  Var LIndex := NameToPropertyIndex(AName);
  If LIndex >= 0 then
    SetHeaderValueByPropertyIndex(LIndex, AValue)
  else
    UnknownHeaders.Values[AName] := AValue;
end;

{*****************************************************}
function TALMailHeadersA.GetContentCharset: AnsiString;
begin
  Result := ALExtractHttpCharsetFromContentType(ContentType);
end;

{******************************}
procedure TALMailHeadersA.Clear;
begin
  for var i := Low(FKnownHeaders) to High(FKnownHeaders) do
    FKnownHeaders[i] := '';
  if FUnknownHeaders <> nil then FUnknownHeaders.Clear;
end;

{******************************************************}
function TALMailHeadersA.GetUnknownHeaders: TALStringsA;
begin
  if FUnknownHeaders = nil then begin
    FUnknownHeaders := TALNVStringListA.Create;
    FUnknownHeaders.NameValueSeparator := ':';
    FUnknownHeaders.TrailingLineBreak := False;
  end;
  Result := FUnknownHeaders;
end;

{***************************************************************************************}
function TALMailHeadersA.GetHeaderValueByPropertyIndex(const Index: Integer): AnsiString;
begin
  {$IF defined(DEBUG)}
  if (Index < Low(FKnownHeaders)) or (Index > High(FKnownHeaders)) then
    raise EArgumentOutOfRangeException.CreateFmt('Header index (%d) out of range [0..%d]', [Index, High(FKnownHeaders)]);
  {$ENDIF}
  Result := FKnownHeaders[Index];
end;

{*****************************************************************************************************}
procedure TALMailHeadersA.SetHeaderValueByPropertyIndex(const Index: Integer; const Value: AnsiString);
begin
  {$IF defined(DEBUG)}
  if (Index < Low(FKnownHeaders)) or (Index > High(FKnownHeaders)) then
    raise EArgumentOutOfRangeException.CreateFmt('Header index (%d) out of range [0..%d]', [Index, High(FKnownHeaders)]);
  {$ENDIF}
  FKnownHeaders[Index] := Value;
end;

{****************************************************}
Function TALMailHeadersA.GetRawHeaderText: AnsiString;
begin

  var LPrevMessageID: ansiString := MessageID;
  var LPrevMIMEVersion: ansiString := MIMEVersion;
  var LPrevDate: ansiString := Date;
  var LPrevSubject: ansiString := Subject;
  try

    if FAutoFillDefaults then begin
      if MessageID = '' then MessageID := '<' + AlGenerateInternetMessageID + '>';
      if MIMEVersion = '' then MIMEVersion := '1.0';
      if Date = '' then Date := ALLocalDateTimeToRfc1123StrA(Now);
      if (Subject <> '') and (alposIgnoreCaseA('=?UTF-8?', Subject) <> 1) then
        Subject := '=?UTF-8?B?' + ALBase64EncodeString(Subject) + '?=';
    end;

    var SB := TALStringBuilderA.Create(2048);
    try
      // 1) Known headers
      for var I := Low(FKnownHeaders) to High(FKnownHeaders) do begin
        if FKnownHeaders[i] <> '' then begin
          SB.Append(PropertyIndexToName(i));
          SB.Append(': ');
          SB.AppendLine(FKnownHeaders[i]);
        end;
      end;

      // 3) Unknown headers
      for var I := 0 to UnknownHeaders.Count - 1 do begin
        SB.Append(UnknownHeaders.Names[I]);
        SB.Append(': ');
        SB.AppendLine(UnknownHeaders.ValueFromIndex[I]);
      end;

      // 4) Produce the final string
      Result := SB.ToString(true{AUpdateCapacity});
    finally
      ALFreeAndNil(SB);
    end;

  finally
    MessageID := LPrevMessageID;
    MIMEVersion := LPrevMIMEVersion;
    Date := LPrevDate;
    Subject := LPrevSubject;
  end;
end;

{***************************************************************************}
procedure TALMailHeadersA.SetRawHeaderText(const ARawHeaderText: AnsiString);
begin
  Clear;
  var LRawHeaders := TALNVStringListA.create;
  try
    LRawHeaders.NameValueSeparator := ':';
    LRawHeaders.Text := ARawHeaderText;
    For var I := 0 to LRawHeaders.count - 1 do
      Values[ALTrim(LRawHeaders.Names[I])] := alTrim(LRawHeaders.ValueFromIndex[I]);
  finally
    AlFreeAndNil(LRawHeaders);
  end;
end;

end.