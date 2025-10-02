unit Alcinoe.Net;

interface

{$I Alcinoe.inc}

{$IF defined(MSWindows)}
uses
  Winapi.Winsock2;
{$ENDIF}

type
  TALIPv6Binary = array[0..15] of Byte;

const
  ALZeroIpV6: TALIPv6Binary = (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0);

function ALIsZeroIPv4(const AIPv4: Cardinal): Boolean;
function ALTryIPV4StrToNumeric(const AIPv4Str: AnsiString; out AIPv4Num: Cardinal): Boolean;
function ALIPV4StrToNumeric(const AIPv4: AnsiString): Cardinal;
function ALNumericToIPv4StrA(const AIPv4: Cardinal): AnsiString; overload;
function ALNumericToIPv4StrW(const AIPv4: Cardinal): String; overload;
function ALIPv4EndOfRange(const AStartIPv4: Cardinal; const AMaskLength: integer): Cardinal;

function ALIsZeroIPv6(const AIPV6: TALIPv6Binary): Boolean;
function ALTryIPV6StrToBinary(const AIPV6Str: AnsiString; out AIPV6Bin: TALIPv6Binary): Boolean;
function ALIPV6StrTobinary(const AIPV6: AnsiString): TALIPv6Binary;
function ALBinaryToIPv6StrA(const AIPV6: TALIPv6Binary): AnsiString; overload;
function ALBinaryToIPv6StrW(const AIPV6: TALIPv6Binary): String; overload;
function ALIPv6EndOfRange(const AStartIPv6: TALIPv6Binary; const AMaskLength: integer): TALIPv6Binary;

Type
  TALIpFamily = (IpUnspecified, IpV4, IpV6);

  TALNetEndpoint = class
  private
    FFamily: TALIpFamily; // IpV4 or IpV6
    FIpV4: Cardinal;      // IPv4 address (0 for IPv6)
    FIpV6: TALIPv6Binary; // IPv6 address (zeroed for IPv4)
    FPort: Word;          // Port number
  public
    constructor Create; overload; virtual;
    constructor Create(const AIPV4: Cardinal; const APort: Word); overload; virtual;
    constructor Create(const AIpV6: TALIPv6Binary; const APort: Word); overload; virtual;
    {$IF defined(MSWindows)}
    constructor Create(const ASOCKADDR: PSOCKADDR); overload; virtual;
    {$ENDIF}
    property Family: TALIpFamily read FFamily;
    property IpV4: Cardinal read FIpV4;
    property IpV6: TALIPv6Binary read FIpV6;
    property Port: Word read FPort;
  end;

implementation

uses
  {$IF defined(MSWindows)}
  Winapi.ShellAPI,
  {$ENDIF}
  System.SysUtils,
  Alcinoe.StringList,
  Alcinoe.StringUtils,
  Alcinoe.Common;

{****************************************************}
function ALIsZeroIPv4(const AIPv4: Cardinal): Boolean;
begin
  Result := AIPv4 = 0;
end;

{******************************************************************************************}
Function ALTryIPV4StrToNumeric(const AIPv4Str: AnsiString; out AIPv4Num: Cardinal): Boolean;
Var I1, I2, I3, I4: integer;
Begin

  // The function produces AIPv4Num = 2130706433 (0x7F000001), which represents
  // the IPv4 address in network byte order (big-endian), where the logical byte
  // order is:
  //   Byte 1 (most significant): 0x7F (127)
  //   Byte 2: 0x00 (0)
  //   Byte 3: 0x00 (0)
  //   Byte 4 (least significant): 0x01 (1)
  // This means the intended logical byte order is 7F 00 00 01, as expected for
  // network protocols like TCP/IP.
  //
  // However, since you’re on a little-endian system (x86/x64), the Cardinal
  // value 0x7F000001 is stored in memory with the least significant
  // byte first:
  //   Address 0: 01 (from I4 = 1)
  //   Address 1: 00 (from I3 = 0)
  //   Address 2: 00 (from I2 = 0)
  //   Address 3: 7F (from I1 = 127)
  // So, inspecting the raw memory of AIPv4Num shows 01 00 00 7F. When the CPU
  // reads this as a Cardinal, it interprets it as 0x7F000001 (2130706433)
  // because it accounts for the little-endian architecture.

  //----------
  if AIPv4Str = '' then begin
    Result := False;
    Exit;
  end;

  //----------
  var P1 := 1;
  var P2 := ALPosA('.',AIPv4Str, P1);
  if (P2 <= P1) or
     (not ALTryStrToInt(AlCopyStr(AIPv4Str,P1,P2-P1), I1)) or
     (not (I1 in [0..255])) then begin
    Result := False;
    Exit;
  end;

  //----------
  P1 := P2+1;
  P2 := ALPosA('.',AIPv4Str, P1);
  if (P2 <= P1) or
     (not ALTryStrToInt(AlCopyStr(AIPv4Str,P1,P2-P1), I2)) or
     (not (I2 in [0..255])) then begin
    Result := False;
    Exit;
  end;

  //----------
  P1 := P2+1;
  P2 := ALPosA('.',AIPv4Str, P1);
  if (P2 <= P1) or
     (not ALTryStrToInt(AlCopyStr(AIPv4Str,P1,P2-P1), I3)) or
     (not (I3 in [0..255])) then begin
    Result := False;
    Exit;
  end;

  //----------
  P1 := P2+1;
  P2 := length(AIPv4Str) + 1;
  if (P2 <= P1) or
     (not ALTryStrToInt(AlCopyStr(AIPv4Str,P1,P2-P1), I4)) or
     (not (I4 in  [0..255])) then begin
    Result := False;
    Exit;
  end;

  //----------
  Result := True;
  AIPv4Num := (cardinal(I1)*256*256*256) + (cardinal(I2)*256*256) +  (cardinal(I3)*256) + (cardinal(I4));

End;

{*************************************************************}
Function ALIPV4StrToNumeric(const AIPv4: AnsiString): Cardinal;
Begin
  if not ALTryIPV4StrToNumeric(AIPv4, Result) then
    Raise Exception.CreateFmt('Bad IPv4 string: %s', [AIPv4]);
End;

{**************************************************************}
Function ALNumericToIPv4StrA(const AIPv4: Cardinal): AnsiString;
Var S1, S2, S3, S4: AnsiString;
Begin

  S1 := ALIntToStrA(( AIPv4 div (256*256*256) ) mod 256);
  S2 := ALIntToStrA(( AIPv4 div (256*256)     ) mod 256);
  S3 := ALIntToStrA(( AIPv4 div (256)         ) mod 256);
  S4 := ALIntToStrA(( AIPv4                   ) mod 256);

  Result := S1 + '.' + S2 + '.' + S3 + '.' + S4;

End;

{**********************************************************}
function ALNumericToIPv4StrW(const AIPv4: Cardinal): String;
Var S1, S2, S3, S4: String;
Begin

  S1 := ALIntToStrW(( AIPv4 div (256*256*256) ) mod 256);
  S2 := ALIntToStrW(( AIPv4 div (256*256)     ) mod 256);
  S3 := ALIntToStrW(( AIPv4 div (256)         ) mod 256);
  S4 := ALIntToStrW(( AIPv4                   ) mod 256);

  Result := S1 + '.' + S2 + '.' + S3 + '.' + S4;

End;

{***********}
/// <summary>
/// This function calculates ending IPv4-address for a given start of IPv4 range and
/// length of subnetwork mask.
/// Calculation is described in RFC: http://tools.ietf.org/html/rfc1878,
/// calculator to test its behaviour - https://www.ultratools.com/tools/netMask.
/// There are declared that length of subnetwork represents number of addresses
/// like 2^(32 - n) - 2 where "n" is a length of subnetwork mask.
///
/// This way for the address:
///  1.0.1.0/26
///
/// the length will be 2^(32-26) - 2 = 2^6 - 2 = 62 addresses + 1 reserved for
/// the services purposes, so the last possible address in that range will be:
///  1.0.1.63
/// </summary>
function ALIPv4EndOfRange(const AStartIPv4: Cardinal; const AMaskLength: integer): Cardinal;
var LHostMask, LNetStart: Cardinal;
begin
  if (AMaskLength < 0) or
     (AMaskLength > 32) then
    raise EArgumentOutOfRangeException.CreateFmt('Wrong value for mask length IPv4: %d', [AMaskLength]);

  // Host mask = low (32 - m) bits set
  if AMaskLength = 0 then LHostMask := High(Cardinal) // /0 → 0xFFFFFFFF
  else LHostMask := (Cardinal(1) shl (32 - AMaskLength)) - 1;

  LNetStart := AStartIPv4 and not LHostMask; // ensure we’re aligned to the subnet
  Result := LNetStart or LHostMask;          // last address in that subnet (incl. broadcast)
end;

{*********************************************************}
function ALIsZeroIPv6(const AIPV6: TALIPv6Binary): Boolean;
begin
  result := CompareMem(@AIpV6, @ALZeroIpV6, SizeOf(TALIPv6Binary));
end;

{*********************}
{$ZEROBASEDSTRINGS OFF}
Function ALTryIPV6StrToBinary(const AIPV6Str: AnsiString; out AIPV6Bin: TALIPv6Binary): Boolean;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _HexToInt(I: Cardinal; Ch: AnsiChar): Cardinal;
  begin
    case Ch of
      '0'..'9': Result := I * 16 + Ord(Ch) - Ord('0');
      'a'..'f': Result := I * 16 + Ord(Ch) - Ord('a') + 10;
      'A'..'F': Result := I * 16 + Ord(Ch) - Ord('A') + 10;
      // Should be unreachable because the caller pre-validates hex digits
      else raise EALException.Create('Wrong HEX-character found');
    end;
  end;

begin

  if AIPV6Str = '' then
    Exit(False);

  // https://howdoesinternetwork.com/2013/ipv6-zone-id
  var LIpv6Str: AnsiString;
  var P1 := ALPosA('%', AIPV6Str); // fe80:3438:7667:5c77:ce27%18
  if (P1 > 0) then LIpv6Str := AlcopyStr(AIPV6Str,1,P1-1) // fe80:3438:7667:5c77:ce27
  else LIpv6Str := AIPV6Str;

  if (length(LIPV6Str) <= 1) or
     ((LIPV6Str[low(LIPV6Str)] = ':') and (LIPV6Str[low(LIPV6Str)+1] <> ':')) or
     ((LIPV6Str[high(LIPV6Str)] = ':') and (LIPV6Str[high(LIPV6Str) - 1] <> ':')) then exit(False);

  var LIpv6Parts := TALStringListA.Create;
  try

    LIpv6Parts.LineBreak := ':';
    LIpv6Parts.Text := LIpv6Str;

    if (LIpv6Parts.Count > 8) then
      Exit(False);

    // http://msdn.microsoft.com/en-us/library/aa921042.aspx
    // IPv6 representation can be further simplified by removing the leading zeros within each 16-bit block.
    // However, each block must have at least a single digit. The following example shows the address without
    // the leading zeros
    // 21DA:D3:0:2F3B:2AA:FF:FE28:9C5A => 21DA:00D3:0000:2F3B:02AA:00FF:FE28:9C5A
    //
    // Some types of addresses contain long sequences of zeros. In IPv6 addresses, a contiguous sequence of
    // 16-bit blocks set to 0 in the colon-hexadecimal format can be compressed to :: (known as double-colon).
    // The following list shows examples of compressing zeros
    // FF02::2 => FF02:0:0:0:0:0:0:2
    var LIPV4Encountered := False;
    var I := 0;
    while i <= LIpv6Parts.Count - 1 do begin
      var LPart := LIpv6Parts[i];
      if (AlposA('.', LPart) > 0) then begin
        var LAIPv4Num: Cardinal;
        If LIPV4Encountered or (i <> LIpv6Parts.Count - 1) or (not ALTryIPV4StrToNumeric(LPart, LAIPv4Num)) then
          Exit(false);
        var A, B, C, D: Byte;
        A := Byte(LAIPv4Num shr 24);
        B := Byte(LAIPv4Num shr 16);
        C := Byte(LAIPv4Num shr  8);
        D := Byte(LAIPv4Num);
        LIpv6Parts[i] := ALIntToHexA((A shl 8) or B, 4);
        inc(i);
        LIpv6Parts.Insert(i, ALIntToHexA((C shl 8) or D, 4));
        LIPV4Encountered := True;
      end
      else if length(LPart) > 4 then
        Exit(False)
      else if (length(LPart) > 0) and (length(LPart) < 4) then begin
        var Ln := 4 - length(LPart);
        SetLength(LPart,4);
        ALMove(PAnsiChar(LPart)^, (PAnsiChar(LPart) + Ln)^, 4 - Ln);
        FillChar(PAnsiChar(LPart)^, Ln, '0');
        LIpv6Parts[i] := LPart;
      end;
      inc(i);
    end;
    //--
    var LZeroCompressionEncountered: Boolean := False;
    I := 0;
    while i <= LIpv6Parts.Count - 1 do begin
      if (LIpv6Parts[i] = '') then begin
        if LZeroCompressionEncountered then
          Exit(False);
        if (i = 0) and
           (i < LIpv6Parts.Count - 1) and
           (LIpv6Parts[i+1] = '') then
          LIpv6Parts.Delete(i);
        LIpv6Parts[i] := '0000';
        while LIpv6Parts.Count < 8 do begin
          LIpv6Parts.Insert(i,'0000');
          inc(i);
        end;
        LZeroCompressionEncountered := True;
      end;
      inc(i);
    end;

    if (LIpv6Parts.Count <> 8) then
      Exit(False);

    for I := 0 to 7 do begin
      var LPart := LIpv6Parts[i];
      if (Length(LPart) = 4) and
         (LPart[1]  in ['A'..'F', 'a'..'f', '0'..'9']) and
         (LPart[2]  in ['A'..'F', 'a'..'f', '0'..'9']) and
         (LPart[3]  in ['A'..'F', 'a'..'f', '0'..'9']) and
         (LPart[4]  in ['A'..'F', 'a'..'f', '0'..'9']) then begin
        var Res: Cardinal := 0;
        for var J := 1 to 2 do
          Res := _HexToInt(Res, LPart[J]);
        if (not (Res in [0..255])) then Exit(False);
        AIPV6Bin[(i*2)] := Res;
        //--
        Res := 0;
        for var J := 3 to 4 do
          Res := _HexToInt(Res, LPart[J]);
        if (not (Res in [0..255])) then Exit(False);
        AIPV6Bin[(i*2) + 1] := Res;
      end
      else exit(False);
    end;

    Result := True;

  finally
    AlFreeAndNil(LIpv6Parts);
  end;

end;
{$IF defined(ALZEROBASEDSTRINGSON)}
  {$ZEROBASEDSTRINGS ON}
{$ENDIF}

{*****************************************************************}
Function ALIPV6StrTobinary(const AIPV6: AnsiString): TALIPv6Binary;
Begin
  if not ALTryIPv6StrToBinary(AIPV6, Result) then
    Raise EALException.CreateFmt('Bad IPv6 string: %s', [AIPV6]);
End;

{******************************************************************}
Function ALBinaryToIPv6StrA(const AIPV6: TALIPv6Binary): AnsiString;
Begin
  Result := ALIntToHexA(AIPV6[0], 2)  + ALIntToHexA(AIPV6[1], 2)  + ':' +
            ALIntToHexA(AIPV6[2], 2)  + ALIntToHexA(AIPV6[3], 2)  + ':' +
            ALIntToHexA(AIPV6[4], 2)  + ALIntToHexA(AIPV6[5], 2)  + ':' +
            ALIntToHexA(AIPV6[6], 2)  + ALIntToHexA(AIPV6[7], 2)  + ':' +
            ALIntToHexA(AIPV6[8], 2)  + ALIntToHexA(AIPV6[9], 2)  + ':' +
            ALIntToHexA(AIPV6[10], 2) + ALIntToHexA(AIPV6[11], 2) + ':' +
            ALIntToHexA(AIPV6[12], 2) + ALIntToHexA(AIPV6[13], 2) + ':' +
            ALIntToHexA(AIPV6[14], 2) + ALIntToHexA(AIPV6[15], 2);
End;

{**************************************************************}
function ALBinaryToIPv6StrW(const AIPV6: TALIPv6Binary): String;
Begin
  Result := ALIntToHexW(AIPV6[0], 2)  + ALIntToHexW(AIPV6[1], 2)  + ':' +
            ALIntToHexW(AIPV6[2], 2)  + ALIntToHexW(AIPV6[3], 2)  + ':' +
            ALIntToHexW(AIPV6[4], 2)  + ALIntToHexW(AIPV6[5], 2)  + ':' +
            ALIntToHexW(AIPV6[6], 2)  + ALIntToHexW(AIPV6[7], 2)  + ':' +
            ALIntToHexW(AIPV6[8], 2)  + ALIntToHexW(AIPV6[9], 2)  + ':' +
            ALIntToHexW(AIPV6[10], 2) + ALIntToHexW(AIPV6[11], 2) + ':' +
            ALIntToHexW(AIPV6[12], 2) + ALIntToHexW(AIPV6[13], 2) + ':' +
            ALIntToHexW(AIPV6[14], 2) + ALIntToHexW(AIPV6[15], 2);
End;

{***********}
/// <summary>
/// This function calculates ending IPv6-address for a given start of IPv6-range and
/// length of subnetwork mask.
/// RFC about IPv6 architecture is described here: https://tools.ietf.org/html/rfc4291.
/// Check out the also the good table representing the rules to calculate this ending address:
///  https://www.ripe.net/internet-coordination/press-centre/cidr_chart1.jpg
/// Calculator to test its behaviour: https://www.ultratools.com/tools/ipv6CIDRToRange
///
/// Assume that start IPv6: 2001:250:c20::/43,
/// /43 means that 128 - 43 = 85 bits starting from the lowest bit must be set to 1 and
/// we will get ending address.
///
/// So this address can be written like:
///  2001:250:c20:0:0:0:0:0
///
/// Setting 85 first bits of this address to 1 we will get
///  2001:250:c3f:ffff:ffff:ffff:ffff:ffff
/// </summary>
function ALIPv6EndOfRange(const AStartIPv6: TALIPv6Binary; const AMaskLength: integer): TALIPv6Binary;
begin
  if (AMaskLength < 0) or (AMaskLength > 128) then
    raise EArgumentOutOfRangeException.CreateFmt('Wrong value for mask length IPv6: %d', [AMaskLength]);

  Result := AStartIPv6;
  var LHostBits := 128 - AMaskLength; // number of host bits to set to 1

  // Work from last byte (least-significant) backwards
  for var i := 15 downto 0 do begin
    if LHostBits <= 0 then Break;
    if LHostBits >= 8 then begin
      Result[i] := Result[i] or $FF;
      Dec(LHostBits, 8);
    end
    else begin
      // Set the HostBits lowest bits to 1 in this byte
      Result[i] := Result[i] or Byte((1 shl LHostBits) - 1);
      Break;
    end;
  end;
end;

{********************************}
constructor TALNetEndpoint.Create;
begin
  inherited;
  FFamily := TALIpFamily.IpUnspecified;
  FIpV4 := 0;
  FIpV6 := ALZeroIpV6;
  FPort := 0;
end;

{**************************************************************************}
constructor TALNetEndpoint.Create(const AIPV4: Cardinal; const APort: Word);
begin
  inherited Create;
  FFamily := TALIpFamily.IpV4;
  FIpV4 := AIPV4;
  FIpV6 := ALZeroIpV6;
  FPort := APort;
end;

{*******************************************************************************}
constructor TALNetEndpoint.Create(const AIpV6: TALIPv6Binary; const APort: Word);
begin
  inherited Create;
  FFamily := TALIpFamily.IpV4;
  FIpV4 := 0;
  FIpV6 := AIpV6;
  FPort := APort;
end;

{**********************}
{$IF defined(MSWindows)}
constructor TALNetEndpoint.Create(const ASOCKADDR: PSOCKADDR);
begin
  inherited Create;
  if Assigned(ASOCKADDR) then begin
    case ASOCKADDR^.sa_family of
      // IPv4
      AF_INET: begin
        var LSockAddrIn := PSockAddrIn(ASOCKADDR);
        FFamily := TALIpFamily.IPv4;
        // S_addr is network byte order -> convert to host order for storage
        FIpV4 := ntohl(LSockAddrIn^.sin_addr.S_addr);
        FIpV6 := ALZeroIpV6;
        // sin_port is network order -> host order
        FPort := ntohs(LSockAddrIn^.sin_port);
      end;
      // IPv6
      AF_INET6: begin
        var LSockAddrIn6 := PSOCKADDR_IN6_LH(ASOCKADDR);
        FFamily := TALIpFamily.IPv6;
        FIpV4 := 0;
        // Copy 16 raw bytes (IPv6 is handled as an octet array; no swap)
        ALMove(LSockAddrIn6^.sin6_addr, FIpV6, SizeOf(TALIPv6Binary));
        // sin6_port is network order -> host order
        FPort := ntohs(LSockAddrIn6^.sin6_port);
      end;
      // Unspecified
      else begin
        FFamily := TALIpFamily.IpUnspecified;
        FIpV4 := 0;
        FIpV6 := ALZeroIpV6;
        FPort := 0;
      end;
    end;
  end;
end;
{$ENDIF}

end.