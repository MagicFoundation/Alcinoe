unit ALDUnitXTestNet;

interface

uses
  Alcinoe.Net,
  DUnitX.TestFramework;

type

  [TestFixture]
  TALDUnitXTestNet = class
  private
    procedure CheckIPv6Binary(const Expected: array of Byte; const Actual: TALIPv6Binary; const Msg: string);
    procedure FillIPv6Binary(var IPv6Bin: TALIPv6Binary; const Values: array of Word);
  public
    //constructor create;
    //[Setup]
    //procedure Setup;
    //[TearDown]
    //procedure TearDown;
    [Test]
    procedure TestALTryIPV4StrToNumeric;
    [Test]
    procedure TestALNumericToIPv4StrA;
    [Test]
    procedure TestALIPv4EndOfRange;
    [Test]
    procedure TestALTryIPV6StrToBinary;
    [Test]
    procedure TestALBinaryToIPv6StrA;
    [Test]
    procedure TestALIPv6EndOfRange;
  end;

implementation

uses
  system.AnsiStrings,
  System.SysUtils,
  System.DateUtils,
  Alcinoe.Url,
  Alcinoe.Localization,
  Alcinoe.StringUtils,
  Alcinoe.Common,
  Alcinoe.Http;

{*******************************************}
procedure TALDUnitXTestNet.CheckIPv6Binary(const Expected: array of byte; const Actual: TALIPv6Binary; const Msg: string);
var
  I: Integer;
begin
  Assert.AreEqual(16, Length(Actual), Msg + ' - Actual array length must be 16 bytes');
  for I := 0 to 15 do
    Assert.AreEqual(Expected[I], Actual[I], Msg + Format(' at byte position %d', [I]));
end;

{*******************************************}
procedure TALDUnitXTestNet.FillIPv6Binary(var IPv6Bin: TALIPv6Binary; const Values: array of Word);
var
  I: Integer;
begin
  Assert.IsTrue(Length(Values) = 8, 'Values array must contain exactly 8 words');
  for I := 0 to 7 do
  begin
    IPv6Bin[I * 2] := Byte(Values[I] shr 8); // High byte
    IPv6Bin[I * 2 + 1] := Byte(Values[I]);   // Low byte
  end;
end;

{*******************************************}
procedure TALDUnitXTestNet.TestALTryIPV4StrToNumeric;
var
  IPv4Num: Cardinal;
begin
  // Test valid IPv4 addresses
  Assert.IsTrue(ALTryIPV4StrToNumeric('192.168.1.1', IPv4Num), 'Valid IP 192.168.1.1 should return True');
  Assert.AreEqual(Cardinal($C0A80101), IPv4Num, '192.168.1.1 should convert to correct numeric value');

  Assert.IsTrue(ALTryIPV4StrToNumeric('0.0.0.0', IPv4Num), 'Valid IP 0.0.0.0 should return True');
  Assert.AreEqual(Cardinal(0), IPv4Num, '0.0.0.0 should convert to 0');

  Assert.IsTrue(ALTryIPV4StrToNumeric('255.255.255.255', IPv4Num), 'Valid IP 255.255.255.255 should return True');
  Assert.AreEqual(Cardinal($FFFFFFFF), IPv4Num, '255.255.255.255 should convert to correct numeric value');

  // Test invalid IPv4 addresses
  Assert.IsFalse(ALTryIPV4StrToNumeric('256.1.2.3', IPv4Num), 'Invalid octet (256) should return False');
  Assert.IsFalse(ALTryIPV4StrToNumeric('1.2.3', IPv4Num), 'Missing octet should return False');
  Assert.IsFalse(ALTryIPV4StrToNumeric('1.2.3.4.5', IPv4Num), 'Too many octets should return False');
  Assert.IsFalse(ALTryIPV4StrToNumeric('1.2.3.a', IPv4Num), 'Non-numeric octet should return False');
  Assert.IsFalse(ALTryIPV4StrToNumeric('', IPv4Num), 'Empty string should return False');
  Assert.IsFalse(ALTryIPV4StrToNumeric('192.168.1.', IPv4Num), 'Trailing dot should return False');
  Assert.IsFalse(ALTryIPV4StrToNumeric('192.168.1.1.1', IPv4Num), 'Too many octets with trailing number should return False');
end;

{*******************************************}
procedure TALDUnitXTestNet.TestALNumericToIPv4StrA;
begin
  // Test valid IPv4 numeric values
  Assert.AreEqual(AnsiString('192.168.1.1'), ALNumericToIPv4StrA($C0A80101), '0xC0A80101 should convert to 192.168.1.1');
  Assert.AreEqual(AnsiString('0.0.0.0'), ALNumericToIPv4StrA(0), '0 should convert to 0.0.0.0');
  Assert.AreEqual(AnsiString('255.255.255.255'), ALNumericToIPv4StrA($FFFFFFFF), '0xFFFFFFFF should convert to 255.255.255.255');
  Assert.AreEqual(AnsiString('10.0.0.1'), ALNumericToIPv4StrA($0A000001), '0x0A000001 should convert to 10.0.0.1');
  Assert.AreEqual(AnsiString('172.16.254.1'), ALNumericToIPv4StrA($AC10FE01), '0xAC10FE01 should convert to 172.16.254.1');
end;

{*******************************************}
procedure TALDUnitXTestNet.TestALIPv4EndOfRange;
begin
  // Test valid subnet ranges
  Assert.AreEqual(Cardinal($C0A801FF), ALIPv4EndOfRange($C0A80100, 24), '192.168.1.0/24 should end at 192.168.1.255');
  Assert.AreEqual(Cardinal($0A0000FF), ALIPv4EndOfRange($0A000000, 24), '10.0.0.0/24 should end at 10.0.0.255');
  Assert.AreEqual(Cardinal($AC10FFFF), ALIPv4EndOfRange($AC10FE00, 16), '172.16.254.0/16 should end at 172.16.255.255');
  Assert.AreEqual(Cardinal($C0A80101), ALIPv4EndOfRange($C0A80101, 32), '192.168.1.1/32 should end at 192.168.1.1');
  Assert.AreEqual(Cardinal($FFFFFFFF), ALIPv4EndOfRange($00000000, 0), '0.0.0.0/0 should end at 255.255.255.255');
  Assert.AreEqual(ALIPV4StrToNumeric('1.0.1.63'), ALIPv4EndOfRange(ALIPV4StrToNumeric('1.0.1.0'), 26), '1.0.1.0/26 should end at 1.0.1.63');

  // Test edge cases for mask length
  Assert.AreEqual(Cardinal($0A000001), ALIPv4EndOfRange($0A000001, 32), '10.0.0.1/32 should end at 10.0.0.1');
  Assert.AreEqual(Cardinal($FFFFFFFF), ALIPv4EndOfRange($C0A80101, 0), '192.168.1.1/0 should end at 255.255.255.255');
end;

{*******************************************}
procedure TALDUnitXTestNet.TestALTryIPV6StrToBinary;
var
  IPv6Bin: TALIPv6Binary;
begin
  // Test valid full IPv6 address
  Assert.IsTrue(ALTryIPV6StrToBinary('2001:0db8:85a3:0000:0000:8a2e:0370:7334', IPv6Bin), 'Valid full IPv6 should return True');
  CheckIPv6Binary([$20, $01, $0D, $B8, $85, $A3, $00, $00, $00, $00, $8A, $2E, $03, $70, $73, $34], IPv6Bin, '2001:0db8:85a3:0000:0000:8a2e:0370:7334 conversion failed');

  // Test valid uppercase IPv6 address
  Assert.IsTrue(ALTryIPV6StrToBinary('2001:0DB8:85A3:0000:0000:8A2E:0370:7334', IPv6Bin), 'Valid uppercase IPv6 should return True');
  CheckIPv6Binary([$20, $01, $0D, $B8, $85, $A3, $00, $00, $00, $00, $8A, $2E, $03, $70, $73, $34], IPv6Bin, '2001:0DB8:85A3:0000:0000:8A2E:0370:7334 conversion failed');

  // Test valid mixed-case IPv6 address
  Assert.IsTrue(ALTryIPV6StrToBinary('2001:Db8:85a3:0:0:8A2e:370:7334', IPv6Bin), 'Valid mixed-case IPv6 should return True');
  CheckIPv6Binary([$20, $01, $0D, $B8, $85, $A3, $00, $00, $00, $00, $8A, $2E, $03, $70, $73, $34], IPv6Bin, '2001:Db8:85a3:0:0:8A2e:370:7334 conversion failed');

  // Test valid compressed IPv6 address
  Assert.IsTrue(ALTryIPV6StrToBinary('2001:db8::8a2e:370:7334', IPv6Bin), 'Valid compressed IPv6 should return True');
  CheckIPv6Binary([$20, $01, $0D, $B8, $00, $00, $00, $00, $00, $00, $8A, $2E, $03, $70, $73, $34], IPv6Bin, '2001:db8::8a2e:370:7334 conversion failed');

  // Test valid loopback address (starts with ::)
  Assert.IsTrue(ALTryIPV6StrToBinary('::1', IPv6Bin), 'Loopback address ::1 should return True');
  CheckIPv6Binary([$00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $01], IPv6Bin, '::1 conversion failed');

  // Test valid unspecified address (starts with ::)
  Assert.IsTrue(ALTryIPV6StrToBinary('::', IPv6Bin), 'Unspecified address :: should return True');
  CheckIPv6Binary([$00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00], IPv6Bin, ':: conversion failed');

  // Test valid address starting with ::
  Assert.IsTrue(ALTryIPV6StrToBinary('::1234:5678', IPv6Bin), 'Address starting with ::1234:5678 should return True');
  CheckIPv6Binary([$00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $12, $34, $56, $78], IPv6Bin, '::1234:5678 conversion failed');

  // Test valid IPv4-mapped address starting with ::
  Assert.IsTrue(ALTryIPV6StrToBinary('::ffff:0:0', IPv6Bin), 'IPv4-mapped ::ffff:0:0 should return True');
  CheckIPv6Binary([$00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $FF, $FF, $00, $00, $00, $00], IPv6Bin, '::ffff:0:0 conversion failed');

  // Test valid address ending with ::
  Assert.IsTrue(ALTryIPV6StrToBinary('1::', IPv6Bin), 'Address ending with 1:: should return True');
  CheckIPv6Binary([$00, $01, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00], IPv6Bin, '1:: conversion failed');

  // Test valid address ending with ::
  Assert.IsTrue(ALTryIPV6StrToBinary('2001:db8:1234::', IPv6Bin), 'Address ending with 2001:db8:1234:: should return True');
  CheckIPv6Binary([$20, $01, $0D, $B8, $12, $34, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00], IPv6Bin, '2001:db8:1234:: conversion failed');

  // Test valid address with :: in the middle
  Assert.IsTrue(ALTryIPV6StrToBinary('2001:db8::1', IPv6Bin), 'Address with :: in middle 2001:db8::1 should return True');
  CheckIPv6Binary([$20, $01, $0D, $B8, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $01], IPv6Bin, '2001:db8::1 conversion failed');

  // Test valid address with :: in the middle (multiple zero sections)
  Assert.IsTrue(ALTryIPV6StrToBinary('2001:0:0:1::1234:5678', IPv6Bin), 'Address with :: in middle 2001:0:0:1::1234:5678 should return True');
  CheckIPv6Binary([$20, $01, $00, $00, $00, $00, $00, $01, $00, $00, $00, $00, $12, $34, $56, $78], IPv6Bin, '2001:0:0:1::1234:5678 conversion failed');

  // Test valid maximum address
  Assert.IsTrue(ALTryIPV6StrToBinary('ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff', IPv6Bin), 'Maximum address should return True');
  CheckIPv6Binary([$FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF], IPv6Bin, 'ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff conversion failed');

  // Test valid IPv6 with mixed compression
  Assert.IsTrue(ALTryIPV6StrToBinary('2001:db8:0:0:1::1', IPv6Bin), 'Mixed compression 2001:db8:0:0:1::1 should return True');
  CheckIPv6Binary([$20, $01, $0D, $B8, $00, $00, $00, $00, $00, $01, $00, $00, $00, $00, $00, $01], IPv6Bin, '2001:db8:0:0:1::1 conversion failed');

  // Test valid IPv4-mapped IPv6 address
  Assert.IsTrue(ALTryIPV6StrToBinary('::ffff:192.168.1.1', IPv6Bin), 'IPv4-mapped ::ffff:192.168.1.1 should return True');
  CheckIPv6Binary([$00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $FF, $FF, $C0, $A8, $01, $01], IPv6Bin, '::ffff:192.168.1.1 conversion failed');

  // Test valid IPv4-compatible IPv6 address (deprecated)
  Assert.IsTrue(ALTryIPV6StrToBinary('::192.168.1.1', IPv6Bin), 'IPv4-compatible ::192.168.1.1 should return True');
  CheckIPv6Binary([$00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $C0, $A8, $01, $01], IPv6Bin, '::192.168.1.1 conversion failed');

  // Test valid link-local address with scope identifier
  Assert.IsTrue(ALTryIPV6StrToBinary('fe80::1ff:fe23:4567:890a%eth0', IPv6Bin), 'Link-local with scope fe80::1ff:fe23:4567:890a%eth0 should return True');
  CheckIPv6Binary([$FE, $80, $00, $00, $00, $00, $00, $00, $01, $FF, $FE, $23, $45, $67, $89, $0A], IPv6Bin, 'fe80::1ff:fe23:4567:890a%eth0 conversion failed');

  // Test valid link-local with numeric scope
  Assert.IsTrue(ALTryIPV6StrToBinary('fe80::1%1', IPv6Bin), 'Link-local with numeric scope fe80::1%1 should return True');
  CheckIPv6Binary([$FE, $80, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $01], IPv6Bin, 'fe80::1%1 conversion failed');

  // Test valid link-local with scope containing special characters
  Assert.IsTrue(ALTryIPV6StrToBinary('fe80::1%eth-0', IPv6Bin), 'Link-local with scope fe80::1%eth-0 should return True');
  CheckIPv6Binary([$FE, $80, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $01], IPv6Bin, 'fe80::1%eth-0 conversion failed');

  // Hex-tail equivalent of mapped IPv4
  Assert.IsTrue(ALTryIPV6StrToBinary('::ffff:c0a8:0101', IPv6Bin), 'Hex tail ::ffff:c0a8:0101 should return True');
  CheckIPv6Binary([$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$FF,$FF,$C0,$A8,$01,$01], IPv6Bin, '::ffff:c0a8:0101 conversion failed');

  // IPv4-mapped boundaries
  Assert.IsTrue(ALTryIPV6StrToBinary('::ffff:0.0.0.0', IPv6Bin), 'Mapped lower boundary should return True');
  CheckIPv6Binary([$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$FF,$FF,$00,$00,$00,$00], IPv6Bin, '::ffff:0.0.0.0 conversion failed');

  Assert.IsTrue(ALTryIPV6StrToBinary('::ffff:255.255.255.255', IPv6Bin), 'Mapped upper boundary should return True');
  CheckIPv6Binary([$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$FF,$FF,$FF,$FF,$FF,$FF], IPv6Bin, '::ffff:255.255.255.255 conversion failed');

  // IPv4 with leading zeros in octets (commonly accepted)
  Assert.IsTrue(ALTryIPV6StrToBinary('::ffff:192.168.001.001', IPv6Bin), 'Mapped with leading-zero IPv4 should return True');
  CheckIPv6Binary([$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$FF,$FF,$C0,$A8,$01,$01], IPv6Bin, '::ffff:192.168.001.001 conversion failed');

  // IPv6 with embedded IPv4 not using ::ffff: (6 hextets + IPv4 tail is valid)
  Assert.IsTrue(ALTryIPV6StrToBinary('2001:db8:1:2:3:4:192.0.2.33', IPv6Bin), '6-hextets + IPv4 tail should return True');
  CheckIPv6Binary([$20,$01,$0D,$B8,$00,$01,$00,$02,$00,$03,$00,$04,$C0,$00,$02,$21], IPv6Bin, '2001:db8:1:2:3:4:192.0.2.33 conversion failed');

  // Trailing compression for whole low part
  Assert.IsTrue(ALTryIPV6StrToBinary('2001:db8::', IPv6Bin), 'Trailing :: should return True');
  CheckIPv6Binary([$20,$01,$0D,$B8,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00], IPv6Bin, '2001:db8:: conversion failed');

  // Minimal hex hextet width (no leading zero) still valid
  Assert.IsTrue(ALTryIPV6StrToBinary('::ffff:c0a8:101', IPv6Bin), 'Short hex hextet width should return True');
  CheckIPv6Binary([$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$FF,$FF,$C0,$A8,$01,$01], IPv6Bin, '::ffff:c0a8:101 conversion failed');

  // ::0 is valid and equals ::
  Assert.IsTrue(ALTryIPV6StrToBinary('::0', IPv6Bin), '::0 should return True');
  CheckIPv6Binary([$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00], IPv6Bin, '::0 conversion failed');

  // Test invalid IPv6 addresses
  Assert.IsFalse(ALTryIPV6StrToBinary('2001:0db8:85a3:0000:0000:8a2e:0370:7334:1', IPv6Bin), 'Too many segments should return False');
  Assert.IsFalse(ALTryIPV6StrToBinary('2001:0db8::8a2e::1', IPv6Bin), 'Multiple :: should return False');
  Assert.IsFalse(ALTryIPV6StrToBinary('2001:0db8:85a3:gggg:0000:8a2e:0370:7334', IPv6Bin), 'Invalid hex segment should return False');
  Assert.IsFalse(ALTryIPV6StrToBinary('2001:0db8:85a3:10000:0000:8a2e:0370:7334', IPv6Bin), 'Overlarge hex segment should return False');
  Assert.IsFalse(ALTryIPV6StrToBinary('2001:0db8:85a3:0000:0000:8a2e:0370', IPv6Bin), 'Missing segment should return False');
  Assert.IsFalse(ALTryIPV6StrToBinary('', IPv6Bin), 'Empty string should return False');
  Assert.IsFalse(ALTryIPV6StrToBinary('192.168.1.1', IPv6Bin), 'IPv4 address alone should return False');
  Assert.IsFalse(ALTryIPV6StrToBinary('2001:0db8:85a3:0000:0000:8a2e:0370:', IPv6Bin), 'Trailing colon should return False');
  Assert.IsFalse(ALTryIPV6StrToBinary('z::1', IPv6Bin), 'Invalid character should return False');
  Assert.IsFalse(ALTryIPV6StrToBinary('2001:0db8:85a3:0000:0000:8a2e:0370:7334:', IPv6Bin), 'Trailing colon with full address should return False');
  Assert.IsFalse(ALTryIPV6StrToBinary('::ffff:256.1.2.3', IPv6Bin), 'Invalid IPv4 in mapped address should return False');
  Assert.IsFalse(ALTryIPV6StrToBinary('::ffff:192.168.1', IPv6Bin), 'Incomplete IPv4 in mapped address should return False');
  Assert.IsFalse(ALTryIPV6StrToBinary(' 2001:db8::1 ', IPv6Bin), 'Whitespace around address should return False');
  Assert.IsFalse(ALTryIPV6StrToBinary('2001: db8::1', IPv6Bin), 'Internal whitespace should return False');
  Assert.IsFalse(ALTryIPV6StrToBinary('[2001:db8::1]', IPv6Bin), 'Bracketed address should return False');
  Assert.IsFalse(ALTryIPV6StrToBinary('2001::1234::', IPv6Bin), 'Invalid :: placement should return False');
  Assert.IsFalse(ALTryIPV6StrToBinary('2001:::1234', IPv6Bin), 'Empty segments around :: should return False');
  Assert.IsFalse(ALTryIPV6StrToBinary('2001:db8:1:2:3:4:5:192.0.2.33', IPv6Bin), '7-hextets + IPv4 tail should return False');
  Assert.IsFalse(ALTryIPV6StrToBinary('::ffff:192.168.1.1:1234', IPv6Bin), 'IPv4 tail must be last; should return False');
  Assert.IsFalse(ALTryIPV6StrToBinary(':1:2:3::4', IPv6Bin), 'Leading single colon is invalid');
  Assert.IsFalse(ALTryIPV6StrToBinary('::g', IPv6Bin), 'Invalid hex digit should return False');
  Assert.IsFalse(ALTryIPV6StrToBinary('%eth0', IPv6Bin), 'Lone scope id should return False');
end;

{************************************************}
procedure TALDUnitXTestNet.TestALBinaryToIPv6StrA;
var
  IPv6Binary: TALIPv6Binary;
  Result: AnsiString;
  Expected: AnsiString;
begin
  // Test Case 1: Standard IPv6 address (2001:0db8:85a3:0000:0000:8a2e:0370:7334)
  FillIPv6Binary(IPv6Binary, [$2001, $0DB8, $85A3, $0000, $0000, $8A2E, $0370, $7334]);
  Expected := '2001:0DB8:85A3:0000:0000:8A2E:0370:7334';
  Result := ALBinaryToIPv6StrA(IPv6Binary);
  Assert.AreEqual(Expected, Result, 'Standard IPv6 address string does not match expected format');

  // Test Case 2: All-zero IPv6 address (::)
  FillIPv6Binary(IPv6Binary, [$0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000]);
  Expected := '0000:0000:0000:0000:0000:0000:0000:0000';
  Result := ALBinaryToIPv6StrA(IPv6Binary);
  Assert.AreEqual(Expected, Result, 'All-zero IPv6 address string does not match expected format');

  // Test Case 3: Maximum value IPv6 address (ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff)
  FillIPv6Binary(IPv6Binary, [$FFFF, $FFFF, $FFFF, $FFFF, $FFFF, $FFFF, $FFFF, $FFFF]);
  Expected := 'FFFF:FFFF:FFFF:FFFF:FFFF:FFFF:FFFF:FFFF';
  Result := ALBinaryToIPv6StrA(IPv6Binary);
  Assert.AreEqual(Expected, Result, 'Maximum value IPv6 address string does not match expected format');
end;

{************************************************}
procedure TALDUnitXTestNet.TestALIPv6EndOfRange;
var
  StartIPv6, ResultIPv6, ExpectedIPv6: TALIPv6Binary;
begin
  // Test Case 1: Start address 2001:0db8:85a3::/48, expect end address 2001:0db8:85a3:ffff:ffff:ffff:ffff:ffff
  FillIPv6Binary(StartIPv6, [$2001, $0DB8, $85A3, $0000, $0000, $0000, $0000, $0000]);
  ResultIPv6 := ALIPv6EndOfRange(StartIPv6, 48);
  FillIPv6Binary(ExpectedIPv6, [$2001, $0DB8, $85A3, $FFFF, $FFFF, $FFFF, $FFFF, $FFFF]);
  CheckIPv6Binary([ $20, $01, $0D, $B8, $85, $A3, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF ], ResultIPv6, 'End of range for /48 does not match expected');

  // Test Case 2: Start address 2001:0db8::/32, expect end address 2001:0db8:ffff:ffff:ffff:ffff:ffff:ffff
  FillIPv6Binary(StartIPv6, [$2001, $0DB8, $0000, $0000, $0000, $0000, $0000, $0000]);
  ResultIPv6 := ALIPv6EndOfRange(StartIPv6, 32);
  FillIPv6Binary(ExpectedIPv6, [$2001, $0DB8, $FFFF, $FFFF, $FFFF, $FFFF, $FFFF, $FFFF]);
  CheckIPv6Binary([ $20, $01, $0D, $B8, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF ], ResultIPv6, 'End of range for /32 does not match expected');

  // Test Case 3: Start address ::/0, expect end address ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
  FillIPv6Binary(StartIPv6, [$0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000]);
  ResultIPv6 := ALIPv6EndOfRange(StartIPv6, 0);
  FillIPv6Binary(ExpectedIPv6, [$FFFF, $FFFF, $FFFF, $FFFF, $FFFF, $FFFF, $FFFF, $FFFF]);
  CheckIPv6Binary([ $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF ], ResultIPv6, 'End of range for /0 does not match expected');

  // Test Case 4: Start address 2001:0db8:85a3::/64, expect end address 2001:0db8:85a3:0000:ffff:ffff:ffff:ffff
  FillIPv6Binary(StartIPv6, [$2001, $0DB8, $85A3, $0000, $0000, $0000, $0000, $0000]);
  ResultIPv6 := ALIPv6EndOfRange(StartIPv6, 64);
  FillIPv6Binary(ExpectedIPv6, [$2001, $0DB8, $85A3, $0000, $FFFF, $FFFF, $FFFF, $FFFF]);
  CheckIPv6Binary([ $20, $01, $0D, $B8, $85, $A3, $00, $00, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF ], ResultIPv6, 'End of range for /64 does not match expected');

  // Test Case 5: /128 → end = start (no host bits)
  FillIPv6Binary(StartIPv6, [$2001, $0DB8, $85A3, $0000, $0000, $0000, $0000, $0001]);
  ResultIPv6 := ALIPv6EndOfRange(StartIPv6, 128);
  CheckIPv6Binary([ $20, $01, $0D, $B8, $85, $A3, $00, $00, $00, $00, $00, $00, $00, $00, $00, $01 ], ResultIPv6, 'End of range for /128 must equal start');

  // Test Case 6: /127 → only the very last bit is host; end should set it
  FillIPv6Binary(StartIPv6, [$0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000]);
  ResultIPv6 := ALIPv6EndOfRange(StartIPv6, 127);
  CheckIPv6Binary([ $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $01 ], ResultIPv6, 'End of range for /127 should set last bit to 1');

  // Test Case 7: /120 → last byte is host; end sets it to FF
  FillIPv6Binary(StartIPv6, [$2001, $0DB8, $0000, $0000, $0000, $0000, $0000, $00AA]);
  ResultIPv6 := ALIPv6EndOfRange(StartIPv6, 120);
  CheckIPv6Binary([ $20, $01, $0D, $B8, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $FF ], ResultIPv6, 'End of range for /120 should set last byte to FF');

  // Test Case 8: /96 on IPv4-mapped network (::ffff:0:0) → last 32 bits all ones
  FillIPv6Binary(StartIPv6, [$0000, $0000, $0000, $0000, $0000, $FFFF, $0000, $0000]); // ::ffff:0:0
  ResultIPv6 := ALIPv6EndOfRange(StartIPv6, 96);
  CheckIPv6Binary([ $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $FF, $FF, $FF, $FF, $FF, $FF ], ResultIPv6, 'End of range for /96 (mapped) should set last 32 bits');

  // Corrected Test Case 9: /8 → keep first byte; all others become FF
  FillIPv6Binary(StartIPv6, [$2001, $0DB8, $0000, $0000, $0000, $0000, $0000, $0000]); // 20 01 DB 8...
  ResultIPv6 := ALIPv6EndOfRange(StartIPv6, 8);
  CheckIPv6Binary([ $20, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF ], ResultIPv6, 'End of range for /8 should preserve first byte (=0x20) and set the rest to FF');

  // Test Case 10: /64 with non-aligned start → last 4 hextets become FFFF, first 4 unchanged
  FillIPv6Binary(StartIPv6, [$2001, $0DB8, $85A3, $1234, $5678, $9ABC, $DEF0, $1111]);
  ResultIPv6 := ALIPv6EndOfRange(StartIPv6, 64);
  CheckIPv6Binary([ $20, $01, $0D, $B8, $85, $A3, $12, $34, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF ], ResultIPv6, 'End of range for /64 should keep first 64 bits and set rest to 1');

  // Test Case 11: /125 → last 3 bits host; end sets last byte = 0x07
  FillIPv6Binary(StartIPv6, [$0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000]);
  ResultIPv6 := ALIPv6EndOfRange(StartIPv6, 125);
  CheckIPv6Binary([ $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $07 ], ResultIPv6, 'End of range for /125 should set last 3 bits');

  // Test Case 12: /111 → last 17 bits host; end sets them (…01:FF:FF at the tail)
  FillIPv6Binary(StartIPv6, [$0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000]);
  ResultIPv6 := ALIPv6EndOfRange(StartIPv6, 111);
  CheckIPv6Binary([ $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $01, $FF, $FF ], ResultIPv6, 'End of range for /111 should set 17 LSBs');

  // Test Case 13: /112 near-maximum start → last two bytes become FF:FF
  FillIPv6Binary(StartIPv6, [$FFFF, $FFFF, $FFFF, $FFFF, $FFFF, $FFFF, $0000, $0000]);
  ResultIPv6 := ALIPv6EndOfRange(StartIPv6, 112);
  CheckIPv6Binary([ $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $00, $00, $FF, $FF ], ResultIPv6, 'End of range for /112 should set last 16 bits');

  // Test Case 14: /1 with first bit zero → first byte becomes 0x7F, rest FF
  FillIPv6Binary(StartIPv6, [$2001, $0DB8, $0000, $0000, $0000, $0000, $0000, $0000]); // 0x20 in first byte → MSB=0
  ResultIPv6 := ALIPv6EndOfRange(StartIPv6, 1);
  CheckIPv6Binary([ $7F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF ], ResultIPv6, 'End of range for /1 with MSB=0 should be 7F:FF…FF');

end;

initialization
  TDUnitX.RegisterTestFixture(TALDUnitXTestNet);

end.
