unit ALDUnitXTestMimeMultipart;

interface

uses
  DUnitX.TestFramework;

type

  [TestFixture]
  TALDUnitXTestMimeMultipart = class
  public
    [Test]
    procedure TestBaseEncoder_BoundaryAndMimeTypeHeaders;
    [Test]
    procedure TestFormDataEncoder_AddText_Basic;
    [Test]
    procedure TestFormDataEncoder_AddText_WithContentTypeAndExtraHeaders;
    [Test]
    procedure TestFormDataEncoder_AddStream_WithFileNameAndDefaultMimeType;
    [Test]
    procedure TestFormDataEncoder_AddBytes;
    [Test]
    procedure TestFormDataEncoder_PayloadEndsWithClosingBoundary;

    [Test]
    procedure TestAlternativeEncoder_AddText_Base64Default;
    [Test]
    procedure TestAlternativeEncoder_AddText_OverrideTransferEncodingHeader;

    [Test]
    procedure TestMixedEncoder_AddText_Base64Default;
    [Test]
    procedure TestMixedEncoder_AddStream_AddsAttachmentDispositionIfMissing;
    [Test]
    procedure TestMixedEncoder_AddFile;
    [Test]
    procedure TestMixedEncoder_AddBytes;

    [Test]
    procedure TestMimePartHeaders_RawHeaderText_Roundtrip;
    [Test]
    procedure TestMimePartHeaders_KnownHeaders_ByProperty;
    [Test]
    procedure TestMimePartHeaders_UnknownHeaders;
    [Test]
    procedure TestMimePartHeaders_ContentTypeCharset;
    [Test]
    procedure TestMimePartHeaders_ContentDispositionNameAndFilename;

    [Test]
    procedure TestDecoder_ParseOnePart;
    [Test]
    procedure TestDecoder_ParseTwoParts;
    [Test]
    procedure TestDecoder_HeadersAndBodyExtraction;
    [Test]
    procedure TestDecoder_Errors_MissingBoundaryInContentType;
    [Test]
    procedure TestDecoder_Errors_BoundaryNotFoundInBody;
    [Test]
    procedure TestDecoder_Errors_BodyMustStartWithBoundary;
    [Test]
    procedure TestDecoder_Errors_InvalidBoundaryLineEnding;
    [Test]
    procedure TestDecoder_Errors_MissingHeadersTerminator;
    [Test]
    procedure TestDecoder_Errors_MissingClosingBoundary;
    [Test]
    procedure TestDecoder_Errors_BoundaryMustBePrecededByCRLF;
    [Test]
    procedure TestDecoder_Errors_UnexpectedEndAfterBoundary;
  end;

implementation

uses
  System.SysUtils,
  System.AnsiStrings,
  System.IOUtils,
  Alcinoe.Common,
  Alcinoe.StringUtils,
  Alcinoe.Mime.Multipart;

type

  {**********************************************************}
  // Small helper to produce deterministic boundaries in tests
  TALTestMultipartFormDataEncoderA = class(TALMultipartFormDataEncoderA)
  protected
    function GenerateBoundary: AnsiString; override;
  end;

  {**************************************************************************}
  TALTestMultipartAlternativeEncoderA = class(TALMultipartAlternativeEncoderA)
  protected
    function GenerateBoundary: AnsiString; override;
  end;

  {**************************************************************}
  TALTestMultipartMixedEncoderA = class(TALMultipartMixedEncoderA)
  protected
    function GenerateBoundary: AnsiString; override;
  end;

{*********************************************************************}
function TALTestMultipartFormDataEncoderA.GenerateBoundary: AnsiString;
begin
  Result := 'ALCINOE_TEST_BOUNDARY_FORMDATA';
end;

{************************************************************************}
function TALTestMultipartAlternativeEncoderA.GenerateBoundary: AnsiString;
begin
  Result := 'ALCINOE_TEST_BOUNDARY_ALTERNATIVE';
end;

{******************************************************************}
function TALTestMultipartMixedEncoderA.GenerateBoundary: AnsiString;
begin
  Result := 'ALCINOE_TEST_BOUNDARY_MIXED';
end;

{****************************************************************************}
procedure _AssertContainsA(const ASub, AText: AnsiString; const AMsg: String);
begin
  Assert.IsTrue(ALPosA(ASub, AText) > 0, AMsg);
end;

{*********************************************************************************}
procedure _AssertStartsWithA(const APrefix, AText: AnsiString; const AMsg: String);
begin
  Assert.IsTrue(ALPosA(APrefix, AText) = Low(AText), AMsg);
end;

{*******************************************************************************}
procedure _AssertEndsWithA(const ASuffix, AText: AnsiString; const AMsg: String);
begin
  Assert.IsTrue(System.AnsiStrings.EndsStr(ASuffix, AText), AMsg);
end;

{******************************************************************************}
procedure TALDUnitXTestMimeMultipart.TestBaseEncoder_BoundaryAndMimeTypeHeaders;
var
  E1: TALTestMultipartFormDataEncoderA;
  E2: TALTestMultipartAlternativeEncoderA;
  E3: TALTestMultipartMixedEncoderA;
begin
  E1 := TALTestMultipartFormDataEncoderA.Create(True);
  try
    Assert.AreEqual(AnsiString('ALCINOE_TEST_BOUNDARY_FORMDATA'), E1.Boundary, 'FormData boundary');
    Assert.AreEqual(AnsiString('multipart/form-data; boundary=ALCINOE_TEST_BOUNDARY_FORMDATA'), E1.MimeTypeHeader, 'FormData MimeTypeHeader');
  finally
    ALFreeAndNil(E1);
  end;

  E2 := TALTestMultipartAlternativeEncoderA.Create(True);
  try
    Assert.AreEqual(AnsiString('ALCINOE_TEST_BOUNDARY_ALTERNATIVE'), E2.Boundary, 'Alternative boundary');
    Assert.AreEqual(AnsiString('multipart/alternative; boundary=ALCINOE_TEST_BOUNDARY_ALTERNATIVE'), E2.MimeTypeHeader, 'Alternative MimeTypeHeader');
  finally
    ALFreeAndNil(E2);
  end;

  E3 := TALTestMultipartMixedEncoderA.Create(True);
  try
    Assert.AreEqual(AnsiString('ALCINOE_TEST_BOUNDARY_MIXED'), E3.Boundary, 'Mixed boundary');
    Assert.AreEqual(AnsiString('multipart/mixed; boundary=ALCINOE_TEST_BOUNDARY_MIXED'), E3.MimeTypeHeader, 'Mixed MimeTypeHeader');
  finally
    ALFreeAndNil(E3);
  end;
end;

{*********************************************************************}
procedure TALDUnitXTestMimeMultipart.TestFormDataEncoder_AddText_Basic;
var
  Enc: TALTestMultipartFormDataEncoderA;
  Payload: AnsiString;
begin
  Enc := TALTestMultipartFormDataEncoderA.Create(True);
  try
    Enc.AddText('field1', 'avalue');
    Payload := Enc.PayloadString;

    _AssertStartsWithA('--ALCINOE_TEST_BOUNDARY_FORMDATA'#13#10, Payload, 'Payload starts with boundary');
    _AssertContainsA('Content-Disposition: form-data; name="field1"'#13#10, Payload, 'Has Content-Disposition');
    _AssertContainsA('Content-Length: 6'#13#10, Payload, 'Has Content-Length');
    _AssertContainsA(#13#10'avalue'#13#10, Payload, 'Has body');
  finally
    ALFreeAndNil(Enc);
  end;
end;

{**********************************************************************************************}
procedure TALDUnitXTestMimeMultipart.TestFormDataEncoder_AddText_WithContentTypeAndExtraHeaders;
var
  Enc: TALTestMultipartFormDataEncoderA;
  Payload: AnsiString;
  Headers: TALNameValueArrayA;
begin
  Enc := TALTestMultipartFormDataEncoderA.Create(True);
  try
    SetLength(Headers, 2);
    Headers[0] := TALNameValuePairA.Create('X-Test', '1');
    Headers[1] := TALNameValuePairA.Create('X-Empty', '');

    Enc.AddText('field1', 'abc', 'text/plain; charset=utf-8', Headers);
    Payload := Enc.PayloadString;

    _AssertContainsA('Content-Type: text/plain; charset=utf-8'#13#10, Payload, 'Has Content-Type');
    _AssertContainsA('Content-Length: 3'#13#10, Payload, 'Has Content-Length');
    _AssertContainsA('X-Test: 1'#13#10, Payload, 'Has extra header');
    Assert.IsFalse(ALPosA('X-Empty:', Payload) > 0, 'Must not emit empty-value header');
  finally
    ALFreeAndNil(Enc);
  end;
end;

{************************************************************************************************}
procedure TALDUnitXTestMimeMultipart.TestFormDataEncoder_AddStream_WithFileNameAndDefaultMimeType;
var
  Enc: TALTestMultipartFormDataEncoderA;
  S: TALStringStreamA;
  Payload: AnsiString;
begin
  Enc := TALTestMultipartFormDataEncoderA.Create(True);
  try
    S := TALStringStreamA.Create('HELLO');
    Enc.AddStream('file', S, True{owns}, 'a.txt', ''{ContentType});
    Payload := Enc.PayloadString;

    _AssertContainsA('Content-Disposition: form-data; name="file"; filename="a.txt"'#13#10, Payload, 'Has filename');
    _AssertContainsA('Content-Length: 5'#13#10, Payload, 'Has content length');
    // Content-Type default depends on ALGetDefaultMIMEContentTypeFromExt, so just ensure header exists
    _AssertContainsA('Content-Type: ', Payload, 'Has Content-Type');
    _AssertContainsA(#13#10'HELLO'#13#10, Payload, 'Stream bytes present');
  finally
    ALFreeAndNil(Enc);
  end;
end;

{****************************************************************}
procedure TALDUnitXTestMimeMultipart.TestFormDataEncoder_AddBytes;
var
  Enc: TALTestMultipartFormDataEncoderA;
  Bytes: TBytes;
  Payload: AnsiString;
begin
  Enc := TALTestMultipartFormDataEncoderA.Create(True);
  try
    Bytes := TBytes.Create($01, $02, $03, $04);
    Enc.AddBytes('bin', Bytes, 'x.bin', 'application/octet-stream');
    Payload := Enc.PayloadString;

    _AssertContainsA('Content-Disposition: form-data; name="bin"; filename="x.bin"'#13#10, Payload, 'Has filename');
    _AssertContainsA('Content-Type: application/octet-stream'#13#10, Payload, 'Has Content-Type');
    _AssertContainsA('Content-Length: 4'#13#10, Payload, 'Has Content-Length');
    // Body is raw bytes; easiest: ensure correct CRLF framing and that payload length is > headers
    _AssertContainsA(#13#10#13#10, Payload, 'Has header/body separator');
  finally
    ALFreeAndNil(Enc);
  end;
end;

{**************************************************************************************}
procedure TALDUnitXTestMimeMultipart.TestFormDataEncoder_PayloadEndsWithClosingBoundary;
var
  Enc: TALTestMultipartFormDataEncoderA;
  Payload: AnsiString;
begin
  Enc := TALTestMultipartFormDataEncoderA.Create(True);
  try
    Enc.AddText('f', 'v');
    Payload := Enc.PayloadString;
    _AssertEndsWithA('--ALCINOE_TEST_BOUNDARY_FORMDATA--'#13#10, Payload, 'Ends with closing boundary');
  finally
    ALFreeAndNil(Enc);
  end;
end;

{********************************************************************************}
procedure TALDUnitXTestMimeMultipart.TestAlternativeEncoder_AddText_Base64Default;
var
  Enc: TALTestMultipartAlternativeEncoderA;
  Payload: AnsiString;
begin
  Enc := TALTestMultipartAlternativeEncoderA.Create(True);
  try
    Enc.AddText('hello', 'text/plain; charset=utf-8');
    Payload := Enc.PayloadString;

    _AssertContainsA('--ALCINOE_TEST_BOUNDARY_ALTERNATIVE'#13#10, Payload, 'Has boundary');
    _AssertContainsA('Content-Type: text/plain; charset=utf-8'#13#10, Payload, 'Has Content-Type');
    _AssertContainsA('Content-Transfer-Encoding: base64'#13#10, Payload, 'Defaults to base64 encoding');
    // base64 of "hello" is "aGVsbG8=" (MIME encoder may include line breaks for long strings, not here)
    _AssertContainsA(#13#10'aGVsbG8='#13#10, Payload, 'Has base64 body');
    _AssertEndsWithA('--ALCINOE_TEST_BOUNDARY_ALTERNATIVE--'#13#10, Payload, 'Closing boundary');
  finally
    ALFreeAndNil(Enc);
  end;
end;

{*************************************************************************************************}
procedure TALDUnitXTestMimeMultipart.TestAlternativeEncoder_AddText_OverrideTransferEncodingHeader;
var
  Enc: TALTestMultipartAlternativeEncoderA;
  Headers: TALNameValueArrayA;
  Payload: AnsiString;
begin
  Enc := TALTestMultipartAlternativeEncoderA.Create(True);
  try
    SetLength(Headers, 1);
    Headers[0] := TALNameValuePairA.Create('Content-Transfer-Encoding', '7bit');

    Enc.AddText('hello', 'text/plain; charset=utf-8', Headers);
    Payload := Enc.PayloadString;

    _AssertContainsA('Content-Transfer-Encoding: 7bit'#13#10, Payload, 'Uses overridden transfer encoding');
    Assert.IsFalse(ALPosA('Content-Transfer-Encoding: base64'#13#10, Payload) > 0, 'Must not inject base64 header when overridden');
    _AssertContainsA(#13#10'hello'#13#10, Payload, 'Must keep raw body when overridden');
  finally
    ALFreeAndNil(Enc);
  end;
end;

{**************************************************************************}
procedure TALDUnitXTestMimeMultipart.TestMixedEncoder_AddText_Base64Default;
var
  Enc: TALTestMultipartMixedEncoderA;
  Payload: AnsiString;
begin
  Enc := TALTestMultipartMixedEncoderA.Create(True);
  try
    Enc.AddText('hello', 'text/plain; charset=utf-8');
    Payload := Enc.PayloadString;

    _AssertContainsA('--ALCINOE_TEST_BOUNDARY_MIXED'#13#10, Payload, 'Has boundary');
    _AssertContainsA('Content-Type: text/plain; charset=utf-8'#13#10, Payload, 'Has Content-Type');
    _AssertContainsA('Content-Transfer-Encoding: base64'#13#10, Payload, 'Defaults to base64');
    _AssertContainsA(#13#10'aGVsbG8='#13#10, Payload, 'Has base64 body');
  finally
    ALFreeAndNil(Enc);
  end;
end;

{*************************************************************************************************}
procedure TALDUnitXTestMimeMultipart.TestMixedEncoder_AddStream_AddsAttachmentDispositionIfMissing;
var
  Enc: TALTestMultipartMixedEncoderA;
  S: TALStringStreamA;
  Payload: AnsiString;
begin
  Enc := TALTestMultipartMixedEncoderA.Create(True);
  try
    S := TALStringStreamA.Create('BIN');
    try
      Enc.AddStream(S, 'file.bin', 'application/octet-stream', nil);
    finally
      // AddStream does NOT take ownership here; we own it in the test
      AlFreeAndNil(S);
    end;

    Payload := Enc.PayloadString;

    _AssertContainsA('Content-Type: application/octet-stream'#13#10, Payload, 'Has Content-Type');
    _AssertContainsA('Content-Disposition: attachment; filename="file.bin"'#13#10, Payload, 'Injects attachment disposition');
    _AssertContainsA('Content-Transfer-Encoding: base64'#13#10, Payload, 'Base64 encoding emitted');
    // "BIN" => "QklO"
    _AssertContainsA(#13#10'QklO'#13#10, Payload, 'Has base64 body');
  finally
    ALFreeAndNil(Enc);
  end;
end;

{************************************************************}
procedure TALDUnitXTestMimeMultipart.TestMixedEncoder_AddFile;
var
  Enc: TALTestMultipartMixedEncoderA;
  TempFile: String;
  Payload: AnsiString;
begin
  Enc := TALTestMultipartMixedEncoderA.Create(True);
  try
    TempFile := IncludeTrailingPathDelimiter(GetEnvironmentVariable('TEMP')) + 'al_multipart_test_file.txt';
    TFile.WriteAllText(TempFile, 'OK', TEncoding.ASCII);
    try
      Enc.AddFile(TempFile, ''{ContentType});
      Payload := Enc.PayloadString;

      _AssertContainsA('Content-Disposition: attachment; filename="al_multipart_test_file.txt"'#13#10, Payload, 'Has attachment filename');
      _AssertContainsA('Content-Transfer-Encoding: base64'#13#10, Payload, 'Base64 encoding');
      // 'OK' => 'T0s='
      _AssertContainsA(#13#10'T0s='#13#10, Payload, 'Has base64 body');
    finally
      if TFile.Exists(TempFile) then
        TFile.Delete(TempFile);
    end;
  finally
    ALFreeAndNil(Enc);
  end;
end;

{*************************************************************}
procedure TALDUnitXTestMimeMultipart.TestMixedEncoder_AddBytes;
var
  Enc: TALTestMultipartMixedEncoderA;
  Bytes: TBytes;
  Payload: AnsiString;
begin
  Enc := TALTestMultipartMixedEncoderA.Create(True);
  try
    Bytes := TBytes.Create(Ord('A'), Ord('B')); // "AB" => "QUI="
    Enc.AddBytes(Bytes, 'ab.bin', 'application/octet-stream', nil);
    Payload := Enc.PayloadString;

    _AssertContainsA('Content-Disposition: attachment; filename="ab.bin"'#13#10, Payload, 'Has attachment disposition');
    _AssertContainsA('Content-Transfer-Encoding: base64'#13#10, Payload, 'Base64 encoding');
    _AssertContainsA(#13#10'QUI='#13#10, Payload, 'Body is base64 of bytes');
  finally
    ALFreeAndNil(Enc);
  end;
end;

{*******************************************************************************}
procedure TALDUnitXTestMimeMultipart.TestMimePartHeaders_RawHeaderText_Roundtrip;
var
  H: TALMimePartHeadersA;
  Raw: AnsiString;
begin
  H := TALMimePartHeadersA.Create;
  try
    H.ContentType := 'text/plain; charset=utf-8';
    H.ContentDisposition := 'form-data; name="field"; filename="a.txt"';
    H.ContentTransferEncoding := '7bit';
    H.ContentLength := '123';
    H.ContentDescription := 'desc';
    H.ContentID := '<id@x>';
    H['X-Test'] := '1';

    Raw := H.RawHeaderText;

    // now parse back
    H.Clear;
    H.RawHeaderText := Raw;

    Assert.AreEqual(AnsiString('text/plain; charset=utf-8'), H.ContentType, 'Roundtrip Content-Type');
    Assert.AreEqual(AnsiString('form-data; name="field"; filename="a.txt"'), H.ContentDisposition, 'Roundtrip Content-Disposition');
    Assert.AreEqual(AnsiString('7bit'), H.ContentTransferEncoding, 'Roundtrip CTE');
    Assert.AreEqual(AnsiString('123'), H.ContentLength, 'Roundtrip length');
    Assert.AreEqual(AnsiString('desc'), H.ContentDescription, 'Roundtrip description');
    Assert.AreEqual(AnsiString('<id@x>'), H.ContentID, 'Roundtrip ID');
    Assert.AreEqual(AnsiString('1'), H['X-Test'], 'Roundtrip unknown header');
  finally
    ALFreeAndNil(H);
  end;
end;

{*******************************************************************************}
procedure TALDUnitXTestMimeMultipart.TestMimePartHeaders_KnownHeaders_ByProperty;
var
  H: TALMimePartHeadersA;
begin
  H := TALMimePartHeadersA.Create;
  try
    H.ContentType := 'a';
    H.ContentDisposition := 'b';
    H.ContentTransferEncoding := 'c';
    H.ContentLength := 'd';
    H.ContentDescription := 'e';
    H.ContentID := 'f';

    Assert.AreEqual(AnsiString('a'), H.ContentType);
    Assert.AreEqual(AnsiString('b'), H.ContentDisposition);
    Assert.AreEqual(AnsiString('c'), H.ContentTransferEncoding);
    Assert.AreEqual(AnsiString('d'), H.ContentLength);
    Assert.AreEqual(AnsiString('e'), H.ContentDescription);
    Assert.AreEqual(AnsiString('f'), H.ContentID);
  finally
    ALFreeAndNil(H);
  end;
end;

{**********************************************************************}
procedure TALDUnitXTestMimeMultipart.TestMimePartHeaders_UnknownHeaders;
var
  H: TALMimePartHeadersA;
begin
  H := TALMimePartHeadersA.Create;
  try
    H['X-A'] := '1';
    H['X-B'] := '2';
    Assert.AreEqual(AnsiString('1'), H['X-A']);
    Assert.AreEqual(AnsiString('2'), H['X-B']);
    Assert.IsTrue(H.UnknownHeaders.Count >= 2, 'UnknownHeaders stores values');
  finally
    ALFreeAndNil(H);
  end;
end;

{**************************************************************************}
procedure TALDUnitXTestMimeMultipart.TestMimePartHeaders_ContentTypeCharset;
var
  H: TALMimePartHeadersA;
begin
  H := TALMimePartHeadersA.Create;
  try
    H.ContentType := 'text/html; charset=ISO-8859-4';
    Assert.AreEqual(AnsiString('ISO-8859-4'), H.ContentCharset);
  finally
    ALFreeAndNil(H);
  end;
end;

{*****************************************************************************************}
procedure TALDUnitXTestMimeMultipart.TestMimePartHeaders_ContentDispositionNameAndFilename;
var
  H: TALMimePartHeadersA;
begin
  H := TALMimePartHeadersA.Create;
  try
    H.ContentDisposition := 'form-data; name="field"; filename="file.jpg"';
    Assert.AreEqual(AnsiString('field'), H.ContentDispositionName);
    Assert.AreEqual(AnsiString('file.jpg'), H.ContentDispositionFilename);
  finally
    ALFreeAndNil(H);
  end;
end;

{************************************************************}
procedure TALDUnitXTestMimeMultipart.TestDecoder_ParseOnePart;
var
  Body: AnsiString;
  Stream: TALStringStreamA;
  Dec: TALMultipartDecoderA;
begin
  Body :=
    '--b'#13#10 +
    'Content-Type: text/plain'#13#10 +
    #13#10 +
    'hello'#13#10 +
    '--b--'#13#10;

  Stream := TALStringStreamA.Create(Body);
  try
    Dec := TALMultipartDecoderA.Create('multipart/form-data; boundary=b', Stream);
    try
      Assert.AreEqual(Nativeint(1), Dec.Parts.Count, 'One part parsed');
      Assert.AreEqual(AnsiString('text/plain'), Dec.Parts[0].Headers.ContentType, 'Header parsed');
      Assert.AreEqual(AnsiString('hello'), Dec.Parts[0].BodyString, 'Body parsed');
    finally
      ALFreeAndNil(Dec);
    end;
  finally
    ALFreeAndNil(Stream);
  end;
end;

{*************************************************************}
procedure TALDUnitXTestMimeMultipart.TestDecoder_ParseTwoParts;
var
  Body: AnsiString;
  Stream: TALStringStreamA;
  Dec: TALMultipartDecoderA;
begin
  Body :=
    '--b'#13#10 +
    'Content-Disposition: form-data; name="f1"'#13#10 +
    #13#10 +
    'v1'#13#10 +
    '--b'#13#10 +
    'Content-Disposition: form-data; name="f2"'#13#10 +
    #13#10 +
    'v2'#13#10 +
    '--b--'#13#10;

  Stream := TALStringStreamA.Create(Body);
  try
    Dec := TALMultipartDecoderA.Create('multipart/form-data; boundary=b', Stream);
    try
      Assert.AreEqual(NativeInt(2), Dec.Parts.Count, 'Two parts parsed');
      Assert.AreEqual(AnsiString('f1'), Dec.Parts[0].Headers.ContentDispositionName, 'Part1 name');
      Assert.AreEqual(AnsiString('v1'), Dec.Parts[0].BodyString, 'Part1 body');
      Assert.AreEqual(AnsiString('f2'), Dec.Parts[1].Headers.ContentDispositionName, 'Part2 name');
      Assert.AreEqual(AnsiString('v2'), Dec.Parts[1].BodyString, 'Part2 body');
    finally
      ALFreeAndNil(Dec);
    end;
  finally
    ALFreeAndNil(Stream);
  end;
end;

{************************************************************************}
procedure TALDUnitXTestMimeMultipart.TestDecoder_HeadersAndBodyExtraction;
var
  Body: AnsiString;
  Stream: TALStringStreamA;
  Dec: TALMultipartDecoderA;
begin
  Body :=
    '--b'#13#10 +
    'Content-Type: text/plain; charset=utf-8'#13#10 +
    'X-Test: 1'#13#10 +
    #13#10 +
    'HELLO WORLD'#13#10 +
    '--b--'#13#10;

  Stream := TALStringStreamA.Create(Body);
  try
    Dec := TALMultipartDecoderA.Create('multipart/form-data; boundary=b', Stream);
    try
      Assert.AreEqual(NativeInt(1), Dec.Parts.Count);
      Assert.AreEqual(AnsiString('text/plain; charset=utf-8'), Dec.Parts[0].Headers.ContentType);
      Assert.AreEqual(AnsiString('utf-8'), Dec.Parts[0].Headers.ContentCharset);
      Assert.AreEqual(AnsiString('1'), Dec.Parts[0].Headers['X-Test']);
      Assert.AreEqual(AnsiString('HELLO WORLD'), Dec.Parts[0].BodyString);
      Assert.IsTrue(Dec.Parts[0].BodyStream.Size = Length(Dec.Parts[0].BodyString), 'BodyStream consistent');
    finally
      ALFreeAndNil(Dec);
    end;
  finally
    ALFreeAndNil(Stream);
  end;
end;

{***********************************************************************************}
procedure TALDUnitXTestMimeMultipart.TestDecoder_Errors_MissingBoundaryInContentType;
var
  Stream: TALStringStreamA;
begin
  Stream := TALStringStreamA.Create('--b--'#13#10);
  try
    Assert.WillRaise(
      procedure
      var Dec: TALMultipartDecoderA;
      begin
        Dec := TALMultipartDecoderA.Create('multipart/form-data', Stream);
        ALFreeAndNil(Dec);
      end,
      EArgumentException,
      'Should raise when boundary is missing in Content-Type');
  finally
    ALFreeAndNil(Stream);
  end;
end;

{*****************************************************************************}
procedure TALDUnitXTestMimeMultipart.TestDecoder_Errors_BoundaryNotFoundInBody;
var
  Stream: TALStringStreamA;
begin
  Stream := TALStringStreamA.Create('no boundary here');
  try
    Assert.WillRaise(
      procedure
      var Dec: TALMultipartDecoderA;
      begin
        Dec := TALMultipartDecoderA.Create('multipart/form-data; boundary=b', Stream);
        ALFreeAndNil(Dec);
      end,
      Exception,
      'Should raise when boundary not found in body');
  finally
    ALFreeAndNil(Stream);
  end;
end;

{********************************************************************************}
procedure TALDUnitXTestMimeMultipart.TestDecoder_Errors_BodyMustStartWithBoundary;
var
  Stream: TALStringStreamA;
  Body: AnsiString;
begin
  Body :=
    'xx' +
    '--b'#13#10 +
    'Content-Type: text/plain'#13#10 +
    #13#10 +
    'hello'#13#10 +
    '--b--'#13#10;

  Stream := TALStringStreamA.Create(Body);
  try
    Assert.WillRaise(
      procedure
      var Dec: TALMultipartDecoderA;
      begin
        Dec := TALMultipartDecoderA.Create('multipart/form-data; boundary=b', Stream);
        ALFreeAndNil(Dec);
      end,
      Exception,
      'Should raise when multipart body does not start with boundary');
  finally
    ALFreeAndNil(Stream);
  end;
end;

{********************************************************************************}
procedure TALDUnitXTestMimeMultipart.TestDecoder_Errors_InvalidBoundaryLineEnding;
var
  Stream: TALStringStreamA;
  Body: AnsiString;
begin
  // boundary must be followed by CRLF or "--"
  Body :=
    '--b' + 'X' + // invalid instead of CRLF
    'Content-Type: text/plain'#13#10 +
    #13#10 +
    'hello'#13#10 +
    '--b--'#13#10;

  Stream := TALStringStreamA.Create(Body);
  try
    Assert.WillRaise(
      procedure
      var Dec: TALMultipartDecoderA;
      begin
        Dec := TALMultipartDecoderA.Create('multipart/form-data; boundary=b', Stream);
        ALFreeAndNil(Dec);
      end,
      Exception,
      'Should raise on invalid boundary line ending');
  finally
    ALFreeAndNil(Stream);
  end;
end;

{*******************************************************************************}
procedure TALDUnitXTestMimeMultipart.TestDecoder_Errors_MissingHeadersTerminator;
var
  Stream: TALStringStreamA;
  Body: AnsiString;
begin
  // Missing CRLFCRLF between headers and body
  Body :=
    '--b'#13#10 +
    'Content-Type: text/plain'#13#10 +
    'X: 1'#13#10 +
    'hello'#13#10 + // wrong: no empty line
    '--b--'#13#10;

  Stream := TALStringStreamA.Create(Body);
  try
    Assert.WillRaise(
      procedure
      var Dec: TALMultipartDecoderA;
      begin
        Dec := TALMultipartDecoderA.Create('multipart/form-data; boundary=b', Stream);
        ALFreeAndNil(Dec);
      end,
      Exception,
      'Should raise when headers terminator is missing');
  finally
    ALFreeAndNil(Stream);
  end;
end;

{*****************************************************************************}
procedure TALDUnitXTestMimeMultipart.TestDecoder_Errors_MissingClosingBoundary;
var
  Stream: TALStringStreamA;
  Body: AnsiString;
begin
  Body :=
    '--b'#13#10 +
    'Content-Type: text/plain'#13#10 +
    #13#10 +
    'hello'#13#10; // no next boundary

  Stream := TALStringStreamA.Create(Body);
  try
    Assert.WillRaise(
      procedure
      var Dec: TALMultipartDecoderA;
      begin
        Dec := TALMultipartDecoderA.Create('multipart/form-data; boundary=b', Stream);
        ALFreeAndNil(Dec);
      end,
      Exception,
      'Should raise when closing boundary is missing');
  finally
    ALFreeAndNil(Stream);
  end;
end;

{***********************************************************************************}
procedure TALDUnitXTestMimeMultipart.TestDecoder_Errors_BoundaryMustBePrecededByCRLF;
var
  Stream: TALStringStreamA;
  Body: AnsiString;
begin
  // Next boundary must be preceded by CRLF; here we place it immediately after body without CRLF
  Body :=
    '--b'#13#10 +
    'Content-Type: text/plain'#13#10 +
    #13#10 +
    'hello' + // NO CRLF
    '--b--'#13#10;

  Stream := TALStringStreamA.Create(Body);
  try
    Assert.WillRaise(
      procedure
      var Dec: TALMultipartDecoderA;
      begin
        Dec := TALMultipartDecoderA.Create('multipart/form-data; boundary=b', Stream);
        ALFreeAndNil(Dec);
      end,
      Exception,
      'Should raise when boundary is not preceded by CRLF');
  finally
    ALFreeAndNil(Stream);
  end;
end;

{*********************************************************************************}
procedure TALDUnitXTestMimeMultipart.TestDecoder_Errors_UnexpectedEndAfterBoundary;
var
  Stream: TALStringStreamA;
  Body: AnsiString;
begin
  // boundary then ends immediately, so decoder can't read either "--" or CRLF
  Body := '--b';

  Stream := TALStringStreamA.Create(Body);
  try
    Assert.WillRaise(
      procedure
      var Dec: TALMultipartDecoderA;
      begin
        Dec := TALMultipartDecoderA.Create('multipart/form-data; boundary=b', Stream);
        ALFreeAndNil(Dec);
      end,
      Exception,
      'Should raise on unexpected end after boundary');
  finally
    ALFreeAndNil(Stream);
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TALDUnitXTestMimeMultipart);

end.