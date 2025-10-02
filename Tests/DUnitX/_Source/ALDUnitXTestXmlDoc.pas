unit ALDUnitXTestXmlDoc;

interface

uses
  DUnitX.TestFramework;

type

  [TestFixture]
  TALDUnitXTestHtml = class
  public
    //constructor create;
    //[Setup]
    //procedure Setup;
    //[TearDown]
    //procedure TearDown;
    [Test]
    procedure TestALXMLAttributeEncodeA;
    [Test]
    procedure TestALXMLAttributeEncodeW;
    [Test]
    procedure TestALXMLTextEncodeA;
    [Test]
    procedure TestALXMLTextEncodeW;
    [Test]
    procedure TestALXMLEntityDecodeInPlaceA;
    [Test]
    procedure TestALXMLEntityDecodeInPlaceW;
  end;

implementation

uses
  System.Character,
  Alcinoe.XmlDoc;

{****************************************************}
procedure TALDUnitXTestHTML.TestALXMLAttributeEncodeA;
begin
  // Empty string
  if ALXMLAttributeEncode(AnsiString('')) <> '' then Assert.Fail('empty string');

  // No special chars
  if ALXMLAttributeEncode(AnsiString('HelloWorld')) <> 'HelloWorld' then Assert.Fail('plain text');

  // Ampersand
  if ALXMLAttributeEncode(AnsiString('&'), True) <> '&#38;' then Assert.Fail('amp numeric');
  if ALXMLAttributeEncode(AnsiString('&'), False) <> '&amp;' then Assert.Fail('amp entity');

  // Less than
  if ALXMLAttributeEncode(AnsiString('<'), True) <> '&#60;' then Assert.Fail('lt numeric');
  if ALXMLAttributeEncode(AnsiString('<'), False) <> '&lt;' then Assert.Fail('lt entity');

  // Greater than
  if ALXMLAttributeEncode(AnsiString('>'), True) <> '&#62;' then Assert.Fail('gt numeric');
  if ALXMLAttributeEncode(AnsiString('>'), False) <> '&gt;' then Assert.Fail('gt entity');

  // Double quote
  if ALXMLAttributeEncode(AnsiString('"'), True) <> '&#34;' then Assert.Fail('quote numeric');
  if ALXMLAttributeEncode(AnsiString('"'), False) <> '&quot;' then Assert.Fail('quote entity');

  // Apostrophe
  if ALXMLAttributeEncode(AnsiString(''''), True) <> '&#39;' then Assert.Fail('apos numeric');
  if ALXMLAttributeEncode(AnsiString(''''), False) <> '&apos;' then Assert.Fail('apos entity');

  // Mixed input
  if ALXMLAttributeEncode(AnsiString('Hello&World<Test>'), True) <> 'Hello&#38;World&#60;Test&#62;' then Assert.Fail('mixed numeric');
  if ALXMLAttributeEncode(AnsiString('Hello&World<Test>'), False) <> 'Hello&amp;World&lt;Test&gt;' then Assert.Fail('mixed entity');

  // Combination with quotes
  if ALXMLAttributeEncode(AnsiString('Say"Hi"''Now'''), True) <> 'Say&#34;Hi&#34;&#39;Now&#39;' then Assert.Fail('quotes numeric');
  if ALXMLAttributeEncode(AnsiString('Say"Hi"''Now'''), False) <> 'Say&quot;Hi&quot;&apos;Now&apos;' then Assert.Fail('quotes entity');

  // Long text with multiple replacements
  if ALXMLAttributeEncode(AnsiString('Test&Check<XML>"Attr"''Value'''), True) <> 'Test&#38;Check&#60;XML&#62;&#34;Attr&#34;&#39;Value&#39;' then Assert.Fail('complex numeric');
  if ALXMLAttributeEncode(AnsiString('Test&Check<XML>"Attr"''Value'''), False) <> 'Test&amp;Check&lt;XML&gt;&quot;Attr&quot;&apos;Value&apos;' then Assert.Fail('complex entity');

  // Unicode and normal ASCII should pass through unchanged
  if ALXMLAttributeEncode(AnsiString('Café')) <> 'Café' then Assert.Fail('unicode passthrough');
  if ALXMLAttributeEncode(AnsiString('Привет')) <> 'Привет' then Assert.Fail('cyrillic passthrough');
end;

{****************************************************}
procedure TALDUnitXTestHTML.TestALXMLAttributeEncodeW;
begin
  // Empty string
  if ALXMLAttributeEncode(String('')) <> '' then Assert.Fail('empty string');

  // No special chars
  if ALXMLAttributeEncode(String('HelloWorld')) <> 'HelloWorld' then Assert.Fail('plain text');

  // Ampersand
  if ALXMLAttributeEncode(String('&'), True) <> '&#38;' then Assert.Fail('amp numeric');
  if ALXMLAttributeEncode(String('&'), False) <> '&amp;' then Assert.Fail('amp entity');

  // Less than
  if ALXMLAttributeEncode(String('<'), True) <> '&#60;' then Assert.Fail('lt numeric');
  if ALXMLAttributeEncode(String('<'), False) <> '&lt;' then Assert.Fail('lt entity');

  // Greater than
  if ALXMLAttributeEncode(String('>'), True) <> '&#62;' then Assert.Fail('gt numeric');
  if ALXMLAttributeEncode(String('>'), False) <> '&gt;' then Assert.Fail('gt entity');

  // Double quote
  if ALXMLAttributeEncode(String('"'), True) <> '&#34;' then Assert.Fail('quote numeric');
  if ALXMLAttributeEncode(String('"'), False) <> '&quot;' then Assert.Fail('quote entity');

  // Apostrophe
  if ALXMLAttributeEncode(String(''''), True) <> '&#39;' then Assert.Fail('apos numeric');
  if ALXMLAttributeEncode(String(''''), False) <> '&apos;' then Assert.Fail('apos entity');

  // Mixed input
  if ALXMLAttributeEncode(String('Hello&World<Test>'), True) <> 'Hello&#38;World&#60;Test&#62;' then Assert.Fail('mixed numeric');
  if ALXMLAttributeEncode(String('Hello&World<Test>'), False) <> 'Hello&amp;World&lt;Test&gt;' then Assert.Fail('mixed entity');

  // Combination with quotes
  if ALXMLAttributeEncode(String('Say"Hi"''Now'''), True) <> 'Say&#34;Hi&#34;&#39;Now&#39;' then Assert.Fail('quotes numeric');
  if ALXMLAttributeEncode(String('Say"Hi"''Now'''), False) <> 'Say&quot;Hi&quot;&apos;Now&apos;' then Assert.Fail('quotes entity');

  // Long text with multiple replacements
  if ALXMLAttributeEncode(String('Test&Check<XML>"Attr"''Value'''), True) <> 'Test&#38;Check&#60;XML&#62;&#34;Attr&#34;&#39;Value&#39;' then Assert.Fail('complex numeric');
  if ALXMLAttributeEncode(String('Test&Check<XML>"Attr"''Value'''), False) <> 'Test&amp;Check&lt;XML&gt;&quot;Attr&quot;&apos;Value&apos;' then Assert.Fail('complex entity');

  // Unicode and normal ASCII should pass through unchanged
  if ALXMLAttributeEncode(String('Café')) <> 'Café' then Assert.Fail('unicode passthrough');
  if ALXMLAttributeEncode(String('Привет')) <> 'Привет' then Assert.Fail('cyrillic passthrough');
end;

{***********************************************}
procedure TALDUnitXTestHTML.TestALXMLTextEncodeA;
begin
  // Empty
  if ALXMLTextEncode(AnsiString('')) <> '' then Assert.Fail('empty');

  // Plain (no specials)
  if ALXMLTextEncode(AnsiString('HelloWorld')) <> 'HelloWorld' then Assert.Fail('plain');

  // Single specials - numeric
  if ALXMLTextEncode(AnsiString('&'), True) <> '&#38;' then Assert.Fail('amp numeric');
  if ALXMLTextEncode(AnsiString('<'), True) <> '&#60;' then Assert.Fail('lt numeric');
  if ALXMLTextEncode(AnsiString('>'), True) <> '&#62;' then Assert.Fail('gt numeric');

  // Single specials - named
  if ALXMLTextEncode(AnsiString('&'), False) <> '&amp;' then Assert.Fail('amp entity');
  if ALXMLTextEncode(AnsiString('<'), False) <> '&lt;' then Assert.Fail('lt entity');
  if ALXMLTextEncode(AnsiString('>'), False) <> '&gt;' then Assert.Fail('gt entity');

  // Mixed specials - numeric
  if ALXMLTextEncode(AnsiString('A&B<C>D'), True) <> 'A&#38;B&#60;C&#62;D' then Assert.Fail('mixed numeric');

  // Mixed specials - named
  if ALXMLTextEncode(AnsiString('A&B<C>D'), False) <> 'A&amp;B&lt;C&gt;D' then Assert.Fail('mixed entity');

  // Already-encoded input gets re-escaped (by design of this proc)
  if ALXMLTextEncode(AnsiString('&amp;<tag>&gt;'), True) <> '&#38;amp;&#60;tag&#62;&#38;gt;' then Assert.Fail('re-escape numeric');
  if ALXMLTextEncode(AnsiString('&amp;<tag>&gt;'), False) <> '&amp;amp;&lt;tag&gt;&amp;gt;' then Assert.Fail('re-escape entity');

  // Quotes should pass through unchanged (this proc only handles &, <, >)
  if ALXMLTextEncode(AnsiString('She said: "Hi" and it''s ok'), True) <> 'She said: "Hi" and it''s ok' then Assert.Fail('quotes passthrough numeric');
  if ALXMLTextEncode(AnsiString('She said: "Hi" and it''s ok'), False) <> 'She said: "Hi" and it''s ok' then Assert.Fail('quotes passthrough entity');

  // Embedded #0 must be preserved (loop is by length, not null-terminated)
  var S := AnsiString('A') + AnsiChar(#0) + AnsiString('&< >B');
  var E := AnsiString('A') + AnsiChar(#0) + AnsiString('&#38;&#60; &#62;B');
  if ALXMLTextEncode(S, True) <> E then Assert.Fail('embedded #0 numeric');

  E := AnsiString('A') + AnsiChar(#0) + AnsiString('&amp;&lt; &gt;B');
  if ALXMLTextEncode(S, False) <> E then Assert.Fail('embedded #0 entity');

  // Edge: special at the very end
  if ALXMLTextEncode(AnsiString('end&'), True) <> 'end&#38;' then Assert.Fail('end amp numeric');
  if ALXMLTextEncode(AnsiString('end<'), True) <> 'end&#60;' then Assert.Fail('end lt numeric');
  if ALXMLTextEncode(AnsiString('end>'), True) <> 'end&#62;' then Assert.Fail('end gt numeric');

  if ALXMLTextEncode(AnsiString('end&'), False) <> 'end&amp;' then Assert.Fail('end amp entity');
  if ALXMLTextEncode(AnsiString('end<'), False) <> 'end&lt;' then Assert.Fail('end lt entity');
  if ALXMLTextEncode(AnsiString('end>'), False) <> 'end&gt;' then Assert.Fail('end gt entity');

  // Longish text with many replacements - numeric
  if ALXMLTextEncode(AnsiString('T&x<y> & <tag> && end'), True) <> 'T&#38;x&#60;y&#62; &#38; &#60;tag&#62; &#38;&#38; end' then Assert.Fail('long numeric');

  // Longish text with many replacements - named
  if ALXMLTextEncode(AnsiString('T&x<y> & <tag> && end'), False) <> 'T&amp;x&lt;y&gt; &amp; &lt;tag&gt; &amp;&amp; end' then Assert.Fail('long entity');
end;

{***********************************************}
procedure TALDUnitXTestHTML.TestALXMLTextEncodeW;
begin
  // Empty
  if ALXMLTextEncode(String('')) <> '' then Assert.Fail('empty');

  // Plain (no specials)
  if ALXMLTextEncode(String('HelloWorld')) <> 'HelloWorld' then Assert.Fail('plain');

  // Single specials - numeric
  if ALXMLTextEncode(String('&'), True) <> '&#38;' then Assert.Fail('amp numeric');
  if ALXMLTextEncode(String('<'), True) <> '&#60;' then Assert.Fail('lt numeric');
  if ALXMLTextEncode(String('>'), True) <> '&#62;' then Assert.Fail('gt numeric');

  // Single specials - named
  if ALXMLTextEncode(String('&'), False) <> '&amp;' then Assert.Fail('amp entity');
  if ALXMLTextEncode(String('<'), False) <> '&lt;' then Assert.Fail('lt entity');
  if ALXMLTextEncode(String('>'), False) <> '&gt;' then Assert.Fail('gt entity');

  // Mixed specials - numeric
  if ALXMLTextEncode(String('A&B<C>D'), True) <> 'A&#38;B&#60;C&#62;D' then Assert.Fail('mixed numeric');

  // Mixed specials - named
  if ALXMLTextEncode(String('A&B<C>D'), False) <> 'A&amp;B&lt;C&gt;D' then Assert.Fail('mixed entity');

  // Already-encoded input gets re-escaped (by design of this proc)
  if ALXMLTextEncode(String('&amp;<tag>&gt;'), True) <> '&#38;amp;&#60;tag&#62;&#38;gt;' then Assert.Fail('re-escape numeric');
  if ALXMLTextEncode(String('&amp;<tag>&gt;'), False) <> '&amp;amp;&lt;tag&gt;&amp;gt;' then Assert.Fail('re-escape entity');

  // Quotes should pass through unchanged (this proc only handles &, <, >)
  if ALXMLTextEncode(String('She said: "Hi" and it''s ok'), True) <> 'She said: "Hi" and it''s ok' then Assert.Fail('quotes passthrough numeric');
  if ALXMLTextEncode(String('She said: "Hi" and it''s ok'), False) <> 'She said: "Hi" and it''s ok' then Assert.Fail('quotes passthrough entity');

  // Embedded #0 must be preserved (loop is by length, not null-terminated)
  var S := String('A') + AnsiChar(#0) + String('&< >B');
  var E := String('A') + AnsiChar(#0) + String('&#38;&#60; &#62;B');
  if ALXMLTextEncode(S, True) <> E then Assert.Fail('embedded #0 numeric');

  E := String('A') + AnsiChar(#0) + String('&amp;&lt; &gt;B');
  if ALXMLTextEncode(S, False) <> E then Assert.Fail('embedded #0 entity');

  // Edge: special at the very end
  if ALXMLTextEncode(String('end&'), True) <> 'end&#38;' then Assert.Fail('end amp numeric');
  if ALXMLTextEncode(String('end<'), True) <> 'end&#60;' then Assert.Fail('end lt numeric');
  if ALXMLTextEncode(String('end>'), True) <> 'end&#62;' then Assert.Fail('end gt numeric');

  if ALXMLTextEncode(String('end&'), False) <> 'end&amp;' then Assert.Fail('end amp entity');
  if ALXMLTextEncode(String('end<'), False) <> 'end&lt;' then Assert.Fail('end lt entity');
  if ALXMLTextEncode(String('end>'), False) <> 'end&gt;' then Assert.Fail('end gt entity');

  // Longish text with many replacements - numeric
  if ALXMLTextEncode(String('T&x<y> & <tag> && end'), True) <> 'T&#38;x&#60;y&#62; &#38; &#60;tag&#62; &#38;&#38; end' then Assert.Fail('long numeric');

  // Longish text with many replacements - named
  if ALXMLTextEncode(String('T&x<y> & <tag> && end'), False) <> 'T&amp;x&lt;y&gt; &amp; &lt;tag&gt; &amp;&amp; end' then Assert.Fail('long entity');
end;

{********************************************************}
procedure TALDUnitXTestHTML.TestALXMLEntityDecodeInPlaceA;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure RunCase(const Input, Expected, Msg: AnsiString);
  begin
    var T: AnsiString := Input;
    ALXMLEntityDecodeInPlace(T);
    if T <> Expected then
      Assert.Fail(
        'ALXMLEntityDecodeInPlace: ' + string(Msg) +
        ' | Expected="' + string(Expected) +
        '" | Actual="' + string(T) + '"');
  end;

begin
  // Empty
  RunCase(AnsiString(''), AnsiString(''), 'empty');

  // Plain text (no entities) => unchanged
  RunCase(AnsiString('Hello, world!'), AnsiString('Hello, world!'), 'plain');

  // Named entities
  RunCase(AnsiString('&amp;'),  AnsiString('&'),  'named &amp;');
  RunCase(AnsiString('&lt;'),   AnsiString('<'),  'named &lt;');
  RunCase(AnsiString('&gt;'),   AnsiString('>'),  'named &gt;');
  RunCase(AnsiString('&quot;'), AnsiString('"'),  'named &quot;');
  RunCase(AnsiString('&apos;'), AnsiString(#39),  'named &apos;');

  // Named entities mixed with text
  RunCase(AnsiString('A&amp;B&lt;C&gt;D'), AnsiString('A&B<C>D'), 'named mixed');

  // Decimal numeric entities (ASCII-range)
  RunCase(AnsiString('&#38;'), AnsiString('&'),  'dec &');
  RunCase(AnsiString('&#60;'), AnsiString('<'),  'dec <');
  RunCase(AnsiString('&#62;'), AnsiString('>'),  'dec >');
  RunCase(AnsiString('A&#38;B&#60;C&#62;D'), AnsiString('A&B<C>D'), 'dec mixed');

  // Hex numeric entities (lowercase x only, as implemented)
  RunCase(AnsiString('&#x26;'), AnsiString('&'),  'hex &');
  RunCase(AnsiString('&#x3c;'), AnsiString('<'),  'hex <');
  RunCase(AnsiString('&#x3E;'), AnsiString('>'),  'hex > (upper hex digits)');
  RunCase(AnsiString('A&#x26;B&#x3c;C&#x3E;D'), AnsiString('A&B<C>D'), 'hex mixed');

  // Mixed named + numeric
  RunCase(AnsiString('&amp;&#60;&gt;&quot;&apos;'), AnsiString('&<>"'''), 'mixed named+numeric');

  // Edge positions: at start and end
  RunCase(AnsiString('&amp;start'),  AnsiString('&start'),  'edge start');
  RunCase(AnsiString('end&lt;'),     AnsiString('end<'),    'edge end <');
  RunCase(AnsiString('end&gt;'),     AnsiString('end>'),    'edge end >');
  RunCase(AnsiString('end&#38;'),    AnsiString('end&'),    'edge end dec &');
  RunCase(AnsiString('end&#x26;'),   AnsiString('end&'),    'edge end hex &');

  // Already-decoded text => idempotent if no entities present
  var S := AnsiString('Just text & symbols but no entities');
  RunCase(S, S, 'idempotent (no entities)');

  // Repeated decoding should be a no-op after first pass
  S := AnsiString('A&amp;B&lt;C&gt;D');
  ALXMLEntityDecodeInPlace(S);
  if S <> AnsiString('A&B<C>D') then Assert.Fail('first decode mismatch');
  ALXMLEntityDecodeInPlace(S);
  if S <> AnsiString('A&B<C>D') then Assert.Fail('second decode should be idempotent');

  // Unterminated entity => should remain effectively unchanged
  RunCase(AnsiString('&amp'),  AnsiString('&amp'),  'unterminated &amp');
  RunCase(AnsiString('&#60'),  AnsiString('&#60'),  'unterminated dec');
  RunCase(AnsiString('&#x3c'), AnsiString('&#x3c'), 'unterminated hex');

  // Unknown named entity => unchanged
  RunCase(AnsiString('&unknown;'), AnsiString('&unknown;'), 'unknown named');

  // Case sensitivity on named entities (code matches lowercase only)
  RunCase(AnsiString('&AMP;'), AnsiString('&AMP;'), 'named uppercase not decoded');
  RunCase(AnsiString('&Lt;'),  AnsiString('&Lt;'),  'named mixed-case not decoded');

  // Invalid numeric (non-hex digit in hex entity) => unchanged
  RunCase(AnsiString('&#xZZ;'), AnsiString('&#xZZ;'), 'invalid hex digits');
  RunCase(AnsiString('&#x2G;'), AnsiString('&#x2G;'), 'invalid hex digit G');

  // Too long entity (collector only stores up to 10 chars before ';')
  RunCase(AnsiString('&#12345678901;'), AnsiString('&#12345678901;'), 'too long dec');
  RunCase(AnsiString('&#x123456789AB;'), AnsiString('&#x123456789AB;'), 'too long hex');

  // Embedded #0 should be preserved; decoding still works around it
  // (String length loop logic uses indices, not null-terminated reads)
  var WithZero := AnsiString('A') + AnsiChar(#0) + AnsiString('&amp;B');
  var ExpectZero := AnsiString('A') + AnsiChar(#0) + AnsiString('&B');
  RunCase(WithZero, ExpectZero, 'embedded #0');

  // Mixed large sample
  RunCase(AnsiString('T&amp;x&#60;y&#62; &lt;tag&gt; &amp;&amp; "Q" &apos;A&apos; end'), AnsiString('T&x<y> <tag> && "Q" ''A'' end'), 'large mixed sample');

  // Boundary numeric values in ASCII range
  RunCase(AnsiString('&#0;'),   AnsiString(#0),   'dec NUL');
  RunCase(AnsiString('&#127;'), AnsiString(#127), 'dec DEL');
  RunCase(AnsiString('&#x00;'), AnsiString(#0),   'hex NUL');
  RunCase(AnsiString('&#x7F;'), AnsiString(#127), 'hex DEL');

  // Single code point that requires two UTF-16 code units (surrogate pair)
  // With AnsiString output this will usually become '?' (ANSI fallback).
  RunCase(AnsiString('&#x1F600;'), AnsiString('😀'), 'emoji U+1F600 😀');
  RunCase(AnsiString('&#x1F680;'), AnsiString('🚀'), 'emoji U+1F680 🚀');
  RunCase(AnsiString('&#x1F9E0;'), AnsiString('🧠'), 'emoji U+1F9E0 🧠');

  // Require at least one digit
  RunCase(AnsiString('&#x;'), AnsiString('&#x;'), 'hex entity with NO digits must stay unchanged');
  RunCase(AnsiString('&#;'),  AnsiString('&#;'),  'decimal entity with NO digits must stay unchanged');

  // Above Unicode max (U+10FFFF)
  RunCase(AnsiString('&#x110000;'), AnsiString('&#x110000;'), 'hex above Unicode max');
  RunCase(AnsiString('&#1114112;'), AnsiString('&#1114112;'), 'dec above Unicode max');

  // Surrogate range (U+D800–U+DFFF) are not valid scalar values
  RunCase(AnsiString('&#xD800;'), AnsiString('&#xD800;'), 'hex high-surrogate start');
  RunCase(AnsiString('&#xDFFF;'), AnsiString('&#xDFFF;'), 'hex low-surrogate end');
  RunCase(AnsiString('&#55296;'), AnsiString('&#55296;'), 'dec high-surrogate start'); // 55296 = $D800
  RunCase(AnsiString('&#57343;'), AnsiString('&#57343;'), 'dec low-surrogate end'); // 57343 = $DFFF
end;

{********************************************************}
procedure TALDUnitXTestHTML.TestALXMLEntityDecodeInPlaceW;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure RunCase(const Input, Expected, Msg: String);
  begin
    var T: String := Input;
    ALXMLEntityDecodeInPlace(T);
    if T <> Expected then
      Assert.Fail(
        'ALXMLEntityDecodeInPlace: ' + string(Msg) +
        ' | Expected="' + string(Expected) +
        '" | Actual="' + string(T) + '"');
  end;

begin
  // Empty
  RunCase(String(''), String(''), 'empty');

  // Plain text (no entities) => unchanged
  RunCase(String('Hello, world!'), String('Hello, world!'), 'plain');

  // Named entities
  RunCase(String('&amp;'),  String('&'),  'named &amp;');
  RunCase(String('&lt;'),   String('<'),  'named &lt;');
  RunCase(String('&gt;'),   String('>'),  'named &gt;');
  RunCase(String('&quot;'), String('"'),  'named &quot;');
  RunCase(String('&apos;'), String(#39),  'named &apos;');

  // Named entities mixed with text
  RunCase(String('A&amp;B&lt;C&gt;D'), String('A&B<C>D'), 'named mixed');

  // Decimal numeric entities (ASCII-range)
  RunCase(String('&#38;'), String('&'),  'dec &');
  RunCase(String('&#60;'), String('<'),  'dec <');
  RunCase(String('&#62;'), String('>'),  'dec >');
  RunCase(String('A&#38;B&#60;C&#62;D'), String('A&B<C>D'), 'dec mixed');

  // Hex numeric entities (lowercase x only, as implemented)
  RunCase(String('&#x26;'), String('&'),  'hex &');
  RunCase(String('&#x3c;'), String('<'),  'hex <');
  RunCase(String('&#x3E;'), String('>'),  'hex > (upper hex digits)');
  RunCase(String('A&#x26;B&#x3c;C&#x3E;D'), String('A&B<C>D'), 'hex mixed');

  // Mixed named + numeric
  RunCase(String('&amp;&#60;&gt;&quot;&apos;'), String('&<>"'''), 'mixed named+numeric');

  // Edge positions: at start and end
  RunCase(String('&amp;start'),  String('&start'),  'edge start');
  RunCase(String('end&lt;'),     String('end<'),    'edge end <');
  RunCase(String('end&gt;'),     String('end>'),    'edge end >');
  RunCase(String('end&#38;'),    String('end&'),    'edge end dec &');
  RunCase(String('end&#x26;'),   String('end&'),    'edge end hex &');

  // Already-decoded text => idempotent if no entities present
  var S := String('Just text & symbols but no entities');
  RunCase(S, S, 'idempotent (no entities)');

  // Repeated decoding should be a no-op after first pass
  S := String('A&amp;B&lt;C&gt;D');
  ALXMLEntityDecodeInPlace(S);
  if S <> String('A&B<C>D') then Assert.Fail('first decode mismatch');
  ALXMLEntityDecodeInPlace(S);
  if S <> String('A&B<C>D') then Assert.Fail('second decode should be idempotent');

  // Unterminated entity => should remain effectively unchanged
  RunCase(String('&amp'),  String('&amp'),  'unterminated &amp');
  RunCase(String('&#60'),  String('&#60'),  'unterminated dec');
  RunCase(String('&#x3c'), String('&#x3c'), 'unterminated hex');

  // Unknown named entity => unchanged
  RunCase(String('&unknown;'), String('&unknown;'), 'unknown named');

  // Case sensitivity on named entities (code matches lowercase only)
  RunCase(String('&AMP;'), String('&AMP;'), 'named uppercase not decoded');
  RunCase(String('&Lt;'),  String('&Lt;'),  'named mixed-case not decoded');

  // Invalid numeric (non-hex digit in hex entity) => unchanged
  RunCase(String('&#xZZ;'), String('&#xZZ;'), 'invalid hex digits');
  RunCase(String('&#x2G;'), String('&#x2G;'), 'invalid hex digit G');

  // Too long entity (collector only stores up to 10 chars before ';')
  RunCase(String('&#12345678901;'), String('&#12345678901;'), 'too long dec');
  RunCase(String('&#x123456789AB;'), String('&#x123456789AB;'), 'too long hex');

  // Embedded #0 should be preserved; decoding still works around it
  // (String length loop logic uses indices, not null-terminated reads)
  var WithZero := String('A') + AnsiChar(#0) + String('&amp;B');
  var ExpectZero := String('A') + AnsiChar(#0) + String('&B');
  RunCase(WithZero, ExpectZero, 'embedded #0');

  // Mixed large sample
  RunCase(String('T&amp;x&#60;y&#62; &lt;tag&gt; &amp;&amp; "Q" &apos;A&apos; end'), String('T&x<y> <tag> && "Q" ''A'' end'), 'large mixed sample');

  // Boundary numeric values in ASCII range
  RunCase(String('&#0;'),   String(#0),   'dec NUL');
  RunCase(String('&#127;'), String(#127), 'dec DEL');
  RunCase(String('&#x00;'), String(#0),   'hex NUL');
  RunCase(String('&#x7F;'), String(#127), 'hex DEL');

  // Single code point that requires two UTF-16 code units (surrogate pair)
  // With String output this will usually become '?' (ANSI fallback).
  RunCase(String('&#x1F600;'), String('😀'), 'emoji U+1F600 😀');
  RunCase(String('&#x1F680;'), String('🚀'), 'emoji U+1F680 🚀');
  RunCase(String('&#x1F9E0;'), String('🧠'), 'emoji U+1F9E0 🧠');

  // Require at least one digit
  RunCase(String('&#x;'), String('&#x;'), 'hex entity with NO digits must stay unchanged');
  RunCase(String('&#;'),  String('&#;'),  'decimal entity with NO digits must stay unchanged');

  // Above Unicode max (U+10FFFF)
  RunCase(String('&#x110000;'), String('&#x110000;'), 'hex above Unicode max');
  RunCase(String('&#1114112;'), String('&#1114112;'), 'dec above Unicode max');

  // Surrogate range (U+D800–U+DFFF) are not valid scalar values
  RunCase(String('&#xD800;'), String('&#xD800;'), 'hex high-surrogate start');
  RunCase(String('&#xDFFF;'), String('&#xDFFF;'), 'hex low-surrogate end');
  RunCase(String('&#55296;'), String('&#55296;'), 'dec high-surrogate start'); // 55296 = $D800
  RunCase(String('&#57343;'), String('&#57343;'), 'dec low-surrogate end'); // 57343 = $DFFF
end;

initialization
  TDUnitX.RegisterTestFixture(TALDUnitXTestHTML);

end.
