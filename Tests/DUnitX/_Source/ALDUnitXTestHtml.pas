unit ALDUnitXTestHtml;

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
    procedure TestXMLAttributeEncodeA;
    [Test]
    procedure TestXMLAttributeEncodeW;
    [Test]
    procedure TestXMLTextEncodeA;
    [Test]
    procedure TestXMLTextEncodeW;
    [Test]
    procedure TestXMLEntityDecodeInPlaceA;
    [Test]
    procedure TestXMLEntityDecodeInPlaceW;
    [Test]
    procedure TestHTMLDecodeInPlaceA;
    [Test]
    procedure TestHTMLDecodeInPlaceW;
    [Test]
    procedure TestJavascriptEncodeA;
    [Test]
    procedure TestJavascriptEncodeW;
    [Test]
    procedure TestJavascriptDecodeA;
    [Test]
    procedure TestJavascriptDecodeW;
  end;

implementation

uses
  System.Character,
  Alcinoe.HTML;

{**************************************************}
procedure TALDUnitXTestHtml.TestXMLAttributeEncodeA;
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

{**************************************************}
procedure TALDUnitXTestHtml.TestXMLAttributeEncodeW;
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

{*********************************************}
procedure TALDUnitXTestHtml.TestXMLTextEncodeA;
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

{*********************************************}
procedure TALDUnitXTestHtml.TestXMLTextEncodeW;
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

{******************************************************}
procedure TALDUnitXTestHtml.TestXMLEntityDecodeInPlaceA;

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

{******************************************************}
procedure TALDUnitXTestHtml.TestXMLEntityDecodeInPlaceW;

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

{*************************************************}
procedure TALDUnitXTestHtml.TestHTMLDecodeInPlaceA;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure RunCase(const Input, Expected, Msg: AnsiString);
  begin
    var T := Input;
    ALHTMLDecodeInPlace(T);
    if T <> Expected then
      Assert.Fail(
        'ALHTMLDecodeInPlace: ' + string(Msg) +
        ' | Expected="' + string(Expected) +
        '" | Actual="' + string(T) + '"');
  end;

var
  ExpectMixed: AnsiString;
  WithZero, ExpectZero: AnsiString;

begin

  // Empty / plain
  RunCase(AnsiString(''), AnsiString(''), 'empty');
  RunCase(AnsiString('Hello, world!'), AnsiString('Hello, world!'), 'plain');

  // Named entities
  RunCase(AnsiString('&amp;'),  AnsiString('&'),  'named &amp;');
  RunCase(AnsiString('&lt;'),   AnsiString('<'),  'named &lt;');
  RunCase(AnsiString('&gt;'),   AnsiString('>'),  'named &gt;');
  RunCase(AnsiString('&quot;'), AnsiString('"'),  'named &quot;');
  RunCase(AnsiString('&apos;'), AnsiString(#39),  'named &apos;');
  RunCase(AnsiString('&thetasym;'), AnsiString('ϑ'),  'named &thetasym;');
  RunCase(AnsiString('&alefsym;'), AnsiString('ℵ'),  'named &alefsym;');


  // Named entities mixed
  RunCase(AnsiString('A&amp;B&lt;C&gt;D'), AnsiString('A&B<C>D'), 'named mixed');

  // Decimal numeric entities (ASCII-range)
  RunCase(AnsiString('&#38;'), AnsiString('&'),  'dec &');
  RunCase(AnsiString('&#60;'), AnsiString('<'),  'dec <');
  RunCase(AnsiString('&#62;'), AnsiString('>'),  'dec >');
  RunCase(AnsiString('A&#38;B&#60;C&#62;D'), AnsiString('A&B<C>D'), 'dec mixed');

  // Hex numeric entities (lowercase x only, as implemented)
  RunCase(AnsiString('&#x26;'), AnsiString('&'),  'hex &');
  RunCase(AnsiString('&#x3c;'), AnsiString('<'),  'hex <');
  RunCase(AnsiString('&#x3E;'), AnsiString('>'),  'hex > uppercase hex digit');
  RunCase(AnsiString('A&#x26;B&#x3c;C&#x3E;D'), AnsiString('A&B<C>D'), 'hex mixed');

  // Uppercase X is NOT supported by this implementation → unchanged
  RunCase(AnsiString('&#X26;'), AnsiString('&#X26;'), 'hex with uppercase X (unsupported)');

  // Mixed named + numeric
  ExpectMixed := AnsiString('&<>"'''); // & then < then > then " then '
  RunCase(AnsiString('&amp;&#60;&gt;&quot;&apos;'), ExpectMixed, 'mixed named+numeric');

  // Edge positions: at start and end
  RunCase(AnsiString('&amp;start'),  AnsiString('&start'),  'edge start');
  RunCase(AnsiString('end&lt;'),     AnsiString('end<'),    'edge end <');
  RunCase(AnsiString('end&gt;'),     AnsiString('end>'),    'edge end >');
  RunCase(AnsiString('end&#38;'),    AnsiString('end&'),    'edge end dec &');
  RunCase(AnsiString('end&#x26;'),   AnsiString('end&'),    'edge end hex &');

  // Idempotence: decode twice = same result
  var S := AnsiString('A&amp;B&lt;C&gt;D');
  ALHTMLDecodeInPlace(S);
  if S <> AnsiString('A&B<C>D') then Assert.Fail('first decode mismatch');
  ALHTMLDecodeInPlace(S);
  if S <> AnsiString('A&B<C>D') then Assert.Fail('second decode should be idempotent');

  // Unterminated entity → unchanged
  RunCase(AnsiString('&amp'),  AnsiString('&amp'),  'unterminated &amp');
  RunCase(AnsiString('&#60'),  AnsiString('&#60'),  'unterminated dec');
  RunCase(AnsiString('&#x3c'), AnsiString('&#x3c'), 'unterminated hex');

  // Unknown named entity → unchanged
  RunCase(AnsiString('&unknown;'), AnsiString('&unknown;'), 'unknown named');

  // Case sensitivity on named entities (dict contains lowercase only)
  RunCase(AnsiString('&AMP;'), AnsiString('&AMP;'), 'named uppercase not decoded');
  RunCase(AnsiString('&Lt;'),  AnsiString('&Lt;'),  'named mixed-case not decoded');

  // Invalid numeric (non-hex digit in hex entity) → unchanged, MUST NOT raise
  RunCase(AnsiString('&#xZZ;'), AnsiString('&#xZZ;'), 'invalid hex digits');
  RunCase(AnsiString('&#x2G;'), AnsiString('&#x2G;'), 'invalid hex digit G');

  // Require at least one digit in numeric forms → unchanged
  RunCase(AnsiString('&#x;'), AnsiString('&#x;'), 'hex with no digits');
  RunCase(AnsiString('&#;'),  AnsiString('&#;'),  'decimal with no digits');

  // Too long entity (collector only stores up to 10 chars before ';') → unchanged
  RunCase(AnsiString('&#12345678901;'),  AnsiString('&#12345678901;'),  'too long dec');
  RunCase(AnsiString('&#x123456789AB;'), AnsiString('&#x123456789AB;'), 'too long hex');

  // Embedded #0 should be preserved; decoding still works around it
  WithZero  := AnsiString('A') + AnsiChar(#0) + AnsiString('&amp;B');
  ExpectZero:= AnsiString('A') + AnsiChar(#0) + AnsiString('&B');
  RunCase(WithZero, ExpectZero, 'embedded #0');

  // Larger mixed sample
  RunCase(AnsiString('T&amp;x&#60;y&#62; &lt;tag&gt; &amp;&amp; "Q" &apos;A&apos; end'), AnsiString('T&x<y> <tag> && "Q" ''A'' end'), 'large mixed sample');

  // Boundary numeric values in ASCII range
  RunCase(AnsiString('&#0;'),   AnsiString(#0),   'dec NUL');
  RunCase(AnsiString('&#127;'), AnsiString(#127), 'dec DEL');
  RunCase(AnsiString('&#x00;'), AnsiString(#0),   'hex NUL');
  RunCase(AnsiString('&#x7F;'), AnsiString(#127), 'hex DEL');

end;

{*************************************************}
procedure TALDUnitXTestHtml.TestHTMLDecodeInPlaceW;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure RunCase(const Input, Expected, Msg: String);
  begin
    var T := Input;
    ALHTMLDecodeInPlace(T);
    if T <> Expected then
      Assert.Fail(
        'ALHTMLDecodeInPlace: ' + string(Msg) +
        ' | Expected="' + string(Expected) +
        '" | Actual="' + string(T) + '"');
  end;

var
  ExpectMixed: String;
  WithZero, ExpectZero: String;

begin

  // Empty / plain
  RunCase(String(''), String(''), 'empty');
  RunCase(String('Hello, world!'), String('Hello, world!'), 'plain');

  // Named entities
  RunCase(String('&amp;'),  String('&'),  'named &amp;');
  RunCase(String('&lt;'),   String('<'),  'named &lt;');
  RunCase(String('&gt;'),   String('>'),  'named &gt;');
  RunCase(String('&quot;'), String('"'),  'named &quot;');
  RunCase(String('&apos;'), String(#39),  'named &apos;');
  RunCase(String('&thetasym;'), String('ϑ'),  'named &thetasym;');
  RunCase(String('&alefsym;'), String('ℵ'),  'named &alefsym;');


  // Named entities mixed
  RunCase(String('A&amp;B&lt;C&gt;D'), String('A&B<C>D'), 'named mixed');

  // Decimal numeric entities (ASCII-range)
  RunCase(String('&#38;'), String('&'),  'dec &');
  RunCase(String('&#60;'), String('<'),  'dec <');
  RunCase(String('&#62;'), String('>'),  'dec >');
  RunCase(String('A&#38;B&#60;C&#62;D'), String('A&B<C>D'), 'dec mixed');

  // Hex numeric entities (lowercase x only, as implemented)
  RunCase(String('&#x26;'), String('&'),  'hex &');
  RunCase(String('&#x3c;'), String('<'),  'hex <');
  RunCase(String('&#x3E;'), String('>'),  'hex > uppercase hex digit');
  RunCase(String('A&#x26;B&#x3c;C&#x3E;D'), String('A&B<C>D'), 'hex mixed');

  // Uppercase X is NOT supported by this implementation → unchanged
  RunCase(String('&#X26;'), String('&#X26;'), 'hex with uppercase X (unsupported)');

  // Mixed named + numeric
  ExpectMixed := String('&<>"'''); // & then < then > then " then '
  RunCase(String('&amp;&#60;&gt;&quot;&apos;'), ExpectMixed, 'mixed named+numeric');

  // Edge positions: at start and end
  RunCase(String('&amp;start'),  String('&start'),  'edge start');
  RunCase(String('end&lt;'),     String('end<'),    'edge end <');
  RunCase(String('end&gt;'),     String('end>'),    'edge end >');
  RunCase(String('end&#38;'),    String('end&'),    'edge end dec &');
  RunCase(String('end&#x26;'),   String('end&'),    'edge end hex &');

  // Idempotence: decode twice = same result
  var S := String('A&amp;B&lt;C&gt;D');
  ALHTMLDecodeInPlace(S);
  if S <> String('A&B<C>D') then Assert.Fail('first decode mismatch');
  ALHTMLDecodeInPlace(S);
  if S <> String('A&B<C>D') then Assert.Fail('second decode should be idempotent');

  // Unterminated entity → unchanged
  RunCase(String('&amp'),  String('&amp'),  'unterminated &amp');
  RunCase(String('&#60'),  String('&#60'),  'unterminated dec');
  RunCase(String('&#x3c'), String('&#x3c'), 'unterminated hex');

  // Unknown named entity → unchanged
  RunCase(String('&unknown;'), String('&unknown;'), 'unknown named');

  // Case sensitivity on named entities (dict contains lowercase only)
  RunCase(String('&AMP;'), String('&AMP;'), 'named uppercase not decoded');
  RunCase(String('&Lt;'),  String('&Lt;'),  'named mixed-case not decoded');

  // Invalid numeric (non-hex digit in hex entity) → unchanged, MUST NOT raise
  RunCase(String('&#xZZ;'), String('&#xZZ;'), 'invalid hex digits');
  RunCase(String('&#x2G;'), String('&#x2G;'), 'invalid hex digit G');

  // Require at least one digit in numeric forms → unchanged
  RunCase(String('&#x;'), String('&#x;'), 'hex with no digits');
  RunCase(String('&#;'),  String('&#;'),  'decimal with no digits');

  // Too long entity (collector only stores up to 10 chars before ';') → unchanged
  RunCase(String('&#12345678901;'),  String('&#12345678901;'),  'too long dec');
  RunCase(String('&#x123456789AB;'), String('&#x123456789AB;'), 'too long hex');

  // Embedded #0 should be preserved; decoding still works around it
  WithZero  := String('A') + AnsiChar(#0) + String('&amp;B');
  ExpectZero:= String('A') + AnsiChar(#0) + String('&B');
  RunCase(WithZero, ExpectZero, 'embedded #0');

  // Larger mixed sample
  RunCase(String('T&amp;x&#60;y&#62; &lt;tag&gt; &amp;&amp; "Q" &apos;A&apos; end'), String('T&x<y> <tag> && "Q" ''A'' end'), 'large mixed sample');

  // Boundary numeric values in ASCII range
  RunCase(String('&#0;'),   String(#0),   'dec NUL');
  RunCase(String('&#127;'), String(#127), 'dec DEL');
  RunCase(String('&#x00;'), String(#0),   'hex NUL');
  RunCase(String('&#x7F;'), String(#127), 'hex DEL');

end;

{************************************************}
procedure TALDUnitXTestHtml.TestJavascriptEncodeA;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure RunCase(const Input, Expected: AnsiString; UseNumeric: Boolean; const Msg: string);
  begin
    var OutS := ALJavascriptEncode(Input, UseNumeric);
    if OutS <> Expected then
      Assert.Fail(
          'ALJavascriptEncode: ' + Msg +
          ' | Expected="' + string(Expected) +
          '" | Actual="'   + string(OutS) + '"'
        );
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure RunNum(const I, E: AnsiString; const Msg: string);
  begin
    RunCase(I, E, True,  Msg);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure RunShort(const I, E: AnsiString; const Msg: string);
  begin
    RunCase(I, E, False, Msg);
  end;

var
  SIn, SExp: AnsiString;

begin
  // --- Empty / passthrough ---
  RunNum  (AnsiString(''), AnsiString(''), 'empty numeric');
  RunShort(AnsiString(''), AnsiString(''), 'empty short');
  RunNum  (AnsiString('Hello, world!'), AnsiString('Hello, world!'), 'plain numeric');
  RunShort(AnsiString('Hello, world!'), AnsiString('Hello, world!'), 'plain short');

  // --- Each escaped character (solo) ---
  // Backspace (#8)
  RunNum  (AnsiString(#8),  AnsiString('\u0008'), 'backspace numeric');
  RunShort(AnsiString(#8),  AnsiString('\b'),     'backspace short');

  // Tab (#9)
  RunNum  (AnsiString(#9),  AnsiString('\u0009'), 'tab numeric');
  RunShort(AnsiString(#9),  AnsiString('\t'),     'tab short');

  // LF (#10)
  RunNum  (AnsiString(#10), AnsiString('\u000A'), 'lf numeric');
  RunShort(AnsiString(#10), AnsiString('\n'),     'lf short');

  // VT (#11)
  RunNum  (AnsiString(#11), AnsiString('\u000B'), 'vt numeric');
  RunShort(AnsiString(#11), AnsiString('\v'),     'vt short');

  // FF (#12)
  RunNum  (AnsiString(#12), AnsiString('\u000C'), 'ff numeric');
  RunShort(AnsiString(#12), AnsiString('\f'),     'ff short');

  // CR (#13)
  RunNum  (AnsiString(#13), AnsiString('\u000D'), 'cr numeric');
  RunShort(AnsiString(#13), AnsiString('\r'),     'cr short');

  // Double quote (#34)
  RunNum  (AnsiString('"'), AnsiString('\u0022'), 'double quote numeric');
  RunShort(AnsiString('"'), AnsiString('\"'),     'double quote short');

  // Ampersand (#38) — always numeric by design
  RunNum  (AnsiString('&'), AnsiString('\u0026'), 'ampersand numeric');
  RunShort(AnsiString('&'), AnsiString('\u0026'), 'ampersand short(always numeric)');

  // Single quote (#39)
  RunNum  (AnsiString(#39), AnsiString('\u0027'), 'single quote numeric');
  RunShort(AnsiString(#39), AnsiString('\'''),    'single quote short');

  // Less-than (#60) — always numeric
  RunNum  (AnsiString('<'), AnsiString('\u003C'), 'lt numeric');
  RunShort(AnsiString('<'), AnsiString('\u003C'), 'lt short(always numeric)');

  // Greater-than (#62) — always numeric
  RunNum  (AnsiString('>'), AnsiString('\u003E'), 'gt numeric');
  RunShort(AnsiString('>'), AnsiString('\u003E'), 'gt short(always numeric)');

  // Backslash (#92)
  RunNum  (AnsiString('\'), AnsiString('\u005C'), 'backslash numeric');
  RunShort(AnsiString('\'), AnsiString('\\'),     'backslash short');

  // --- Mixed strings ---
  // Numeric mode
  SIn  := AnsiString('Line1' + AnsiString(#10) + 'Line2 & "Q" ' + AnsiString(#39) + 'S' + AnsiString(#39) + ' <tag> \ end');
  SExp := AnsiString('Line1\u000ALine2 \u0026 \u0022Q\u0022 \u0027S\u0027 \u003Ctag\u003E \u005C end');
  RunNum(SIn, SExp, 'mixed numeric');

  // Short-escape mode (note: &, <, > remain numeric)
  SExp := AnsiString('Line1\nLine2 \u0026 \"Q\" \''S\'' \u003Ctag\u003E \\ end');
  RunShort(SIn, SExp, 'mixed short');

  // --- Embedded #0 preserved ---
  SIn  := AnsiString('A') + AnsiChar(#0) + AnsiString('&B');
  SExp := AnsiString('A') + AnsiChar(#0) + AnsiString('\u0026B');
  RunNum(SIn,  SExp, 'embedded #0 numeric');
  RunShort(SIn, SExp, 'embedded #0 short');

  // --- Non-ASCII bytes pass through unchanged ---
  // (e.g., byte $E9; content unaffected in both modes)
  SIn := AnsiString(AnsiChar(#233)) + AnsiString(' Café & Bar');
  // Expect: \u0026 for '&', but the leading #233 and ' Café ' bytes are unchanged
  RunNum  (SIn, AnsiString(AnsiChar(#233)) + AnsiString(' Café \u0026 Bar'), 'non-ascii numeric');
  RunShort(SIn, AnsiString(AnsiChar(#233)) + AnsiString(' Café \u0026 Bar'), 'non-ascii short');

  // --- No-escape strings remain identical ---
  RunNum  (AnsiString('AlphaNumeric_123'), AnsiString('AlphaNumeric_123'), 'no-escape numeric');
  RunShort(AnsiString('AlphaNumeric_123'), AnsiString('AlphaNumeric_123'), 'no-escape short');

  // --- CRLF pair ---
  RunNum  (AnsiString(#13#10), AnsiString('\u000D\u000A'), 'CRLF numeric');
  RunShort(AnsiString(#13#10), AnsiString('\r\n'),         'CRLF short');
end;

{************************************************}
procedure TALDUnitXTestHtml.TestJavascriptEncodeW;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure RunCase(const Input, Expected: String; UseNumeric: Boolean; const Msg: string);
  begin
    var OutS := ALJavascriptEncode(Input, UseNumeric);
    if OutS <> Expected then
      Assert.Fail(
          'ALJavascriptEncode: ' + Msg +
          ' | Expected="' + string(Expected) +
          '" | Actual="'   + string(OutS) + '"'
        );
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure RunNum(const I, E: String; const Msg: string);
  begin
    RunCase(I, E, True,  Msg);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure RunShort(const I, E: String; const Msg: string);
  begin
    RunCase(I, E, False, Msg);
  end;

var
  SIn, SExp: String;

begin
  // --- Empty / passthrough ---
  RunNum  (String(''), String(''), 'empty numeric');
  RunShort(String(''), String(''), 'empty short');
  RunNum  (String('Hello, world!'), String('Hello, world!'), 'plain numeric');
  RunShort(String('Hello, world!'), String('Hello, world!'), 'plain short');

  // --- Each escaped character (solo) ---
  // Backspace (#8)
  RunNum  (String(#8),  String('\u0008'), 'backspace numeric');
  RunShort(String(#8),  String('\b'),     'backspace short');

  // Tab (#9)
  RunNum  (String(#9),  String('\u0009'), 'tab numeric');
  RunShort(String(#9),  String('\t'),     'tab short');

  // LF (#10)
  RunNum  (String(#10), String('\u000A'), 'lf numeric');
  RunShort(String(#10), String('\n'),     'lf short');

  // VT (#11)
  RunNum  (String(#11), String('\u000B'), 'vt numeric');
  RunShort(String(#11), String('\v'),     'vt short');

  // FF (#12)
  RunNum  (String(#12), String('\u000C'), 'ff numeric');
  RunShort(String(#12), String('\f'),     'ff short');

  // CR (#13)
  RunNum  (String(#13), String('\u000D'), 'cr numeric');
  RunShort(String(#13), String('\r'),     'cr short');

  // Double quote (#34)
  RunNum  (String('"'), String('\u0022'), 'double quote numeric');
  RunShort(String('"'), String('\"'),     'double quote short');

  // Ampersand (#38) — always numeric by design
  RunNum  (String('&'), String('\u0026'), 'ampersand numeric');
  RunShort(String('&'), String('\u0026'), 'ampersand short(always numeric)');

  // Single quote (#39)
  RunNum  (String(#39), String('\u0027'), 'single quote numeric');
  RunShort(String(#39), String('\'''),    'single quote short');

  // Less-than (#60) — always numeric
  RunNum  (String('<'), String('\u003C'), 'lt numeric');
  RunShort(String('<'), String('\u003C'), 'lt short(always numeric)');

  // Greater-than (#62) — always numeric
  RunNum  (String('>'), String('\u003E'), 'gt numeric');
  RunShort(String('>'), String('\u003E'), 'gt short(always numeric)');

  // Backslash (#92)
  RunNum  (String('\'), String('\u005C'), 'backslash numeric');
  RunShort(String('\'), String('\\'),     'backslash short');

  // --- Mixed strings ---
  // Numeric mode
  SIn  := String('Line1' + String(#10) + 'Line2 & "Q" ' + String(#39) + 'S' + String(#39) + ' <tag> \ end');
  SExp := String('Line1\u000ALine2 \u0026 \u0022Q\u0022 \u0027S\u0027 \u003Ctag\u003E \u005C end');
  RunNum(SIn, SExp, 'mixed numeric');

  // Short-escape mode (note: &, <, > remain numeric)
  SExp := String('Line1\nLine2 \u0026 \"Q\" \''S\'' \u003Ctag\u003E \\ end');
  RunShort(SIn, SExp, 'mixed short');

  // --- Embedded #0 preserved ---
  SIn  := String('A') + AnsiChar(#0) + String('&B');
  SExp := String('A') + AnsiChar(#0) + String('\u0026B');
  RunNum(SIn,  SExp, 'embedded #0 numeric');
  RunShort(SIn, SExp, 'embedded #0 short');

  // --- Non-ASCII bytes pass through unchanged ---
  // (e.g., byte $E9; content unaffected in both modes)
  SIn := String(AnsiChar(#233)) + String(' Café & Bar');
  // Expect: \u0026 for '&', but the leading #233 and ' Café ' bytes are unchanged
  RunNum  (SIn, String(AnsiChar(#233)) + String(' Café \u0026 Bar'), 'non-ascii numeric');
  RunShort(SIn, String(AnsiChar(#233)) + String(' Café \u0026 Bar'), 'non-ascii short');

  // --- No-escape strings remain identical ---
  RunNum  (String('AlphaNumeric_123'), String('AlphaNumeric_123'), 'no-escape numeric');
  RunShort(String('AlphaNumeric_123'), String('AlphaNumeric_123'), 'no-escape short');

  // --- CRLF pair ---
  RunNum  (String(#13#10), String('\u000D\u000A'), 'CRLF numeric');
  RunShort(String(#13#10), String('\r\n'),         'CRLF short');
end;

{************************************************}
procedure TALDUnitXTestHtml.TestJavascriptDecodeA;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure RunCase(const Input, Expected: AnsiString; const Msg: string);
  begin
    var S := Input;
    ALJavascriptDecodeInPlace(S);
    if S <> Expected then
      Assert.Fail(
        'ALJavascriptDecodeInPlace: ' + Msg +
        ' | Expected="' + string(Expected) +
        '" | Actual="'   + string(S) + '"');
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function FromCP32(Code: Cardinal): AnsiString;
  begin
    // Mirror the function’s path: UTF-32 -> UTF-16 -> AnsiString via system ACP.
    Result := AnsiString(Char.ConvertFromUtf32(Code));
  end;

var
  InS, ExpS: AnsiString;

begin
  // --- Plain / empty ---
  RunCase(AnsiString(''), AnsiString(''), 'empty');
  RunCase(AnsiString('Hello, world!'), AnsiString('Hello, world!'), 'plain');

  // --- Simple JS escapes ---
  RunCase(AnsiString('\b'), AnsiString(#8),  'backspace');
  RunCase(AnsiString('\t'), AnsiString(#9),  'tab');
  RunCase(AnsiString('\n'), AnsiString(#10), 'newline');
  RunCase(AnsiString('\v'), AnsiString(#11), 'vertical tab');
  RunCase(AnsiString('\f'), AnsiString(#12), 'form feed');
  RunCase(AnsiString('\r'), AnsiString(#13), 'carriage return');
  RunCase(AnsiString('\"'), AnsiString(#34), 'double quote');
  RunCase(AnsiString('\/'), AnsiString('/'), 'forward slash (unknown escape → keep char, drop backslash)');
  // \' cannot be written directly; build it:
  RunCase(AnsiString('\') + AnsiChar(#39), AnsiString(#39), 'single quote');
  RunCase(AnsiString('\\'), AnsiString('\'), 'backslash');

  // --- Hex ISO-8859-1: \xXX ---
  RunCase(AnsiString('\x41'), AnsiString('A'), 'hex A');
  RunCase(AnsiString('\x61'), AnsiString('a'), 'hex a');
  RunCase(AnsiString('\x00'), AnsiString(#0),  'hex NUL');
  RunCase(AnsiString('\xFF'), AnsiString('ÿ'), 'hex 0xFF');

  // Invalid hex: not both digits -> drop backslash, keep the rest
  RunCase(AnsiString('\xG0'), AnsiString('xG0'), 'hex invalid (G0)');
  RunCase(AnsiString('\x0G'), AnsiString('x0G'), 'hex invalid (0G)');
  // Too short: "\x" or "\xA" → drop backslash, keep 'x' (and the rest)
  RunCase(AnsiString('\x'),   AnsiString('x'),   'hex too short (just \x)');
  RunCase(AnsiString('\xA'),  AnsiString('xA'),  'hex too short (one digit)');

  // --- Octal ISO-8859-1: \ooo (exactly 3 octal digits) ---
  RunCase(AnsiString('\141'), AnsiString('a'), 'octal 141 = a');
  RunCase(AnsiString('\101'), AnsiString('A'), 'octal 101 = A');
  RunCase(AnsiString('\000'), AnsiString(#0),  'octal NUL');
  RunCase(AnsiString('\377'), AnsiString('ÿ'), 'octal 377 = 255');

  // Invalid/out of range: \400 (=256) → delete backslash only
  RunCase(AnsiString('\400'), AnsiString('400'), 'octal out-of-range 400');
  // Not 3 digits → not matched as octal; drop backslash and keep first char
  RunCase(AnsiString('\47'),  AnsiString('47'),  'octal too short (2 digits)');

  // --- Unicode \uXXXX ---
  RunCase(AnsiString('\u0041'), AnsiString('A'), 'unicode A');
  RunCase(AnsiString('\u0061'), AnsiString('a'), 'unicode a');
  RunCase(AnsiString('\u0000'), AnsiString(#0),  'unicode NUL');
  // Mixed case hex digits
  RunCase(AnsiString('\u00a9'), FromCP32($00A9), 'unicode U+00A9 © (lower hex)');
  RunCase(AnsiString('\u00A9'), FromCP32($00A9), 'unicode U+00A9 © (upper hex)');

  // Too short / malformed \u
  RunCase(AnsiString('\u'),     AnsiString('u'),   'unicode too short (just \u)');
  RunCase(AnsiString('\u1'),    AnsiString('u1'),  'unicode too short (1 digit)');
  RunCase(AnsiString('\u12'),   AnsiString('u12'), 'unicode too short (2 digits)');
  RunCase(AnsiString('\u12Z4'), AnsiString('u12Z4'),'unicode invalid digit');

  // --- Surrogate pairs ---
  // 😀 U+1F600 = \uD83D\uDE00
  RunCase(AnsiString('\uD83D\uDE00'), FromCP32($1F600), 'surrogate pair: 😀');
  // 🚀 U+1F680
  RunCase(AnsiString('\uD83D\uDE80'), FromCP32($1F680), 'surrogate pair: 🚀');

  // Lone surrogates → backslash is deleted, hex sequence stays (as per implementation)
  RunCase(AnsiString('\uD800'), AnsiString('uD800'), 'lone high surrogate');
  RunCase(AnsiString('\uDFFF'), AnsiString('uDFFF'), 'lone low surrogate');

  // High surrogate followed by something that isn't "\u" → backslash deleted for the first escape
  RunCase(AnsiString('\uD83D X'), AnsiString('uD83D X'), 'high surrogate not followed by \u');

  // --- Mixed sequences ---
  InS  := AnsiString('Line1\na\tb\r"q"\s\ \x41\u0042\143\u0044 \\ \/ end');
  ExpS := AnsiString('Line1' + #10 + 'a' + #9 + 'b' + #13 + '"q"' + 's' + ' ' + 'A' + 'B' + 'c' + 'D' + ' ' + '\' + ' ' + '/' + ' end');
  RunCase(InS, ExpS, 'mixed sequences');

  // Embedded #0
  InS  := AnsiString('A') + AnsiChar(#0) + AnsiString('\nB');
  ExpS := AnsiString('A') + AnsiChar(#0) + AnsiString(#10'B');
  RunCase(InS, ExpS, 'embedded #0');

  // Unknown escape → backslash dropped, keep char
  RunCase(AnsiString('\q'), AnsiString('q'), 'unknown escape \q');

  // & via unicode
  RunCase(AnsiString('\u0026'), AnsiString('&'), 'unicode ampersand');

  // Idempotence: decoding again should be a no-op
  InS := AnsiString('X\u0041\101\x42\uD83D\uDE00');
  ALJavascriptDecodeInPlace(InS);
  ExpS := AnsiString('XAAB😀');
  RunCase(InS, ExpS, 'idempotence first pass');
  // second pass
  RunCase(ExpS, ExpS, 'idempotence second pass');

  // Input: AnsiString('\')
  RunCase(AnsiString('AbC\'), AnsiString('AbC'), 'single trailing');
  RunCase(AnsiString('\'), AnsiString(''), 'single trailing');

  // --- Cursor-advance sanity checks for ISO-8859-1 paths ---
  RunCase(AnsiString('\x4Z'), AnsiString('x4Z'), 'incomplete hex escape');
  RunCase(AnsiString('\x41Y'),  AnsiString('AY'), 'hex path advance check');
  RunCase(AnsiString('\141Y'),  AnsiString('aY'), 'octal path advance check');
end;

{************************************************}
procedure TALDUnitXTestHtml.TestJavascriptDecodeW;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure RunCase(const Input, Expected: String; const Msg: string);
  begin
    var S := Input;
    ALJavascriptDecodeInPlace(S);
    if S <> Expected then
      Assert.Fail(
        'ALJavascriptDecodeInPlace: ' + Msg +
        ' | Expected="' + string(Expected) +
        '" | Actual="'   + string(S) + '"');
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function FromCP32(Code: Cardinal): String;
  begin
    // Mirror the function’s path: UTF-32 -> UTF-16 -> String via system ACP.
    Result := String(Char.ConvertFromUtf32(Code));
  end;

var
  InS, ExpS: String;

begin
  // --- Plain / empty ---
  RunCase(String(''), String(''), 'empty');
  RunCase(String('Hello, world!'), String('Hello, world!'), 'plain');

  // --- Simple JS escapes ---
  RunCase(String('\b'), String(#8),  'backspace');
  RunCase(String('\t'), String(#9),  'tab');
  RunCase(String('\n'), String(#10), 'newline');
  RunCase(String('\v'), String(#11), 'vertical tab');
  RunCase(String('\f'), String(#12), 'form feed');
  RunCase(String('\r'), String(#13), 'carriage return');
  RunCase(String('\"'), String(#34), 'double quote');
  RunCase(String('\/'), String('/'), 'forward slash (unknown escape → keep char, drop backslash)');
  // \' cannot be written directly; build it:
  RunCase(String('\') + Char(#39), String(#39), 'single quote');
  RunCase(String('\\'), String('\'), 'backslash');

  // --- Hex ISO-8859-1: \xXX ---
  RunCase(String('\x41'), String('A'), 'hex A');
  RunCase(String('\x61'), String('a'), 'hex a');
  RunCase(String('\x00'), String(#0),  'hex NUL');
  RunCase(String('\xFF'), String('ÿ'), 'hex 0xFF');

  // Invalid hex: not both digits -> drop backslash, keep the rest
  RunCase(String('\xG0'), String('xG0'), 'hex invalid (G0)');
  RunCase(String('\x0G'), String('x0G'), 'hex invalid (0G)');
  // Too short: "\x" or "\xA" → drop backslash, keep 'x' (and the rest)
  RunCase(String('\x'),   String('x'),   'hex too short (just \x)');
  RunCase(String('\xA'),  String('xA'),  'hex too short (one digit)');

  // --- Octal ISO-8859-1: \ooo (exactly 3 octal digits) ---
  RunCase(String('\141'), String('a'), 'octal 141 = a');
  RunCase(String('\101'), String('A'), 'octal 101 = A');
  RunCase(String('\000'), String(#0),  'octal NUL');
  RunCase(String('\377'), String('ÿ'), 'octal 377 = 255');

  // Invalid/out of range: \400 (=256) → delete backslash only
  RunCase(String('\400'), String('400'), 'octal out-of-range 400');
  // Not 3 digits → not matched as octal; drop backslash and keep first char
  RunCase(String('\47'),  String('47'),  'octal too short (2 digits)');

  // --- Unicode \uXXXX ---
  RunCase(String('\u0041'), String('A'), 'unicode A');
  RunCase(String('\u0061'), String('a'), 'unicode a');
  RunCase(String('\u0000'), String(#0),  'unicode NUL');
  // Mixed case hex digits
  RunCase(String('\u00a9'), FromCP32($00A9), 'unicode U+00A9 © (lower hex)');
  RunCase(String('\u00A9'), FromCP32($00A9), 'unicode U+00A9 © (upper hex)');

  // Too short / malformed \u
  RunCase(String('\u'),     String('u'),   'unicode too short (just \u)');
  RunCase(String('\u1'),    String('u1'),  'unicode too short (1 digit)');
  RunCase(String('\u12'),   String('u12'), 'unicode too short (2 digits)');
  RunCase(String('\u12Z4'), String('u12Z4'),'unicode invalid digit');

  // --- Surrogate pairs ---
  // 😀 U+1F600 = \uD83D\uDE00
  RunCase(String('\uD83D\uDE00'), FromCP32($1F600), 'surrogate pair: 😀');
  // 🚀 U+1F680
  RunCase(String('\uD83D\uDE80'), FromCP32($1F680), 'surrogate pair: 🚀');

  // Lone surrogates → backslash is deleted, hex sequence stays (as per implementation)
  RunCase(String('\uD800'), String('uD800'), 'lone high surrogate');
  RunCase(String('\uDFFF'), String('uDFFF'), 'lone low surrogate');

  // High surrogate followed by something that isn't "\u" → backslash deleted for the first escape
  RunCase(String('\uD83D X'), String('uD83D X'), 'high surrogate not followed by \u');

  // --- Mixed sequences ---
  InS  := String('Line1\na\tb\r"q"\s\ \x41\u0042\143\u0044 \\ \/ end');
  ExpS := String('Line1' + #10 + 'a' + #9 + 'b' + #13 + '"q"' + 's' + ' ' + 'A' + 'B' + 'c' + 'D' + ' ' + '\' + ' ' + '/' + ' end');
  RunCase(InS, ExpS, 'mixed sequences');

  // Embedded #0
  InS  := String('A') + Char(#0) + String('\nB');
  ExpS := String('A') + Char(#0) + String(#10'B');
  RunCase(InS, ExpS, 'embedded #0');

  // Unknown escape → backslash dropped, keep char
  RunCase(String('\q'), String('q'), 'unknown escape \q');

  // & via unicode
  RunCase(String('\u0026'), String('&'), 'unicode ampersand');

  // Idempotence: decoding again should be a no-op
  InS := String('X\u0041\101\x42\uD83D\uDE00');
  ALJavascriptDecodeInPlace(InS);
  ExpS := String('XAAB😀');
  RunCase(InS, ExpS, 'idempotence first pass');
  // second pass
  RunCase(ExpS, ExpS, 'idempotence second pass');

  // Input: String('\')
  RunCase(String('AbC\'), String('AbC'), 'single trailing');
  RunCase(String('\'), String(''), 'single trailing');

  // --- Cursor-advance sanity checks for ISO-8859-1 paths ---
  RunCase(String('\x4Z'), String('x4Z'), 'incomplete hex escape');
  RunCase(String('\x41Y'),  String('AY'), 'hex path advance check');
  RunCase(String('\141Y'),  String('aY'), 'octal path advance check');
end;

initialization
  TDUnitX.RegisterTestFixture(TALDUnitXTestHtml);

end.
