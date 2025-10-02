unit ALDUnitXTestUrl;

interface

uses
  DUnitX.TestFramework;

type

  [TestFixture]
  TALDUnitXTestUrl = class
  private
  public
    //constructor create;
    //[Setup]
    //procedure Setup;
    //[TearDown]
    //procedure TearDown;
    [Test]
    procedure TestALCookedUrlA;
    [Test]
    procedure TestALCombineUrlA;
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

{******************************************}
procedure TALDUnitXTestUrl.TestALCookedUrlA;
var
  CookedUrl: TALCookedUrlA;
begin
  // Test case 1: Full URL with all components, decoded
  CookedUrl := TALCookedUrlA.Create('https://user:pass@www.example.com:8080/path/to/resource?param1=value1&param2=value2#anchor', True);
  try
    Assert.AreEqual<AnsiString>('https', CookedUrl.Scheme, 'Scheme should be https');
    Assert.AreEqual<AnsiString>('www.example.com', CookedUrl.Host, 'Host should be www.example.com');
    Assert.AreEqual(8080, CookedUrl.Port, 'Port should be 8080');
    Assert.AreEqual<AnsiString>('user', CookedUrl.UserName, 'Username should be user');
    Assert.AreEqual<AnsiString>('pass', CookedUrl.Password, 'Password should be pass');
    Assert.AreEqual<AnsiString>('/path/to/resource', CookedUrl.Path, 'Path should be /path/to/resource');
    Assert.AreEqual<AnsiString>('param1=value1&param2=value2', CookedUrl.QueryString, 'QueryString should be param1=value1&param2=value2');
    Assert.IsNotNull(CookedUrl.QueryParams, 'QueryParams should not be nil');
    Assert.AreEqual(2, CookedUrl.QueryParams.Count, 'QueryParams should have 2 items');
    Assert.AreEqual<AnsiString>('value1', CookedUrl.QueryParams.Values['param1'], 'QueryParams param1 should be value1');
    Assert.AreEqual<AnsiString>('value2', CookedUrl.QueryParams.Values['param2'], 'QueryParams param2 should be value2');
    Assert.AreEqual<AnsiString>('anchor', CookedUrl.Anchor, 'Anchor should be anchor');
    Assert.AreEqual<AnsiString>('https://user:pass@www.example.com:8080/path/to/resource?param1=value1&param2=value2#anchor', CookedUrl.GetFullUrl(True), 'FullUrl should match input');
  finally
    CookedUrl.Free;
  end;

  // Test case 2: Simple URL, no decoding
  CookedUrl := TALCookedUrlA.Create('http://example.com', False);
  try
    Assert.AreEqual<AnsiString>('http', CookedUrl.Scheme, 'Scheme should be http');
    Assert.AreEqual<AnsiString>('example.com', CookedUrl.Host, 'Host should be example.com');
    Assert.AreEqual(80, CookedUrl.Port, 'Port should be 0');
    Assert.AreEqual<AnsiString>('', CookedUrl.UserName, 'Username should be empty');
    Assert.AreEqual<AnsiString>('', CookedUrl.Password, 'Password should be empty');
    Assert.AreEqual<AnsiString>('', CookedUrl.Path, 'Path should be empty');
    Assert.AreEqual<AnsiString>('', CookedUrl.QueryString, 'QueryString should be empty');
    Assert.IsNotNull(CookedUrl.QueryParams, 'QueryParams should not be nil');
    Assert.AreEqual(0, CookedUrl.QueryParams.Count, 'QueryParams should be empty');
    Assert.AreEqual<AnsiString>('', CookedUrl.Anchor, 'Anchor should be empty');
    Assert.AreEqual<AnsiString>('http://example.com', CookedUrl.GetFullUrl(False), 'FullUrl should match input');
  finally
    CookedUrl.Free;
  end;

  // Test case 3: URL with encoded characters, decoded
  CookedUrl := TALCookedUrlA.Create('https://example.com/path%20with%20spaces?param=val%2Bue', True);
  try
    Assert.AreEqual<AnsiString>('https', CookedUrl.Scheme, 'Scheme should be https');
    Assert.AreEqual<AnsiString>('example.com', CookedUrl.Host, 'Host should be example.com');
    Assert.AreEqual<AnsiString>('/path with spaces', CookedUrl.Path, 'Path should be decoded');
    Assert.AreEqual<AnsiString>('param=val%2Bue', CookedUrl.QueryString, 'QueryString should not be decoded');
    Assert.AreEqual<AnsiString>('val+ue', CookedUrl.QueryParams.Values['param'], 'QueryParams param should be decoded');
  finally
    CookedUrl.Free;
  end;

  // Test case 4: Empty URL
  CookedUrl := TALCookedUrlA.Create('', False);
  try
    Assert.AreEqual<AnsiString>('', CookedUrl.Scheme, 'Scheme should be empty');
    Assert.AreEqual<AnsiString>('', CookedUrl.Host, 'Host should be empty');
    Assert.AreEqual(0, CookedUrl.Port, 'Port should be 0');
    Assert.AreEqual<AnsiString>('', CookedUrl.Path, 'Path should be empty');
    Assert.AreEqual<AnsiString>('', CookedUrl.QueryString, 'QueryString should be empty');
    Assert.IsNotNull(CookedUrl.QueryParams, 'QueryParams should not be nil');
    Assert.AreEqual(0, CookedUrl.QueryParams.Count, 'QueryParams should be empty');
    Assert.AreEqual<AnsiString>('', CookedUrl.GetFullUrl(False), 'FullUrl should be empty');
  finally
    CookedUrl.Free;
  end;

// Test case 5: Explicit default HTTP port (80) + root slash
  CookedUrl := TALCookedUrlA.Create('http://example.com:80/', False);
  try
    Assert.AreEqual<AnsiString>('http', CookedUrl.Scheme);
    Assert.AreEqual<AnsiString>('example.com', CookedUrl.Host);
    Assert.AreEqual(80, CookedUrl.Port);
    Assert.AreEqual<AnsiString>('/', CookedUrl.Path);
    Assert.AreEqual<AnsiString>('', CookedUrl.QueryString);
    Assert.AreEqual<AnsiString>('', CookedUrl.Anchor);
    // Encoding true should keep structure; omit spaces test
    Assert.AreEqual<AnsiString>('http://example.com/', CookedUrl.GetFullUrl(True));
  finally
    CookedUrl.Free;
  end;

  // Test case 6: Explicit default HTTPS port (443) + no trailing slash
  CookedUrl := TALCookedUrlA.Create('https://example.com:443', False);
  try
    Assert.AreEqual<AnsiString>('https', CookedUrl.Scheme);
    Assert.AreEqual<AnsiString>('example.com', CookedUrl.Host);
    Assert.AreEqual(443, CookedUrl.Port);
    Assert.AreEqual<AnsiString>('', CookedUrl.Path);
    Assert.AreEqual<AnsiString>('', CookedUrl.QueryString);
    Assert.AreEqual<AnsiString>('', CookedUrl.Anchor);
    Assert.AreEqual<AnsiString>('https://example.com', CookedUrl.GetFullUrl(True));
  finally
    CookedUrl.Free;
  end;

  // Test case 7: IPv4 with non-default port and query
  CookedUrl := TALCookedUrlA.Create('http://192.0.2.10:8081/api?v=42', True);
  try
    Assert.AreEqual<AnsiString>('http', CookedUrl.Scheme);
    Assert.AreEqual<AnsiString>('192.0.2.10', CookedUrl.Host);
    Assert.AreEqual(8081, CookedUrl.Port);
    Assert.AreEqual<AnsiString>('/api', CookedUrl.Path);
    Assert.AreEqual<AnsiString>('v=42', CookedUrl.QueryString);
    Assert.AreEqual<AnsiString>('42', CookedUrl.QueryParams.Values['v']);
    Assert.AreEqual<AnsiString>('http://192.0.2.10:8081/api?v=42', CookedUrl.GetFullUrl(True));
  finally
    CookedUrl.Free;
  end;

  // Test case 8: IPv6 basic (no port)
  CookedUrl := TALCookedUrlA.Create('http://[2001:db8::1]/status', False);
  try
    Assert.AreEqual<AnsiString>('http', CookedUrl.Scheme);
    Assert.AreEqual<AnsiString>('[2001:db8::1]', CookedUrl.Host); // no brackets stored
    Assert.AreEqual(80, CookedUrl.Port);
    Assert.AreEqual<AnsiString>('/status', CookedUrl.Path);
    Assert.AreEqual<AnsiString>('http://[2001:db8::1]/status', CookedUrl.GetFullUrl(false));
  finally
    CookedUrl.Free;
  end;

  // Test case 9: IPv6 with port and fragment
  CookedUrl := TALCookedUrlA.Create('https://[2001:db8::1]:8443/api#frag', True);
  try
    Assert.AreEqual<AnsiString>('https', CookedUrl.Scheme);
    Assert.AreEqual<AnsiString>('[2001:db8::1]', CookedUrl.Host);
    Assert.AreEqual(8443, CookedUrl.Port);
    Assert.AreEqual<AnsiString>('/api', CookedUrl.Path);
    Assert.AreEqual<AnsiString>('frag', CookedUrl.Anchor);
    Assert.AreEqual<AnsiString>('https://[2001:db8::1]:8443/api#frag', CookedUrl.GetFullUrl(True));
  finally
    CookedUrl.Free;
  end;

  // Test case 10: IPv6 with zone id (%25 encoded) — keep as-is
  CookedUrl := TALCookedUrlA.Create('http://[fe80::1%25eth0]/health', True);
  try
    Assert.AreEqual<AnsiString>('http', CookedUrl.Scheme);
    Assert.AreEqual<AnsiString>('[fe80::1%25eth0]', CookedUrl.Host);
    Assert.AreEqual(80, CookedUrl.Port);
    Assert.AreEqual<AnsiString>('/health', CookedUrl.Path);
    Assert.AreEqual<AnsiString>('http://[fe80::1%25eth0]/health', CookedUrl.GetFullUrl(True));
  finally
    CookedUrl.Free;
  end;

  // Test case 11: Userinfo w/o password
  CookedUrl := TALCookedUrlA.Create('https://user@example.com/', True);
  try
    Assert.AreEqual<AnsiString>('user', CookedUrl.UserName);
    Assert.AreEqual<AnsiString>('', CookedUrl.Password);
    Assert.AreEqual<AnsiString>('/', CookedUrl.Path);
  finally
    CookedUrl.Free;
  end;

  // Test case 12: Userinfo with encoded '@' and ':' in credentials
  CookedUrl := TALCookedUrlA.Create('https://us%40er:p%3Ass@secure.example.com/', True);
  try
    Assert.AreEqual<AnsiString>('us@er', CookedUrl.UserName);
    Assert.AreEqual<AnsiString>('p:ss', CookedUrl.Password);
    Assert.AreEqual<AnsiString>('secure.example.com', CookedUrl.Host);
  finally
    CookedUrl.Free;
  end;

  // Test case 13: Path with spaces (encoded), decoded=True
  CookedUrl := TALCookedUrlA.Create('https://example.com/files/My%20Doc.txt', True);
  try
    Assert.AreEqual<AnsiString>('/files/My Doc.txt', CookedUrl.Path);
    Assert.AreEqual<AnsiString>('https://example.com/files/My%20Doc.txt', CookedUrl.GetFullUrl(True));
  finally
    CookedUrl.Free;
  end;

  // Test case 14: Path with literal plus (should remain '+')
  CookedUrl := TALCookedUrlA.Create('https://example.com/a+b', True);
  try
    Assert.AreEqual<AnsiString>('/a+b', CookedUrl.Path);
    Assert.AreEqual<AnsiString>('https://example.com/a%2Bb', CookedUrl.GetFullUrl(True));
  finally
    CookedUrl.Free;
  end;

  // Test case 15: Query empty (present but empty)
  CookedUrl := TALCookedUrlA.Create('https://example.com/path?', False);
  try
    Assert.AreEqual<AnsiString>('/path', CookedUrl.Path);
    Assert.AreEqual<AnsiString>('', CookedUrl.QueryString);
    Assert.AreEqual(0, CookedUrl.QueryParams.Count);
    Assert.AreEqual<AnsiString>('https://example.com/path', CookedUrl.GetFullUrl(True)); // no trailing '?'
  finally
    CookedUrl.Free;
  end;

  // Test case 16: Flag-only query param (no '=')
  CookedUrl := TALCookedUrlA.Create('https://example.com/p?flag', True);
  try
    Assert.AreEqual<AnsiString>('flag', CookedUrl.QueryString);
    Assert.AreEqual(1, CookedUrl.QueryParams.Count);
    Assert.AreEqual<AnsiString>('', CookedUrl.QueryParams.Values['flag']);
  finally
    CookedUrl.Free;
  end;

  // Test case 17: Repeated keys
  CookedUrl := TALCookedUrlA.Create('https://example.com/p?tag=red&tag=blue&tag=green', True);
  try
    Assert.AreEqual<AnsiString>('tag=red&tag=blue&tag=green', CookedUrl.QueryString);
    Assert.IsTrue(CookedUrl.QueryParams.Count >= 3);
  finally
    CookedUrl.Free;
  end;

  // Test case 18: Plus-as-spaces = True; 'a+b' becomes 'a b' in QueryParams only
  CookedUrl := TALCookedUrlA.Create('https://example.com/q?x=a+b&y=7+8', True{Decode}, True{PlusAsSpaces});
  try
    Assert.AreEqual<AnsiString>('x=a+b&y=7+8', CookedUrl.QueryString); // raw stays with '+'
    Assert.AreEqual<AnsiString>('a b', CookedUrl.QueryParams.Values['x']);
    Assert.AreEqual<AnsiString>('7 8', CookedUrl.QueryParams.Values['y']);
  finally
    CookedUrl.Free;
  end;

  // Test case 19: Plus-as-spaces = False; '+' should remain '+'
  CookedUrl := TALCookedUrlA.Create('https://example.com/q?x=a+b', True{Decode}, False{PlusAsSpaces});
  try
    Assert.AreEqual<AnsiString>('a+b', CookedUrl.QueryParams.Values['x']);
  finally
    CookedUrl.Free;
  end;

  // Test case 20: %2B in query should decode to literal '+', even with PlusAsSpaces=True
  CookedUrl := TALCookedUrlA.Create('https://example.com/q?param=val%2Bue', True{Decode}, True{PlusAsSpaces});
  try
    Assert.AreEqual<AnsiString>('val+ue', CookedUrl.QueryParams.Values['param']); // remains '+'
  finally
    CookedUrl.Free;
  end;

  // Test case 21: UTF-8 percent-encoded in query (é)
  CookedUrl := TALCookedUrlA.Create('https://example.com/q?name=St%C3%A9phane', True);
  try
    Assert.AreEqual<AnsiString>('Stéphane', CookedUrl.QueryParams.Values['name']);
  finally
    CookedUrl.Free;
  end;

  // Test case 22: Fragment only
  CookedUrl := TALCookedUrlA.Create('https://example.com/doc#section-2', True);
  try
    Assert.AreEqual<AnsiString>('section-2', CookedUrl.Anchor);
    Assert.AreEqual<AnsiString>('https://example.com/doc#section-2', CookedUrl.GetFullUrl(True));
  finally
    CookedUrl.Free;
  end;

  // Test case 23: Empty fragment (present but empty)
  CookedUrl := TALCookedUrlA.Create('https://example.com/#', False);
  try
    Assert.AreEqual<AnsiString>('/', CookedUrl.Path);
    Assert.AreEqual<AnsiString>('', CookedUrl.Anchor);
    Assert.AreEqual<AnsiString>('https://example.com/', CookedUrl.GetFullUrl(True));
  finally
    CookedUrl.Free;
  end;

  // Test case 24: Fragment with encoded '#'
  CookedUrl := TALCookedUrlA.Create('https://example.com/p#hash%23inside', True);
  try
    Assert.AreEqual<AnsiString>('hash#inside', CookedUrl.Anchor);
  finally
    CookedUrl.Free;
  end;

  // Test case 25: Trailing dot host
  CookedUrl := TALCookedUrlA.Create('http://example.com./', False);
  try
    Assert.AreEqual<AnsiString>('example.com.', CookedUrl.Host);
    Assert.AreEqual<AnsiString>('/', CookedUrl.Path);
  finally
    CookedUrl.Free;
  end;

  // Test case 26: Query with empty value
  CookedUrl := TALCookedUrlA.Create('https://example.com/p?empty=', True);
  try
    Assert.AreEqual<AnsiString>('empty=', CookedUrl.QueryString);
    Assert.AreEqual<AnsiString>('', CookedUrl.QueryParams.Values['empty']);
  finally
    CookedUrl.Free;
  end;

  // Test case 27: Query with empty name (=value)
  CookedUrl := TALCookedUrlA.Create('https://example.com/p?=value', True);
  try
    Assert.AreEqual<AnsiString>('=value', CookedUrl.QueryString);
    // name is empty; behavior may vary — just ensure parsing doesn't crash
    Assert.IsTrue(CookedUrl.QueryParams.Count >= 1);
  finally
    CookedUrl.Free;
  end;

  // Test case 28: Multiple params incl. empty and flag
  CookedUrl := TALCookedUrlA.Create('https://example.com/p?a=1&&b=&flag', True);
  try
    Assert.AreEqual<AnsiString>('a=1&&b=&flag', CookedUrl.QueryString);
    Assert.AreEqual<AnsiString>('1', CookedUrl.QueryParams.Values['a']);
    Assert.AreEqual<AnsiString>('', CookedUrl.QueryParams.Values['b']);
  finally
    CookedUrl.Free;
  end;

  // Test case 29: Username/password with '+', ensure not turned into space
  CookedUrl := TALCookedUrlA.Create('https://user+name:pa+ss@example.com/', True{Decode}, True{PlusAsSpaces});
  try
    Assert.AreEqual<AnsiString>('user+name', CookedUrl.UserName);
    Assert.AreEqual<AnsiString>('pa+ss', CookedUrl.Password);
  finally
    CookedUrl.Free;
  end;

  // Test case 30: Path with percent-encoded reserved chars
  CookedUrl := TALCookedUrlA.Create('https://example.com/%5Bfile%5D%20v1.0', True);
  try
    Assert.AreEqual<AnsiString>('/[file] v1.0', CookedUrl.Path);
  finally
    CookedUrl.Free;
  end;

  // Test case 31: Build URL with encoding off (should keep raw)
  CookedUrl := TALCookedUrlA.Create('https://example.com/p?a=1+2', False);
  try
    Assert.AreEqual<AnsiString>('a=1+2', CookedUrl.QueryString);
    Assert.AreEqual<AnsiString>('https://example.com/p?a=1+2', CookedUrl.GetFullUrl(False)); // no extra encoding
  finally
    CookedUrl.Free;
  end;

  // Test case 32: Build URL with encoding on, using '+' for spaces in query
  CookedUrl := TALCookedUrlA.Create('https://example.com', True);
  try
    CookedUrl.Path := '/search';
    CookedUrl.QueryString := 'q=a b';
    Assert.AreEqual<AnsiString>('https://example.com/search?q=a+b', CookedUrl.GetFullUrl(True{Encode}, True{SpacesAsPlus}));
  finally
    CookedUrl.Free;
  end;

  // Test case 33: Build URL with encoding on, using %20 for spaces in query
  CookedUrl := TALCookedUrlA.Create('https://example.com', True);
  try
    CookedUrl.Path := '/search';
    CookedUrl.QueryString := 'q=a b';
    Assert.AreEqual<AnsiString>('https://example.com/search?q=a%20b', CookedUrl.GetFullUrl(True{Encode}, False{SpacesAsPlus}));
  finally
    CookedUrl.Free;
  end;

  // Test case 34: No scheme/host changes on clear
  CookedUrl := TALCookedUrlA.Create('http://example.com/path?x=1#f', True);
  try
    CookedUrl.Clear;
    Assert.AreEqual<AnsiString>('', CookedUrl.Scheme);
    Assert.AreEqual<AnsiString>('', CookedUrl.Host);
    Assert.AreEqual(0, CookedUrl.Port);
    Assert.AreEqual<AnsiString>('', CookedUrl.Path);
    Assert.AreEqual<AnsiString>('', CookedUrl.QueryString);
    Assert.AreEqual<AnsiString>('', CookedUrl.Anchor);
  finally
    CookedUrl.Free;
  end;

  // Test case 35: Username with percent-encoded space
  CookedUrl := TALCookedUrlA.Create('https://us%20er:pwd@example.com/', True);
  try
    Assert.AreEqual<AnsiString>('us er', CookedUrl.UserName);
  finally
    CookedUrl.Free;
  end;

  // Test case 36: Password with percent-encoded plus (%2B -> '+')
  CookedUrl := TALCookedUrlA.Create('https://user:p%2Bss@example.com/', True, True);
  try
    Assert.AreEqual<AnsiString>('p+ss', CookedUrl.Password);
  finally
    CookedUrl.Free;
  end;

  // Test case 37: Query with encoded '=' in value
  CookedUrl := TALCookedUrlA.Create('https://example.com/p?tok=a%3Db%3D', True);
  try
    Assert.AreEqual<AnsiString>('a=b=', CookedUrl.QueryParams.Values['tok']);
  finally
    CookedUrl.Free;
  end;

  // Test case 38: Mixed case scheme/host normalized (if you normalize)
  CookedUrl := TALCookedUrlA.Create('HTTP://ExAmPlE.Com/Path', True);
  try
    Assert.AreEqual<AnsiString>('HTTP', CookedUrl.Scheme);
    Assert.AreEqual<AnsiString>('ExAmPlE.Com', CookedUrl.Host);
    Assert.AreEqual<AnsiString>('/Path', CookedUrl.Path);
  finally
    CookedUrl.Free;
  end;

  // Test case 39: Query key with '+' encoded (%2B) and space separate
  CookedUrl := TALCookedUrlA.Create('https://example.com/p?x%2By=7%2B8', True, True);
  try
    Assert.AreEqual<AnsiString>('7+8', CookedUrl.QueryParams.Values['x+y']);
  finally
    CookedUrl.Free;
  end;

  // Test case 40: Empty path (no '/')
  CookedUrl := TALCookedUrlA.Create('http://example.com?x=1', False);
  try
    Assert.AreEqual<AnsiString>('', CookedUrl.Path);
    Assert.AreEqual<AnsiString>('x=1', CookedUrl.QueryString);
  finally
    CookedUrl.Free;
  end;

  // Test case 41: Root path '/'
  CookedUrl := TALCookedUrlA.Create('http://example.com/?x=1', False);
  try
    Assert.AreEqual<AnsiString>('/', CookedUrl.Path);
    Assert.AreEqual<AnsiString>('x=1', CookedUrl.QueryString);
  finally
    CookedUrl.Free;
  end;

  // Test case 42: Complex query; ensure no crash and correct counts
  CookedUrl := TALCookedUrlA.Create('https://example.com/p?a=1&b=2&c=&d&&e=5#top', True);
  try
    Assert.AreEqual<AnsiString>('top', CookedUrl.Anchor);
    Assert.IsTrue(CookedUrl.QueryParams.Count >= 4);
  finally
    CookedUrl.Free;
  end;

  // Test case 43: Path with consecutive slashes (left as-is)
  CookedUrl := TALCookedUrlA.Create('https://example.com/a//b///c', False);
  try
    Assert.AreEqual<AnsiString>('/a//b///c', CookedUrl.Path);
  finally
    CookedUrl.Free;
  end;

  // Test case 44: Dot segments (no normalization expected)
  CookedUrl := TALCookedUrlA.Create('https://example.com/a/b/../c/.', False);
  try
    Assert.AreEqual<AnsiString>('/a/b/../c/.', CookedUrl.Path);
  finally
    CookedUrl.Free;
  end;

  // Test case 45: Query name only, then name=value
  CookedUrl := TALCookedUrlA.Create('https://example.com/p?flag&k=v', True);
  try
    Assert.AreEqual<AnsiString>('flag', CookedUrl.QueryParams.Names[0]);
    Assert.AreEqual<AnsiString>('k', CookedUrl.QueryParams.Names[1]);
    Assert.AreEqual<AnsiString>('v', CookedUrl.QueryParams.ValueFromIndex[1]);
  finally
    CookedUrl.Free;
  end;

  // Test case 46: Build with encoded path and fragment
  CookedUrl := TALCookedUrlA.Create('', True);
  try
    CookedUrl.Scheme := 'https';
    CookedUrl.Host := 'example.com';
    CookedUrl.Path := '/a path/with ü';
    CookedUrl.Anchor := 'frag with#hash';
    // Encoding on: path spaces/utf-8 must percent-encode; fragment '#' inside should be %23
    Assert.AreEqual<AnsiString>('https://example.com/a%20path/with%20%C3%BC#frag%20with%23hash',
      CookedUrl.GetFullUrl(True));
  finally
    CookedUrl.Free;
  end;

  // Test case 47: Non-default port with query + fragment
  CookedUrl := TALCookedUrlA.Create('https://example.com:4443/p?q=1#z', True);
  try
    Assert.AreEqual(4443, CookedUrl.Port);
    Assert.AreEqual<AnsiString>('q=1', CookedUrl.QueryString);
    Assert.AreEqual<AnsiString>('z', CookedUrl.Anchor);
  finally
    CookedUrl.Free;
  end;

  // Test case 48: Encoded slash in query value (%2F)
  CookedUrl := TALCookedUrlA.Create('https://example.com/p?redir=%2Fhome%2Findex', True);
  try
    Assert.AreEqual<AnsiString>('/home/index', CookedUrl.QueryParams.Values['redir']);
  finally
    CookedUrl.Free;
  end;

  // Test case 49: Encoded '?' in query value (%3F)
  CookedUrl := TALCookedUrlA.Create('https://example.com/p?x=a%3Fb', True);
  try
    Assert.AreEqual<AnsiString>('a?b', CookedUrl.QueryParams.Values['x']);
  finally
    CookedUrl.Free;
  end;

  // Test case 50: Encoded '#' in path
  CookedUrl := TALCookedUrlA.Create('https://example.com/p%23art', True);
  try
    Assert.AreEqual<AnsiString>('/p#art', CookedUrl.Path);
  finally
    CookedUrl.Free;
  end;

  // Test case 51: Encoded '@' in path
  CookedUrl := TALCookedUrlA.Create('https://example.com/user/%40me', True);
  try
    Assert.AreEqual<AnsiString>('/user/@me', CookedUrl.Path);
  finally
    CookedUrl.Free;
  end;

  // Test case 52: Query key contains '=' encoded
  CookedUrl := TALCookedUrlA.Create('https://example.com/p?k%3Dx=1', True);
  try
    Assert.AreEqual<AnsiString>('1', CookedUrl.QueryParams.Values['k=x']);
  finally
    CookedUrl.Free;
  end;

  // Test case 53: Query with '%20' (no '+'), keep raw; Values decode to space
  CookedUrl := TALCookedUrlA.Create('https://example.com/p?q=a%20b', True, True);
  try
    Assert.AreEqual<AnsiString>('q=a%20b', CookedUrl.QueryString);
    Assert.AreEqual<AnsiString>('a b', CookedUrl.QueryParams.Values['q']);
  finally
    CookedUrl.Free;
  end;

  // Test case 54: Build with SpacesAsPlus=False to force %20
  CookedUrl := TALCookedUrlA.Create('https://example.com', True);
  try
    CookedUrl.Path := '/search files';
    CookedUrl.QueryString := 'q=a b&x=1 2';
    Assert.AreEqual<AnsiString>('https://example.com/search%20files?q=a%20b&x=1%202',
      CookedUrl.GetFullUrl(True{Encode}, False{SpacesAsPlus}));
  finally
    CookedUrl.Free;
  end;

  // Test case 55: URL with no scheme, relative path
  CookedUrl := TALCookedUrlA.Create('example.com/path/to/file', False);
  try
    Assert.AreEqual<AnsiString>('', CookedUrl.Scheme);
    Assert.AreEqual<AnsiString>('', CookedUrl.Host);
    Assert.AreEqual<AnsiString>('example.com/path/to/file', CookedUrl.Path);
    Assert.AreEqual<AnsiString>('example.com/path/to/file', CookedUrl.GetFullUrl(False{Encode}));
  finally
    CookedUrl.Free;
  end;

  // Test case 56: URL with only scheme and host, no path
  CookedUrl := TALCookedUrlA.Create('ftp://ftp.example.com', False);
  try
    Assert.AreEqual<AnsiString>('ftp', CookedUrl.Scheme);
    Assert.AreEqual<AnsiString>('ftp.example.com', CookedUrl.Host);
    Assert.AreEqual(21, CookedUrl.Port);
    Assert.AreEqual<AnsiString>('', CookedUrl.Path);
    Assert.AreEqual<AnsiString>('ftp://ftp.example.com', CookedUrl.GetFullUrl(False{Encode}));
  finally
    CookedUrl.Free;
  end;

  // Test case 57: Query with multiple equal signs in value
  CookedUrl := TALCookedUrlA.Create('https://example.com/p?data=x=y=z', True);
  try
    Assert.AreEqual<AnsiString>('x=y=z', CookedUrl.QueryParams.Values['data']);
  finally
    CookedUrl.Free;
  end;

  // Test case 58: URL with encoded ampersand in query value
  CookedUrl := TALCookedUrlA.Create('https://example.com/p?pair=x%26y', True);
  try
    Assert.AreEqual<AnsiString>('x&y', CookedUrl.QueryParams.Values['pair']);
  finally
    CookedUrl.Free;
  end;

  // Test case 59: URL with port 0
  CookedUrl := TALCookedUrlA.Create('http://example.com:0/path', False);
  try
    Assert.AreEqual<AnsiString>('http', CookedUrl.Scheme);
    Assert.AreEqual<AnsiString>('example.com', CookedUrl.Host);
    Assert.AreEqual(0, CookedUrl.Port);
    Assert.AreEqual<AnsiString>('/path', CookedUrl.Path);
  finally
    CookedUrl.Free;
  end;

  // Test case 60: URL with encoded colon in path
  CookedUrl := TALCookedUrlA.Create('https://example.com/path%3Atest', True);
  try
    Assert.AreEqual<AnsiString>('/path:test', CookedUrl.Path);
  finally
    CookedUrl.Free;
  end;

  // Test case 61: Query with encoded space in key
  CookedUrl := TALCookedUrlA.Create('https://example.com/p?key%20name=value', True);
  try
    Assert.AreEqual<AnsiString>('value', CookedUrl.QueryParams.Values['key name']);
  finally
    CookedUrl.Free;
  end;

  // Test case 62: Fragment with encoded space
  CookedUrl := TALCookedUrlA.Create('https://example.com/p#section%20one', True);
  try
    Assert.AreEqual<AnsiString>('section one', CookedUrl.Anchor);
  finally
    CookedUrl.Free;
  end;

  // Test case 63: URL with multiple consecutive colons in userinfo
  CookedUrl := TALCookedUrlA.Create('https://user::pass@example.com/', True);
  try
    Assert.AreEqual<AnsiString>('user', CookedUrl.UserName);
    Assert.AreEqual<AnsiString>(':pass', CookedUrl.Password);
  finally
    CookedUrl.Free;
  end;

  // Test case 64: URL with empty userinfo
  CookedUrl := TALCookedUrlA.Create('https://@example.com/', True);
  try
    Assert.AreEqual<AnsiString>('', CookedUrl.UserName);
    Assert.AreEqual<AnsiString>('', CookedUrl.Password);
    Assert.AreEqual<AnsiString>('example.com', CookedUrl.Host);
  finally
    CookedUrl.Free;
  end;

  // Test case 65: URL with non-standard scheme
  CookedUrl := TALCookedUrlA.Create('ws://websocket.example.com:9000', False);
  try
    Assert.AreEqual<AnsiString>('ws', CookedUrl.Scheme);
    Assert.AreEqual<AnsiString>('websocket.example.com', CookedUrl.Host);
    Assert.AreEqual(9000, CookedUrl.Port);
  finally
    CookedUrl.Free;
  end;

  // Test case 66: URL with subdomain
  CookedUrl := TALCookedUrlA.Create('https://sub.sub.example.com/path', True);
  try
    Assert.AreEqual<AnsiString>('sub.sub.example.com', CookedUrl.Host);
    Assert.AreEqual<AnsiString>('/path', CookedUrl.Path);
  finally
    CookedUrl.Free;
  end;

  // Test case 67: Query with encoded percent sign
  CookedUrl := TALCookedUrlA.Create('https://example.com/p?val=x%25y', True);
  try
    Assert.AreEqual<AnsiString>('x%y', CookedUrl.QueryParams.Values['val']);
  finally
    CookedUrl.Free;
  end;

  // Test case 68: Path with encoded parentheses
  CookedUrl := TALCookedUrlA.Create('https://example.com/path%28test%29', True);
  try
    Assert.AreEqual<AnsiString>('/path(test)', CookedUrl.Path);
  finally
    CookedUrl.Free;
  end;

  // Test case 69: URL with empty path and query
  CookedUrl := TALCookedUrlA.Create('https://example.com?', True);
  try
    Assert.AreEqual<AnsiString>('', CookedUrl.Path);
    Assert.AreEqual<AnsiString>('', CookedUrl.QueryString);
    Assert.AreEqual<AnsiString>('https://example.com', CookedUrl.GetFullUrl(True{Encode}));
  finally
    CookedUrl.Free;
  end;

  // Test case 70: URL with encoded angle brackets in path
  CookedUrl := TALCookedUrlA.Create('https://example.com/%3Ctag%3E', True);
  try
    Assert.AreEqual<AnsiString>('/<tag>', CookedUrl.Path);
  finally
    CookedUrl.Free;
  end;

  // Test case 71: Query with multiple values for same key
  CookedUrl := TALCookedUrlA.Create('https://example.com/p?key=val1&key=val2', True);
  try
    Assert.AreEqual<AnsiString>('key=val1&key=val2', CookedUrl.QueryString);
    Assert.IsTrue(CookedUrl.QueryParams.Count >= 2);
  finally
    CookedUrl.Free;
  end;

  // Test case 72: URL with encoded semicolon in query
  CookedUrl := TALCookedUrlA.Create('https://example.com/p?sep=x%3By', True);
  try
    Assert.AreEqual<AnsiString>('x;y', CookedUrl.QueryParams.Values['sep']);
  finally
    CookedUrl.Free;
  end;

  // Test case 73: URL with trailing slashes
  CookedUrl := TALCookedUrlA.Create('https://example.com/path///', False);
  try
    Assert.AreEqual<AnsiString>('/path///', CookedUrl.Path);
  finally
    CookedUrl.Free;
  end;

  // Test case 74: URL with encoded comma in query
  CookedUrl := TALCookedUrlA.Create('https://example.com/p?list=a%2Cb', True);
  try
    Assert.AreEqual<AnsiString>('a,b', CookedUrl.QueryParams.Values['list']);
  finally
    CookedUrl.Free;
  end;

  // Test case 75: Build URL with non-ASCII characters
  CookedUrl := TALCookedUrlA.Create('', True);
  try
    CookedUrl.Scheme := 'https';
    CookedUrl.Host := 'example.com';
    CookedUrl.Path := '/café';
    Assert.AreEqual<AnsiString>('https://example.com/caf%C3%A9', CookedUrl.GetFullUrl(True{Encode}));
  finally
    CookedUrl.Free;
  end;

  // Test case 76: URL with encoded asterisk in path
  CookedUrl := TALCookedUrlA.Create('https://example.com/path%2Atest', True);
  try
    Assert.AreEqual<AnsiString>('/path*test', CookedUrl.Path);
  finally
    CookedUrl.Free;
  end;

  // Test case 77: URL with no host, only path
  CookedUrl := TALCookedUrlA.Create('/local/path', False);
  try
    Assert.AreEqual<AnsiString>('', CookedUrl.Scheme);
    Assert.AreEqual<AnsiString>('', CookedUrl.Host);
    Assert.AreEqual<AnsiString>('/local/path', CookedUrl.Path);
    Assert.AreEqual<AnsiString>('/local/path', CookedUrl.GetFullUrl(False{Encode}));
  finally
    CookedUrl.Free;
  end;

  // Test case 78: URL with encoded pipe in query
  CookedUrl := TALCookedUrlA.Create('https://example.com/p?opt=x%7Cy', True);
  try
    Assert.AreEqual<AnsiString>('x|y', CookedUrl.QueryParams.Values['opt']);
  finally
    CookedUrl.Free;
  end;

  // Test case 79: URL with multiple fragments (should take first)
  CookedUrl := TALCookedUrlA.Create('https://example.com/p#first#second', True);
  try
    Assert.AreEqual<AnsiString>('first#second', CookedUrl.Anchor);
  finally
    CookedUrl.Free;
  end;

  // Test case 80: URL with encoded plus in username
  CookedUrl := TALCookedUrlA.Create('https://user%2Bname:pwd@example.com', True);
  try
    Assert.AreEqual<AnsiString>('user+name', CookedUrl.UserName);
  finally
    CookedUrl.Free;
  end;

  // Test case 80: URL with encoded plus in username
  CookedUrl := TALCookedUrlA.Create('http://example.com#anchor?query', false);
  try
    Assert.AreEqual<AnsiString>('anchor?query', CookedUrl.anchor);
  finally
    CookedUrl.Free;
  end;

  // Test case 80: URL with encoded plus in username
  CookedUrl := TALCookedUrlA.Create('www.example.com#anchor?query#ftp://www.yahoo.fr', false);
  try
    Assert.AreEqual<AnsiString>('', CookedUrl.scheme);
    Assert.AreEqual<AnsiString>('anchor?query#ftp://www.yahoo.fr', CookedUrl.anchor);
  finally
    CookedUrl.Free;
  end;

  // Test case 1: Full URL with all components, decoded
  CookedUrl := TALCookedUrlA.Create('./path/to/resource?param1=value1&param2=value2#anchor', True);
  try
    Assert.AreEqual<AnsiString>('', CookedUrl.Scheme);
    Assert.AreEqual<AnsiString>('', CookedUrl.Host);
    Assert.AreEqual(0, CookedUrl.Port);
    Assert.AreEqual<AnsiString>('', CookedUrl.UserName);
    Assert.AreEqual<AnsiString>('', CookedUrl.Password);
    Assert.AreEqual<AnsiString>('./path/to/resource', CookedUrl.Path, 'Path should be ./path/to/resource');
    Assert.AreEqual<AnsiString>('param1=value1&param2=value2', CookedUrl.QueryString, 'QueryString should be param1=value1&param2=value2');
    Assert.IsNotNull(CookedUrl.QueryParams, 'QueryParams should not be nil');
    Assert.AreEqual(2, CookedUrl.QueryParams.Count, 'QueryParams should have 2 items');
    Assert.AreEqual<AnsiString>('value1', CookedUrl.QueryParams.Values['param1'], 'QueryParams param1 should be value1');
    Assert.AreEqual<AnsiString>('value2', CookedUrl.QueryParams.Values['param2'], 'QueryParams param2 should be value2');
    Assert.AreEqual<AnsiString>('anchor', CookedUrl.Anchor, 'Anchor should be anchor');
    Assert.AreEqual<AnsiString>('./path/to/resource?param1=value1&param2=value2#anchor', CookedUrl.GetFullUrl(True), 'FullUrl should match input');
  finally
    CookedUrl.Free;
  end;

end;

{*******************************************}
procedure TALDUnitXTestUrl.TestALCombineUrlA;

  // Helper procedure to create and parse result URL
  procedure CheckUrl(
              const AResult: AnsiString; const AExpected: AnsiString;
              const AScheme, AHost, APath, AQuery, AAnchor: AnsiString;
              APort: Integer);
  begin
    Assert.AreEqual(AExpected, AResult, 'Full URL mismatch');
    var CookedUrl := TALCookedUrlA.Create(AResult, True{Decode}, True{PlusAsSpaces});
    try
      Assert.AreEqual<AnsiString>(AScheme, CookedUrl.Scheme, 'Scheme mismatch');
      Assert.AreEqual<AnsiString>(AHost, CookedUrl.Host, 'Host mismatch');
      Assert.AreEqual(APort, CookedUrl.Port, 'Port mismatch');
      Assert.AreEqual<AnsiString>(APath, CookedUrl.Path, 'Path mismatch');
      Assert.AreEqual<AnsiString>(AQuery, CookedUrl.QueryString, 'QueryString mismatch');
      Assert.AreEqual<AnsiString>(AAnchor, CookedUrl.Anchor, 'Anchor mismatch');
    finally
      CookedUrl.Free;
    end;
  end;

begin
  {$IF defined(MSWindows)}
  var ResultUrl: AnsiString;

  // Test case 1: Absolute relative URL (should return unchanged)
  ResultUrl := AlCombineUrlA('https://example.com/path?key=value#anchor', 'http://base.com');
  CheckUrl(
    ResultUrl, 'https://example.com/path?key=value#anchor',
    'https', 'example.com', '/path', 'key=value', 'anchor', 443);

  // Test case 2: Relative path with base URL
  ResultUrl := AlCombineUrlA('sub/path', 'http://base.com/main/page');
  CheckUrl(
    ResultUrl, 'http://base.com/main/sub/path',
    'http', 'base.com', '/main/sub/path', '', '', 80);

  // Test case 3: Relative path with base URL ending in slash
  ResultUrl := AlCombineUrlA('sub/path', 'http://base.com/main/');
  CheckUrl(
    ResultUrl, 'http://base.com/main/sub/path',
    'http', 'base.com', '/main/sub/path', '', '', 80);

  // Test case 4: Relative path with parent directory (..)
  ResultUrl := AlCombineUrlA('../sub/path', 'http://base.com/main/folder/page');
  CheckUrl(
    ResultUrl, 'http://base.com/main/sub/path',
    'http', 'base.com', '/main/sub/path', '', '', 80);

  // Test case 5: Relative path with query and fragment
  ResultUrl := AlCombineUrlA('new?key=value#frag', 'http://base.com/path/');
  CheckUrl(
    ResultUrl, 'http://base.com/path/new?key=value#frag',
    'http', 'base.com', '/path/new', 'key=value', 'frag', 80);

  // Test case 6: Empty relative URL (should return base URL without fragment)
  ResultUrl := AlCombineUrlA('', 'http://base.com/path?key=value#anchor');
  CheckUrl(
    ResultUrl, 'http://base.com/',
    'http', 'base.com', '/', '', '', 80);

  // Test case 7: Root relative URL (starts with /)
  ResultUrl := AlCombineUrlA('/new/path', 'http://base.com/main/page');
  CheckUrl(
    ResultUrl, 'http://base.com/new/path',
    'http', 'base.com', '/new/path', '', '', 80);

  // Test case 8: Base URL with port and userinfo
  ResultUrl := AlCombineUrlA('sub', 'http://user:pass@base.com:8080/main/');
  CheckUrl(
    ResultUrl, 'http://user:pass@base.com:8080/main/sub',
    'http', 'base.com', '/main/sub', '', '', 8080);

  // Test case 9: Relative URL with encoded characters
  ResultUrl := AlCombineUrlA('sub%20path/file%2Bname', 'http://base.com/main/');
  CheckUrl(
    ResultUrl, 'http://base.com/main/sub%20path/file%2Bname',
    'http', 'base.com', '/main/sub path/file+name', '', '', 80);

  // Test case 10: Fragment-only relative URL
  ResultUrl := AlCombineUrlA('#newanchor', 'http://base.com/path');
  CheckUrl(
    ResultUrl, 'http://base.com/path#newanchor',
    'http', 'base.com', '/path', '', 'newanchor', 80);

  // Test case 11: Query-only relative URL
  ResultUrl := AlCombineUrlA('?newkey=newvalue', 'http://base.com/path');
  CheckUrl(
    ResultUrl, 'http://base.com/path?newkey=newvalue',
    'http', 'base.com', '/path', 'newkey=newvalue', '', 80);

  // Test case 12: Empty base URL with relative URL
  ResultUrl := AlCombineUrlA('sub/path', '');
  CheckUrl(
    ResultUrl, 'sub/path',
    '', '', 'sub/path', '', '', 0);

  // Test case 13: Malformed base URL (missing scheme)
  ResultUrl := AlCombineUrlA('sub', 'base.com/path');
  CheckUrl(
    ResultUrl, 'sub',
    '', '', 'sub', '', '', 0);

  // Test case 14: IPv6 base URL
  ResultUrl := AlCombineUrlA('sub', 'http://[2001:db8::1]:8080/main');
  CheckUrl(
    ResultUrl, 'http://[2001:db8::1]:8080/sub',
    'http', '[2001:db8::1]', '/sub', '', '', 8080);

  // Test case 15: Both URLs empty
  ResultUrl := AlCombineUrlA('', '');
  CheckUrl(
    ResultUrl, '',
    '', '', '', '', '', 0);

  // Test case 16: Relative URL with multiple parent directories (../../)
  ResultUrl := AlCombineUrlA('../../sub', 'http://base.com/a/b/c/d');
  CheckUrl(
    ResultUrl, 'http://base.com/a/sub',
    'http', 'base.com', '/a/sub', '', '', 80);

  // Test case 17: Relative URL with dot (.)
  ResultUrl := AlCombineUrlA('./sub', 'http://base.com/main/');
  CheckUrl(
    ResultUrl, 'http://base.com/main/sub',
    'http', 'base.com', '/main/sub', '', '', 80);

  // Test case 18: Base URL with fragment (should be ignored)
  ResultUrl := AlCombineUrlA('sub', 'http://base.com/path#oldanchor');
  CheckUrl(
    ResultUrl, 'http://base.com/sub',
    'http', 'base.com', '/sub', '', '', 80);

  // Test case 19: Relative URL with encoded query
  ResultUrl := AlCombineUrlA('?key=val%20ue', 'http://base.com/path');
  CheckUrl(
    ResultUrl, 'http://base.com/path?key=val%20ue',
    'http', 'base.com', '/path', 'key=val%20ue', '', 80);

  // Test case 20: Malformed relative URL with scheme-like fragment (e.g., www.yahoo.fr#aze@aze.fr)
  ResultUrl := AlCombineUrlA('#aze@aze.fr', 'http://base.com/path');
  CheckUrl(
    ResultUrl, 'http://base.com/path#aze@aze.fr',
    'http', 'base.com', '/path', '', 'aze@aze.fr', 80);
  {$ENDIF}
end;

initialization
  TDUnitX.RegisterTestFixture(TALDUnitXTestUrl);

end.