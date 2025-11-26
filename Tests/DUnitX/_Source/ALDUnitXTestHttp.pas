unit ALDUnitXTestHttp;

interface

uses
  DUnitX.TestFramework;

type

  [TestFixture]
  TALDUnitXTestHttp = class
  public
    //constructor create;
    //[Setup]
    //procedure Setup;
    //[TearDown]
    //procedure TearDown;
    [Test]
    procedure TestALHttpCookieA;
    [Test]
    procedure TestALHttpCookieW;
    [Test]
    procedure TestALUtcDateTimeToRfc1123StrA;
    [Test]
    procedure TestALUtcDateTimeToRfc1123StrW;
    [Test]
    procedure TestALTryRfc822DateStrToUtcDateTimeA;
    [Test]
    procedure TestALTryRfc822DateStrToUtcDateTimeW;
  end;

implementation

uses
  System.StrUtils,
  system.AnsiStrings,
  System.SysUtils,
  System.DateUtils,
  Alcinoe.Url,
  Alcinoe.Localization,
  Alcinoe.StringUtils,
  Alcinoe.Common,
  Alcinoe.Http;

{********************************************}
procedure TALDUnitXTestHttp.TestALHttpCookieA;
var
  Cookie: TALHttpCookieA;
  TestDate, TestDate2: TDateTime;
  ExpectedHeader: AnsiString;
begin
  Cookie := TALHttpCookieA.Create;
  try
    // Test Constructor
    Assert.AreEqual(ansiString(''), Cookie.Name, 'Name should be empty');
    Assert.AreEqual(ansiString(''), Cookie.Value, 'Value should be empty');
    Assert.AreEqual(ansiString(''), Cookie.Path, 'Path should be empty');
    Assert.AreEqual(ansiString(''), Cookie.Domain, 'Domain should be empty');
    Assert.AreEqual(TDateTime(ALNulLDate), Cookie.Expires, 'Expires should be 0');
    Assert.AreEqual(TALHttpCookieA.MaxAgeUnset, Cookie.MaxAge, 'MaxAge should be 0');
    Assert.AreEqual(ansiString(''), Cookie.SameSite, 'SameSite should be empty');
    Assert.IsFalse(Cookie.Secure, 'Secure should be false');
    Assert.IsFalse(Cookie.HttpOnly, 'HttpOnly should be false');
    Assert.IsFalse(Cookie.Partitioned, 'Partitioned should be false');

    // Test Properties
    Cookie.Name := 'session_id';
    Assert.AreEqual(AnsiString('session_id'), Cookie.Name, 'Name property should be set correctly');

    Cookie.Value := 'abc123';
    Assert.AreEqual(AnsiString('abc123'), Cookie.Value, 'Value property should be set correctly');

    Cookie.Path := '/app';
    Assert.AreEqual(AnsiString('/app'), Cookie.Path, 'Path property should be set correctly');

    Cookie.Domain := 'example.com';
    Assert.AreEqual(AnsiString('example.com'), Cookie.Domain, 'Domain property should be set correctly');

    TestDate := EncodeDateTime(2025, 12, 31, 23, 59, 59, 0);
    Cookie.Expires := TestDate;
    Assert.AreEqual(TestDate, Cookie.Expires, 'Expires property should be set correctly');

    Cookie.MaxAge := 3600;
    Assert.AreEqual(3600, Cookie.MaxAge, 'MaxAge property should be set correctly');

    Cookie.SameSite := 'Lax';
    Assert.AreEqual(AnsiString('Lax'), Cookie.SameSite, 'SameSite property should be set correctly');

    Cookie.Secure := True;
    Assert.IsTrue(Cookie.Secure, 'Secure property should be set to true');
    Cookie.Secure := False;
    Assert.IsFalse(Cookie.Secure, 'Secure property should be set to false');

    Cookie.HttpOnly := True;
    Assert.IsTrue(Cookie.HttpOnly, 'HttpOnly property should be set to true');
    Cookie.HttpOnly := False;
    Assert.IsFalse(Cookie.HttpOnly, 'HttpOnly property should be set to false');

    Cookie.Secure := True; // Required for Partitioned
    Cookie.Partitioned := True;
    Assert.IsTrue(Cookie.Partitioned, 'Partitioned property should be set to true');
    Cookie.Partitioned := False;
    Assert.IsFalse(Cookie.Partitioned, 'Partitioned property should be set to false');

    // Test Reset
    Cookie.Name := 'test';
    Cookie.Value := 'value';
    Cookie.Path := '/';
    Cookie.Domain := 'example.com';
    Cookie.Expires := Now;
    Cookie.MaxAge := 3600;
    Cookie.SameSite := 'Strict';
    Cookie.Secure := True;
    Cookie.HttpOnly := True;
    Cookie.Partitioned := True;

    Cookie.Reset;

    Assert.AreEqual(ansiString(''), Cookie.Name, 'Name should be reset');
    Assert.AreEqual(ansiString(''), Cookie.Value, 'Value should be reset');
    Assert.AreEqual(ansiString(''), Cookie.Path, 'Path should be reset');
    Assert.AreEqual(ansiString(''), Cookie.Domain, 'Domain should be reset');
    Assert.AreEqual(TDateTime(ALNullDate), Cookie.Expires, 'Expires should be reset');
    Assert.AreEqual(TALHttpCookieA.MaxAgeUnset, Cookie.MaxAge, 'MaxAge should be reset');
    Assert.AreEqual(ansiString(''), Cookie.SameSite, 'SameSite should be reset');
    Assert.IsFalse(Cookie.Secure, 'Secure should be reset');
    Assert.IsFalse(Cookie.HttpOnly, 'HttpOnly should be reset');
    Assert.IsFalse(Cookie.Partitioned, 'Partitioned should be reset');

    // Test HeaderValue Getter (30 test cases)
    Cookie.Name := 'session_id';
    Cookie.Value := 'abc123';
    Assert.AreEqual(AnsiString('session_id=abc123'), Cookie.HeaderValue, 'HeaderValue Getter: Basic name=value');

    Cookie.Value := '';
    Assert.AreEqual(AnsiString('session_id='), Cookie.HeaderValue, 'HeaderValue Getter: Empty value');

    Cookie.Path := '/';
    Assert.AreEqual(AnsiString('session_id=; Path=/'), Cookie.HeaderValue, 'HeaderValue Getter: With Path');

    Cookie.Domain := 'example.com';
    Assert.AreEqual(AnsiString('session_id=; Domain=example.com; Path=/'), Cookie.HeaderValue, 'HeaderValue Getter: With Domain');

    Cookie.Expires := TestDate;
    ExpectedHeader := 'session_id=; Domain=example.com; Path=/; Expires=' +
      ALFormatDateTimeA('ddd, dd mmm yyyy hh:nn:ss', TestDate) + ' GMT';
    Assert.AreEqual(ExpectedHeader, Cookie.HeaderValue, 'HeaderValue Getter: With Expires');

    Cookie.MaxAge := 3600;
    ExpectedHeader := ExpectedHeader + '; Max-Age=3600';
    Assert.AreEqual(ExpectedHeader, Cookie.HeaderValue, 'HeaderValue Getter: With Max-Age');

    Cookie.SameSite := 'Strict';
    ExpectedHeader := ExpectedHeader + '; SameSite=Strict';
    Assert.AreEqual(ExpectedHeader, Cookie.HeaderValue, 'HeaderValue Getter: With SameSite');

    Cookie.Secure := True;
    ExpectedHeader := ExpectedHeader + '; Secure';
    Assert.AreEqual(ExpectedHeader, Cookie.HeaderValue, 'HeaderValue Getter: With Secure');

    Cookie.HttpOnly := True;
    ExpectedHeader := ExpectedHeader + '; HttpOnly';
    Assert.AreEqual(ExpectedHeader, Cookie.HeaderValue, 'HeaderValue Getter: With HttpOnly');

    Cookie.Partitioned := True;
    ExpectedHeader := ExpectedHeader + '; Partitioned';
    Assert.AreEqual(ExpectedHeader, Cookie.HeaderValue, 'HeaderValue Getter: With Partitioned');

    Cookie.Reset;
    Cookie.Name := 'test_cookie';
    Cookie.Value := ALUrlEncode(AnsiString('value with spaces'), False{ASpacesAsPlus});
    Assert.AreEqual(AnsiString('test_cookie=value%20with%20spaces'), Cookie.HeaderValue, 'HeaderValue Getter: Value with spaces');

    Cookie.Name := 'cookie@123';
    Assert.AreEqual(AnsiString('cookie@123=value%20with%20spaces'), Cookie.HeaderValue, 'HeaderValue Getter: Name with special chars');

    Cookie.Path := '/test%20path/';
    Assert.AreEqual(AnsiString('cookie@123=value%20with%20spaces; Path=/test%20path/'), Cookie.HeaderValue, 'HeaderValue Getter: Path with spaces');

    Cookie.MaxAge := -1;
    Assert.AreEqual(AnsiString('cookie@123=value%20with%20spaces; Path=/test%20path/; Max-Age=-1'), Cookie.HeaderValue, 'HeaderValue Getter: Negative Max-Age');

    Cookie.SameSite := 'None';
    Cookie.Secure := True;
    Assert.AreEqual(AnsiString('cookie@123=value%20with%20spaces; Path=/test%20path/; Max-Age=-1; SameSite=None; Secure'), Cookie.HeaderValue, 'HeaderValue Getter: SameSite=None with Secure');

    Cookie.Reset;
    Cookie.Name := 'cookie';
    Cookie.Value := ALUrlEncode(AnsiString('val!@#'), False{ASpacesAsPlus});
    Assert.AreEqual(AnsiString('cookie=val%21%40%23'), Cookie.HeaderValue, 'HeaderValue Getter: Value with special chars');

    Cookie.Domain := '.example.com';
    Assert.AreEqual(AnsiString('cookie=val%21%40%23; Domain=.example.com'), Cookie.HeaderValue, 'HeaderValue Getter: Domain with leading dot');

    Cookie.Path := '/';
    Cookie.MaxAge := 0;
    Assert.AreEqual(AnsiString('cookie=val%21%40%23; Domain=.example.com; Path=/; Max-Age=0'), Cookie.HeaderValue, 'HeaderValue Getter: Zero Max-Age');

    Cookie.Name := 'long_cookie_name_1234567890';
    Assert.AreEqual(AnsiString('long_cookie_name_1234567890=val%21%40%23; Domain=.example.com; Path=/; Max-Age=0'), Cookie.HeaderValue, 'HeaderValue Getter: Long cookie name');

    TestDate2 := EncodeDateTime(2023, 1, 1, 0, 0, 0, 0);
    Cookie.Expires := TestDate2;
    ExpectedHeader := 'long_cookie_name_1234567890=val%21%40%23; Domain=.example.com; Path=/; Expires=' + ALFormatDateTimeA('ddd, dd mmm yyyy hh:nn:ss', TestDate2) + ' GMT; Max-Age=0';
    Assert.AreEqual(ExpectedHeader, Cookie.HeaderValue, 'HeaderValue Getter: Past Expires date');

    Cookie.SameSite := 'Lax';
    ExpectedHeader := ExpectedHeader + '; SameSite=Lax';
    Assert.AreEqual(ExpectedHeader, Cookie.HeaderValue, 'HeaderValue Getter: With SameSite Lax');

    Cookie.Value := 'a';
    ExpectedHeader := 'long_cookie_name_1234567890=a; Domain=.example.com; Path=/; Expires=' + ALFormatDateTimeA('ddd, dd mmm yyyy hh:nn:ss', TestDate2) + ' GMT; Max-Age=0; SameSite=Lax';
    Assert.AreEqual(ExpectedHeader, Cookie.HeaderValue, 'HeaderValue Getter: Single char value');

    Cookie.Path := '';
    ExpectedHeader := 'long_cookie_name_1234567890=a; Domain=.example.com; Expires=' + ALFormatDateTimeA('ddd, dd mmm yyyy hh:nn:ss', TestDate2) + ' GMT; Max-Age=0; SameSite=Lax';
    Assert.AreEqual(ExpectedHeader, Cookie.HeaderValue, 'HeaderValue Getter: Empty Path');

    Cookie.Domain := '';
    ExpectedHeader := 'long_cookie_name_1234567890=a; Expires=' +
      ALFormatDateTimeA('ddd, dd mmm yyyy hh:nn:ss', TestDate2) + ' GMT; Max-Age=0; SameSite=Lax';
    Assert.AreEqual(ExpectedHeader, Cookie.HeaderValue, 'HeaderValue Getter: Empty Domain');

    Cookie.SameSite := '';
    ExpectedHeader := 'long_cookie_name_1234567890=a; Expires=' +
      ALFormatDateTimeA('ddd, dd mmm yyyy hh:nn:ss', TestDate2) + ' GMT; Max-Age=0';
    Assert.AreEqual(ExpectedHeader, Cookie.HeaderValue, 'HeaderValue Getter: Empty SameSite');

    Cookie.Expires := 0;
    ExpectedHeader := 'long_cookie_name_1234567890=a; Max-Age=0';
    Assert.AreEqual(ExpectedHeader, Cookie.HeaderValue, 'HeaderValue Getter: No Expires');

    Cookie.MaxAge := MaxInt;
    ExpectedHeader := 'long_cookie_name_1234567890=a; Max-Age=' + ALIntToStrA(MaxInt);
    Assert.AreEqual(ExpectedHeader, Cookie.HeaderValue, 'HeaderValue Getter: MaxInt Max-Age');

    Cookie.Secure := True;
    Cookie.HttpOnly := True;
    ExpectedHeader := 'long_cookie_name_1234567890=a; Max-Age=' + ALIntToStrA(MaxInt) + '; Secure; HttpOnly';
    Assert.AreEqual(ExpectedHeader, Cookie.HeaderValue, 'HeaderValue Getter: Secure and HttpOnly');

    Cookie.Partitioned := True;
    ExpectedHeader := 'long_cookie_name_1234567890=a; Max-Age=' + ALIntToStrA(MaxInt) + '; Secure; HttpOnly; Partitioned';
    Assert.AreEqual(ExpectedHeader, Cookie.HeaderValue, 'HeaderValue Getter: With Partitioned');

    Cookie.Name := 'x';
    ExpectedHeader := 'x=a; Max-Age=' + ALIntToStrA(MaxInt) + '; Secure; HttpOnly; Partitioned';
    Assert.AreEqual(ExpectedHeader, Cookie.HeaderValue, 'HeaderValue Getter: Single char name');

    // Test HeaderValue Setter (30 test cases)
    Cookie.Reset;
    Cookie.HeaderValue := 'session_id=abc123';
    Assert.AreEqual(AnsiString('session_id'), Cookie.Name, 'HeaderValue Setter: Basic name');
    Assert.AreEqual(AnsiString('abc123'), Cookie.Value, 'HeaderValue Setter: Basic value');

    Cookie.HeaderValue := 'test_cookie=';
    Assert.AreEqual(AnsiString('test_cookie'), Cookie.Name, 'HeaderValue Setter: Empty value name');
    Assert.AreEqual(ansiString(''), Cookie.Value, 'HeaderValue Setter: Empty value');

    Cookie.HeaderValue := 'cookie=val; Path=/';
    Assert.AreEqual(AnsiString('/'), Cookie.Path, 'HeaderValue Setter: Path');

    Cookie.HeaderValue := 'cookie=val; Domain=example.com';
    Assert.AreEqual(AnsiString('example.com'), Cookie.Domain, 'HeaderValue Setter: Domain');

    Cookie.HeaderValue := 'cookie=val; Domain=example.com';
    Assert.AreEqual(AnsiString('example.com'), Cookie.Domain, 'HeaderValue Setter: Domain');

    Cookie.HeaderValue := 'cookie=val; Expires=' + ALFormatDateTimeA('ddd, dd mmm yyyy hh:nn:ss', TestDate) + ' GMT';
    Assert.IsTrue(Abs(Cookie.Expires - TestDate) < OneSecond, 'HeaderValue Setter: Expires');

    Cookie.HeaderValue := 'cookie=val; Max-Age=7200';
    Assert.AreEqual(7200, Cookie.MaxAge, 'HeaderValue Setter: Max-Age');

    Cookie.HeaderValue := 'cookie=val; SameSite=Lax';
    Assert.AreEqual(AnsiString('Lax'), Cookie.SameSite, 'HeaderValue Setter: SameSite');

    Cookie.HeaderValue := 'cookie=val; Secure';
    Assert.IsTrue(Cookie.Secure, 'HeaderValue Setter: Secure');

    Cookie.HeaderValue := 'cookie=val; HttpOnly';
    Assert.IsTrue(Cookie.HttpOnly, 'HeaderValue Setter: HttpOnly');

    Cookie.HeaderValue := 'cookie=val; Secure; Partitioned';
    Assert.IsTrue(Cookie.Partitioned, 'HeaderValue Setter: Partitioned');

    Cookie.HeaderValue := 'cookie=value%20with%20spaces';
    Assert.AreEqual(AnsiString('value%20with%20spaces'), Cookie.Value, 'HeaderValue Setter: URL encoded value');

    Cookie.HeaderValue := 'cookie=val; Path=/test%20path/';
    Assert.AreEqual(AnsiString('/test%20path/'), Cookie.Path, 'HeaderValue Setter: URL encoded path');

    Cookie.HeaderValue := 'cookie=val; Max-Age=0';
    Assert.AreEqual(0, Cookie.MaxAge, 'HeaderValue Setter: Zero Max-Age');

    Cookie.HeaderValue := 'cookie=val; SameSite=None; Secure';
    Assert.AreEqual(AnsiString('None'), Cookie.SameSite, 'HeaderValue Setter: SameSite=None');
    Assert.IsTrue(Cookie.Secure, 'HeaderValue Setter: SameSite=None requires Secure');

    Cookie.HeaderValue := 'complex_cookie=val123; Path=/app; Domain=sub.example.com; Max-Age=3600; SameSite=Strict; Secure; HttpOnly; Partitioned';
    Assert.AreEqual(AnsiString('complex_cookie'), Cookie.Name, 'HeaderValue Setter: Complex name');
    Assert.AreEqual(AnsiString('val123'), Cookie.Value, 'HeaderValue Setter: Complex value');
    Assert.AreEqual(AnsiString('/app'), Cookie.Path, 'HeaderValue Setter: Complex path');
    Assert.AreEqual(AnsiString('sub.example.com'), Cookie.Domain, 'HeaderValue Setter: Complex domain');
    Assert.AreEqual(3600, Cookie.MaxAge, 'HeaderValue Setter: Complex Max-Age');
    Assert.AreEqual(AnsiString('Strict'), Cookie.SameSite, 'HeaderValue Setter: Complex SameSite');
    Assert.IsTrue(Cookie.Secure, 'HeaderValue Setter: Complex Secure');
    Assert.IsTrue(Cookie.HttpOnly, 'HeaderValue Setter: Complex HttpOnly');
    Assert.IsTrue(Cookie.Partitioned, 'HeaderValue Setter: Complex Partitioned');

    Cookie.HeaderValue := 'cookie=val%21%40%23';
    Assert.AreEqual(AnsiString('val%21%40%23'), Cookie.Value, 'HeaderValue Setter: Special chars in value');

    Cookie.HeaderValue := 'cookie=val; Domain=.example.com';
    Assert.AreEqual(AnsiString('.example.com'), Cookie.Domain, 'HeaderValue Setter: Domain with leading dot');

    Cookie.HeaderValue := 'long_cookie_name_1234567890=val';
    Assert.AreEqual(AnsiString('long_cookie_name_1234567890'), Cookie.Name, 'HeaderValue Setter: Long cookie name');

    Cookie.HeaderValue := 'cookie=a';
    Assert.AreEqual(AnsiString('a'), Cookie.Value, 'HeaderValue Setter: Single char value');

    Cookie.HeaderValue := 'x=val';
    Assert.AreEqual(AnsiString('x'), Cookie.Name, 'HeaderValue Setter: Single char name');

    Cookie.HeaderValue := 'cookie=val; Max-Age=-1';
    Assert.AreEqual(-1, Cookie.MaxAge, 'HeaderValue Setter: Negative Max-Age');

    Cookie.HeaderValue := 'cookie=val; Path=';
    Assert.AreEqual(ansiString(''), Cookie.Path, 'HeaderValue Setter: Empty Path');

    Cookie.HeaderValue := 'cookie=val; Domain=';
    Assert.AreEqual(ansiString(''), Cookie.Domain, 'HeaderValue Setter: Empty Domain');

    Cookie.HeaderValue := 'cookie=val; SameSite=';
    Assert.AreEqual(ansiString(''), Cookie.SameSite, 'HeaderValue Setter: Empty SameSite');

    Cookie.HeaderValue := 'cookie=val; Expires=' + ALFormatDateTimeA('ddd, dd mmm yyyy hh:nn:ss', TestDate2) + ' GMT';
    Assert.IsTrue(Abs(Cookie.Expires - TestDate2) < OneSecond, 'HeaderValue Setter: Past Expires date');

  finally
    Cookie.Free;
  end;
end;

{********************************************}
procedure TALDUnitXTestHttp.TestALHttpCookieW;
var
  Cookie: TALHttpCookieW;
  TestDate, TestDate2: TDateTime;
  ExpectedHeader: String;
begin
  Cookie := TALHttpCookieW.Create;
  try
    // Test Constructor
    Assert.AreEqual(String(''), Cookie.Name, 'Name should be empty');
    Assert.AreEqual(String(''), Cookie.Value, 'Value should be empty');
    Assert.AreEqual(String(''), Cookie.Path, 'Path should be empty');
    Assert.AreEqual(String(''), Cookie.Domain, 'Domain should be empty');
    Assert.AreEqual(TDateTime(ALNulLDate), Cookie.Expires, 'Expires should be 0');
    Assert.AreEqual(TALHttpCookieW.MaxAgeUnset, Cookie.MaxAge, 'MaxAge should be 0');
    Assert.AreEqual(String(''), Cookie.SameSite, 'SameSite should be empty');
    Assert.IsFalse(Cookie.Secure, 'Secure should be false');
    Assert.IsFalse(Cookie.HttpOnly, 'HttpOnly should be false');
    Assert.IsFalse(Cookie.Partitioned, 'Partitioned should be false');

    // Test Properties
    Cookie.Name := 'session_id';
    Assert.AreEqual(String('session_id'), Cookie.Name, 'Name property should be set correctly');

    Cookie.Value := 'abc123';
    Assert.AreEqual(String('abc123'), Cookie.Value, 'Value property should be set correctly');

    Cookie.Path := '/app';
    Assert.AreEqual(String('/app'), Cookie.Path, 'Path property should be set correctly');

    Cookie.Domain := 'example.com';
    Assert.AreEqual(String('example.com'), Cookie.Domain, 'Domain property should be set correctly');

    TestDate := EncodeDateTime(2025, 12, 31, 23, 59, 59, 0);
    Cookie.Expires := TestDate;
    Assert.AreEqual(TestDate, Cookie.Expires, 'Expires property should be set correctly');

    Cookie.MaxAge := 3600;
    Assert.AreEqual(3600, Cookie.MaxAge, 'MaxAge property should be set correctly');

    Cookie.SameSite := 'Lax';
    Assert.AreEqual(String('Lax'), Cookie.SameSite, 'SameSite property should be set correctly');

    Cookie.Secure := True;
    Assert.IsTrue(Cookie.Secure, 'Secure property should be set to true');
    Cookie.Secure := False;
    Assert.IsFalse(Cookie.Secure, 'Secure property should be set to false');

    Cookie.HttpOnly := True;
    Assert.IsTrue(Cookie.HttpOnly, 'HttpOnly property should be set to true');
    Cookie.HttpOnly := False;
    Assert.IsFalse(Cookie.HttpOnly, 'HttpOnly property should be set to false');

    Cookie.Secure := True; // Required for Partitioned
    Cookie.Partitioned := True;
    Assert.IsTrue(Cookie.Partitioned, 'Partitioned property should be set to true');
    Cookie.Partitioned := False;
    Assert.IsFalse(Cookie.Partitioned, 'Partitioned property should be set to false');

    // Test Reset
    Cookie.Name := 'test';
    Cookie.Value := 'value';
    Cookie.Path := '/';
    Cookie.Domain := 'example.com';
    Cookie.Expires := Now;
    Cookie.MaxAge := 3600;
    Cookie.SameSite := 'Strict';
    Cookie.Secure := True;
    Cookie.HttpOnly := True;
    Cookie.Partitioned := True;

    Cookie.Reset;

    Assert.AreEqual(String(''), Cookie.Name, 'Name should be reset');
    Assert.AreEqual(String(''), Cookie.Value, 'Value should be reset');
    Assert.AreEqual(String(''), Cookie.Path, 'Path should be reset');
    Assert.AreEqual(String(''), Cookie.Domain, 'Domain should be reset');
    Assert.AreEqual(TDateTime(ALNullDate), Cookie.Expires, 'Expires should be reset');
    Assert.AreEqual(TALHttpCookieW.MaxAgeUnset, Cookie.MaxAge, 'MaxAge should be reset');
    Assert.AreEqual(String(''), Cookie.SameSite, 'SameSite should be reset');
    Assert.IsFalse(Cookie.Secure, 'Secure should be reset');
    Assert.IsFalse(Cookie.HttpOnly, 'HttpOnly should be reset');
    Assert.IsFalse(Cookie.Partitioned, 'Partitioned should be reset');

    // Test HeaderValue Getter (30 test cases)
    Cookie.Name := 'session_id';
    Cookie.Value := 'abc123';
    Assert.AreEqual(String('session_id=abc123'), Cookie.HeaderValue, 'HeaderValue Getter: Basic name=value');

    Cookie.Value := '';
    Assert.AreEqual(String('session_id='), Cookie.HeaderValue, 'HeaderValue Getter: Empty value');

    Cookie.Path := '/';
    Assert.AreEqual(String('session_id=; Path=/'), Cookie.HeaderValue, 'HeaderValue Getter: With Path');

    Cookie.Domain := 'example.com';
    Assert.AreEqual(String('session_id=; Domain=example.com; Path=/'), Cookie.HeaderValue, 'HeaderValue Getter: With Domain');

    Cookie.Expires := TestDate;
    ExpectedHeader := 'session_id=; Domain=example.com; Path=/; Expires=' +
      ALFormatDateTimeW('ddd, dd mmm yyyy hh:nn:ss', TestDate) + ' GMT';
    Assert.AreEqual(ExpectedHeader, Cookie.HeaderValue, 'HeaderValue Getter: With Expires');

    Cookie.MaxAge := 3600;
    ExpectedHeader := ExpectedHeader + '; Max-Age=3600';
    Assert.AreEqual(ExpectedHeader, Cookie.HeaderValue, 'HeaderValue Getter: With Max-Age');

    Cookie.SameSite := 'Strict';
    ExpectedHeader := ExpectedHeader + '; SameSite=Strict';
    Assert.AreEqual(ExpectedHeader, Cookie.HeaderValue, 'HeaderValue Getter: With SameSite');

    Cookie.Secure := True;
    ExpectedHeader := ExpectedHeader + '; Secure';
    Assert.AreEqual(ExpectedHeader, Cookie.HeaderValue, 'HeaderValue Getter: With Secure');

    Cookie.HttpOnly := True;
    ExpectedHeader := ExpectedHeader + '; HttpOnly';
    Assert.AreEqual(ExpectedHeader, Cookie.HeaderValue, 'HeaderValue Getter: With HttpOnly');

    Cookie.Partitioned := True;
    ExpectedHeader := ExpectedHeader + '; Partitioned';
    Assert.AreEqual(ExpectedHeader, Cookie.HeaderValue, 'HeaderValue Getter: With Partitioned');

    Cookie.Reset;
    Cookie.Name := 'test_cookie';
    Cookie.Value := ALUrlEncode(String('value with spaces'), False{ASpacesAsPlus});
    Assert.AreEqual(String('test_cookie=value%20with%20spaces'), Cookie.HeaderValue, 'HeaderValue Getter: Value with spaces');

    Cookie.Name := 'cookie@123';
    Assert.AreEqual(String('cookie@123=value%20with%20spaces'), Cookie.HeaderValue, 'HeaderValue Getter: Name with special chars');

    Cookie.Path := '/test%20path/';
    Assert.AreEqual(String('cookie@123=value%20with%20spaces; Path=/test%20path/'), Cookie.HeaderValue, 'HeaderValue Getter: Path with spaces');

    Cookie.MaxAge := -1;
    Assert.AreEqual(String('cookie@123=value%20with%20spaces; Path=/test%20path/; Max-Age=-1'), Cookie.HeaderValue, 'HeaderValue Getter: Negative Max-Age');

    Cookie.SameSite := 'None';
    Cookie.Secure := True;
    Assert.AreEqual(String('cookie@123=value%20with%20spaces; Path=/test%20path/; Max-Age=-1; SameSite=None; Secure'), Cookie.HeaderValue, 'HeaderValue Getter: SameSite=None with Secure');

    Cookie.Reset;
    Cookie.Name := 'cookie';
    Cookie.Value := ALUrlEncode(String('val!@#'), False{ASpacesAsPlus});
    Assert.AreEqual(String('cookie=val%21%40%23'), Cookie.HeaderValue, 'HeaderValue Getter: Value with special chars');

    Cookie.Domain := '.example.com';
    Assert.AreEqual(String('cookie=val%21%40%23; Domain=.example.com'), Cookie.HeaderValue, 'HeaderValue Getter: Domain with leading dot');

    Cookie.Path := '/';
    Cookie.MaxAge := 0;
    Assert.AreEqual(String('cookie=val%21%40%23; Domain=.example.com; Path=/; Max-Age=0'), Cookie.HeaderValue, 'HeaderValue Getter: Zero Max-Age');

    Cookie.Name := 'long_cookie_name_1234567890';
    Assert.AreEqual(String('long_cookie_name_1234567890=val%21%40%23; Domain=.example.com; Path=/; Max-Age=0'), Cookie.HeaderValue, 'HeaderValue Getter: Long cookie name');

    TestDate2 := EncodeDateTime(2023, 1, 1, 0, 0, 0, 0);
    Cookie.Expires := TestDate2;
    ExpectedHeader := 'long_cookie_name_1234567890=val%21%40%23; Domain=.example.com; Path=/; Expires=' + ALFormatDateTimeW('ddd, dd mmm yyyy hh:nn:ss', TestDate2) + ' GMT; Max-Age=0';
    Assert.AreEqual(ExpectedHeader, Cookie.HeaderValue, 'HeaderValue Getter: Past Expires date');

    Cookie.SameSite := 'Lax';
    ExpectedHeader := ExpectedHeader + '; SameSite=Lax';
    Assert.AreEqual(ExpectedHeader, Cookie.HeaderValue, 'HeaderValue Getter: With SameSite Lax');

    Cookie.Value := 'a';
    ExpectedHeader := 'long_cookie_name_1234567890=a; Domain=.example.com; Path=/; Expires=' + ALFormatDateTimeW('ddd, dd mmm yyyy hh:nn:ss', TestDate2) + ' GMT; Max-Age=0; SameSite=Lax';
    Assert.AreEqual(ExpectedHeader, Cookie.HeaderValue, 'HeaderValue Getter: Single char value');

    Cookie.Path := '';
    ExpectedHeader := 'long_cookie_name_1234567890=a; Domain=.example.com; Expires=' + ALFormatDateTimeW('ddd, dd mmm yyyy hh:nn:ss', TestDate2) + ' GMT; Max-Age=0; SameSite=Lax';
    Assert.AreEqual(ExpectedHeader, Cookie.HeaderValue, 'HeaderValue Getter: Empty Path');

    Cookie.Domain := '';
    ExpectedHeader := 'long_cookie_name_1234567890=a; Expires=' +
      ALFormatDateTimeW('ddd, dd mmm yyyy hh:nn:ss', TestDate2) + ' GMT; Max-Age=0; SameSite=Lax';
    Assert.AreEqual(ExpectedHeader, Cookie.HeaderValue, 'HeaderValue Getter: Empty Domain');

    Cookie.SameSite := '';
    ExpectedHeader := 'long_cookie_name_1234567890=a; Expires=' +
      ALFormatDateTimeW('ddd, dd mmm yyyy hh:nn:ss', TestDate2) + ' GMT; Max-Age=0';
    Assert.AreEqual(ExpectedHeader, Cookie.HeaderValue, 'HeaderValue Getter: Empty SameSite');

    Cookie.Expires := 0;
    ExpectedHeader := 'long_cookie_name_1234567890=a; Max-Age=0';
    Assert.AreEqual(ExpectedHeader, Cookie.HeaderValue, 'HeaderValue Getter: No Expires');

    Cookie.MaxAge := MaxInt;
    ExpectedHeader := 'long_cookie_name_1234567890=a; Max-Age=' + ALIntToStrW(MaxInt);
    Assert.AreEqual(ExpectedHeader, Cookie.HeaderValue, 'HeaderValue Getter: MaxInt Max-Age');

    Cookie.Secure := True;
    Cookie.HttpOnly := True;
    ExpectedHeader := 'long_cookie_name_1234567890=a; Max-Age=' + ALIntToStrW(MaxInt) + '; Secure; HttpOnly';
    Assert.AreEqual(ExpectedHeader, Cookie.HeaderValue, 'HeaderValue Getter: Secure and HttpOnly');

    Cookie.Partitioned := True;
    ExpectedHeader := 'long_cookie_name_1234567890=a; Max-Age=' + ALIntToStrW(MaxInt) + '; Secure; HttpOnly; Partitioned';
    Assert.AreEqual(ExpectedHeader, Cookie.HeaderValue, 'HeaderValue Getter: With Partitioned');

    Cookie.Name := 'x';
    ExpectedHeader := 'x=a; Max-Age=' + ALIntToStrW(MaxInt) + '; Secure; HttpOnly; Partitioned';
    Assert.AreEqual(ExpectedHeader, Cookie.HeaderValue, 'HeaderValue Getter: Single char name');

    // Test HeaderValue Setter (30 test cases)
    Cookie.Reset;
    Cookie.HeaderValue := 'session_id=abc123';
    Assert.AreEqual(String('session_id'), Cookie.Name, 'HeaderValue Setter: Basic name');
    Assert.AreEqual(String('abc123'), Cookie.Value, 'HeaderValue Setter: Basic value');

    Cookie.HeaderValue := 'test_cookie=';
    Assert.AreEqual(String('test_cookie'), Cookie.Name, 'HeaderValue Setter: Empty value name');
    Assert.AreEqual(String(''), Cookie.Value, 'HeaderValue Setter: Empty value');

    Cookie.HeaderValue := 'cookie=val; Path=/';
    Assert.AreEqual(String('/'), Cookie.Path, 'HeaderValue Setter: Path');

    Cookie.HeaderValue := 'cookie=val; Domain=example.com';
    Assert.AreEqual(String('example.com'), Cookie.Domain, 'HeaderValue Setter: Domain');

    Cookie.HeaderValue := 'cookie=val; Domain=example.com';
    Assert.AreEqual(String('example.com'), Cookie.Domain, 'HeaderValue Setter: Domain');

    Cookie.HeaderValue := 'cookie=val; Expires=' + ALFormatDateTimeW('ddd, dd mmm yyyy hh:nn:ss', TestDate) + ' GMT';
    Assert.IsTrue(Abs(Cookie.Expires - TestDate) < OneSecond, 'HeaderValue Setter: Expires');

    Cookie.HeaderValue := 'cookie=val; Max-Age=7200';
    Assert.AreEqual(7200, Cookie.MaxAge, 'HeaderValue Setter: Max-Age');

    Cookie.HeaderValue := 'cookie=val; SameSite=Lax';
    Assert.AreEqual(String('Lax'), Cookie.SameSite, 'HeaderValue Setter: SameSite');

    Cookie.HeaderValue := 'cookie=val; Secure';
    Assert.IsTrue(Cookie.Secure, 'HeaderValue Setter: Secure');

    Cookie.HeaderValue := 'cookie=val; HttpOnly';
    Assert.IsTrue(Cookie.HttpOnly, 'HeaderValue Setter: HttpOnly');

    Cookie.HeaderValue := 'cookie=val; Secure; Partitioned';
    Assert.IsTrue(Cookie.Partitioned, 'HeaderValue Setter: Partitioned');

    Cookie.HeaderValue := 'cookie=value%20with%20spaces';
    Assert.AreEqual(String('value%20with%20spaces'), Cookie.Value, 'HeaderValue Setter: URL encoded value');

    Cookie.HeaderValue := 'cookie=val; Path=/test%20path/';
    Assert.AreEqual(String('/test%20path/'), Cookie.Path, 'HeaderValue Setter: URL encoded path');

    Cookie.HeaderValue := 'cookie=val; Max-Age=0';
    Assert.AreEqual(0, Cookie.MaxAge, 'HeaderValue Setter: Zero Max-Age');

    Cookie.HeaderValue := 'cookie=val; SameSite=None; Secure';
    Assert.AreEqual(String('None'), Cookie.SameSite, 'HeaderValue Setter: SameSite=None');
    Assert.IsTrue(Cookie.Secure, 'HeaderValue Setter: SameSite=None requires Secure');

    Cookie.HeaderValue := 'complex_cookie=val123; Path=/app; Domain=sub.example.com; Max-Age=3600; SameSite=Strict; Secure; HttpOnly; Partitioned';
    Assert.AreEqual(String('complex_cookie'), Cookie.Name, 'HeaderValue Setter: Complex name');
    Assert.AreEqual(String('val123'), Cookie.Value, 'HeaderValue Setter: Complex value');
    Assert.AreEqual(String('/app'), Cookie.Path, 'HeaderValue Setter: Complex path');
    Assert.AreEqual(String('sub.example.com'), Cookie.Domain, 'HeaderValue Setter: Complex domain');
    Assert.AreEqual(3600, Cookie.MaxAge, 'HeaderValue Setter: Complex Max-Age');
    Assert.AreEqual(String('Strict'), Cookie.SameSite, 'HeaderValue Setter: Complex SameSite');
    Assert.IsTrue(Cookie.Secure, 'HeaderValue Setter: Complex Secure');
    Assert.IsTrue(Cookie.HttpOnly, 'HeaderValue Setter: Complex HttpOnly');
    Assert.IsTrue(Cookie.Partitioned, 'HeaderValue Setter: Complex Partitioned');

    Cookie.HeaderValue := 'cookie=val%21%40%23';
    Assert.AreEqual(String('val%21%40%23'), Cookie.Value, 'HeaderValue Setter: Special chars in value');

    Cookie.HeaderValue := 'cookie=val; Domain=.example.com';
    Assert.AreEqual(String('.example.com'), Cookie.Domain, 'HeaderValue Setter: Domain with leading dot');

    Cookie.HeaderValue := 'long_cookie_name_1234567890=val';
    Assert.AreEqual(String('long_cookie_name_1234567890'), Cookie.Name, 'HeaderValue Setter: Long cookie name');

    Cookie.HeaderValue := 'cookie=a';
    Assert.AreEqual(String('a'), Cookie.Value, 'HeaderValue Setter: Single char value');

    Cookie.HeaderValue := 'x=val';
    Assert.AreEqual(String('x'), Cookie.Name, 'HeaderValue Setter: Single char name');

    Cookie.HeaderValue := 'cookie=val; Max-Age=-1';
    Assert.AreEqual(-1, Cookie.MaxAge, 'HeaderValue Setter: Negative Max-Age');

    Cookie.HeaderValue := 'cookie=val; Path=';
    Assert.AreEqual(String(''), Cookie.Path, 'HeaderValue Setter: Empty Path');

    Cookie.HeaderValue := 'cookie=val; Domain=';
    Assert.AreEqual(String(''), Cookie.Domain, 'HeaderValue Setter: Empty Domain');

    Cookie.HeaderValue := 'cookie=val; SameSite=';
    Assert.AreEqual(String(''), Cookie.SameSite, 'HeaderValue Setter: Empty SameSite');

    Cookie.HeaderValue := 'cookie=val; Expires=' + ALFormatDateTimeW('ddd, dd mmm yyyy hh:nn:ss', TestDate2) + ' GMT';
    Assert.IsTrue(Abs(Cookie.Expires - TestDate2) < OneSecond, 'HeaderValue Setter: Past Expires date');

  finally
    Cookie.Free;
  end;
end;

{*********************************************************}
procedure TALDUnitXTestHttp.TestALUtcDateTimeToRfc1123StrA;
var
  DateTime: TDateTime;
  Result: AnsiString;
begin
  // Test 1: Standard date and time (RFC 1123 example)
  DateTime := EncodeDateTime(1994, 11, 6, 8, 49, 37, 0);
  Result := ALUtcDateTimeToRfc1123StrA(DateTime);
  Assert.AreEqual(AnsiString('Sun, 06 Nov 1994 08:49:37 GMT'), Result, 'Test 1: Standard RFC 1123 date');

  // Test 2: Current date in UTC
  DateTime := TTimeZone.Local.ToUniversalTime(Now);
  Result := ALUtcDateTimeToRfc1123StrA(DateTime);
  Assert.IsTrue(Length(Result) = 29, 'Test 2: Length of current date string');
  Assert.IsTrue(system.AnsiStrings.EndsStr('GMT', AnsiString(Result)), 'Test 2: Ends with GMT');
  Assert.IsMatch('^[A-Za-z]{3}, \d{2} [A-Za-z]{3} \d{4} \d{2}:\d{2}:\d{2} GMT$', String(Result), 'Test 2: RFC 1123 format');

  // Test 5: Midnight on a known date
  DateTime := EncodeDateTime(2025, 9, 15, 0, 0, 0, 0);
  Result := ALUtcDateTimeToRfc1123StrA(DateTime);
  Assert.AreEqual(AnsiString('Mon, 15 Sep 2025 00:00:00 GMT'), Result, 'Test 5: Midnight 2025-09-15');

  // Test 6: End of day
  DateTime := EncodeDateTime(2025, 9, 15, 23, 59, 59, 999);
  Result := ALUtcDateTimeToRfc1123StrA(DateTime);
  Assert.AreEqual(AnsiString('Mon, 15 Sep 2025 23:59:59 GMT'), Result, 'Test 6: End of day 2025-09-15');

  // Test 7: Leap year date
  DateTime := EncodeDateTime(2024, 2, 29, 12, 0, 0, 0);
  Result := ALUtcDateTimeToRfc1123StrA(DateTime);
  Assert.AreEqual(AnsiString('Thu, 29 Feb 2024 12:00:00 GMT'), Result, 'Test 7: Leap year date');

  // Test 8: First day of year
  DateTime := EncodeDateTime(2025, 1, 1, 0, 0, 0, 0);
  Result := ALUtcDateTimeToRfc1123StrA(DateTime);
  Assert.AreEqual(AnsiString('Wed, 01 Jan 2025 00:00:00 GMT'), Result, 'Test 8: First day of 2025');

  // Test 9: Last day of year
  DateTime := EncodeDateTime(2025, 12, 31, 23, 59, 59, 0);
  Result := ALUtcDateTimeToRfc1123StrA(DateTime);
  Assert.AreEqual(AnsiString('Wed, 31 Dec 2025 23:59:59 GMT'), Result, 'Test 9: Last day of 2025');

  // Test 10: Non-UTC input (local time, should convert to UTC)
  DateTime := EncodeDateTime(2025, 9, 15, 10, 0, 0, 0); // Local time
  DateTime := TTimeZone.Local.ToUniversalTime(DateTime);
  Result := ALUtcDateTimeToRfc1123StrA(DateTime);
  Assert.IsTrue(system.AnsiStrings.EndsStr('GMT', AnsiString(Result)), 'Test 10: Non-UTC input ends with GMT');
  Assert.IsMatch('^[A-Za-z]{3}, 15 Sep 2025 \d{2}:\d{2}:\d{2} GMT$', String(Result), 'Test 10: Non-UTC input format');

  // Test 11: Date with milliseconds (should ignore milliseconds)
  DateTime := EncodeDateTime(2025, 9, 15, 14, 30, 45, 500);
  Result := ALUtcDateTimeToRfc1123StrA(DateTime);
  Assert.AreEqual(AnsiString('Mon, 15 Sep 2025 14:30:45 GMT'), Result, 'Test 11: Milliseconds ignored');

  // Test 12: First second of a century
  DateTime := EncodeDateTime(2000, 1, 1, 0, 0, 0, 0);
  Result := ALUtcDateTimeToRfc1123StrA(DateTime);
  Assert.AreEqual(AnsiString('Sat, 01 Jan 2000 00:00:00 GMT'), Result, 'Test 12: First second of 2000');

  // Test 13: Invalid date (0.0, should handle gracefully)
  DateTime := 0.0; // 1899-12-30 00:00:00
  Result := ALUtcDateTimeToRfc1123StrA(DateTime);
  Assert.AreEqual(AnsiString('Sat, 30 Dec 1899 00:00:00 GMT'), Result, 'Test 13: Zero date');

  // Test 15: Date with single-digit day
  DateTime := EncodeDateTime(2025, 1, 1, 8, 5, 3, 0);
  Result := ALUtcDateTimeToRfc1123StrA(DateTime);
  Assert.AreEqual(AnsiString('Wed, 01 Jan 2025 08:05:03 GMT'), Result, 'Test 15: Single-digit day');

  // Test 16: Date with single-digit hour
  DateTime := EncodeDateTime(2025, 1, 15, 9, 5, 3, 0);
  Result := ALUtcDateTimeToRfc1123StrA(DateTime);
  Assert.AreEqual(AnsiString('Wed, 15 Jan 2025 09:05:03 GMT'), Result, 'Test 16: Single-digit hour');

  // Test 17: Date with single-digit minute
  DateTime := EncodeDateTime(2025, 1, 15, 14, 7, 3, 0);
  Result := ALUtcDateTimeToRfc1123StrA(DateTime);
  Assert.AreEqual(AnsiString('Wed, 15 Jan 2025 14:07:03 GMT'), Result, 'Test 17: Single-digit minute');

  // Test 18: Date with single-digit second
  DateTime := EncodeDateTime(2025, 1, 15, 14, 15, 9, 0);
  Result := ALUtcDateTimeToRfc1123StrA(DateTime);
  Assert.AreEqual(AnsiString('Wed, 15 Jan 2025 14:15:09 GMT'), Result, 'Test 18: Single-digit second');

  // Test 19: Early 20th century
  DateTime := EncodeDateTime(1900, 1, 1, 0, 0, 0, 0);
  Result := ALUtcDateTimeToRfc1123StrA(DateTime);
  Assert.AreEqual(AnsiString('Mon, 01 Jan 1900 00:00:00 GMT'), Result, 'Test 19: Early 20th century');
end;

{*********************************************************}
procedure TALDUnitXTestHttp.TestALUtcDateTimeToRfc1123StrW;
var
  DateTime: TDateTime;
  Result: String;
begin
  // Test 1: Standard date and time (RFC 1123 example)
  DateTime := EncodeDateTime(1994, 11, 6, 8, 49, 37, 0);
  Result := ALUtcDateTimeToRfc1123StrW(DateTime);
  Assert.AreEqual(String('Sun, 06 Nov 1994 08:49:37 GMT'), Result, 'Test 1: Standard RFC 1123 date');

  // Test 2: Current date in UTC
  DateTime := TTimeZone.Local.ToUniversalTime(Now);
  Result := ALUtcDateTimeToRfc1123StrW(DateTime);
  Assert.IsTrue(Length(Result) = 29, 'Test 2: Length of current date string');
  Assert.IsTrue(System.StrUtils.EndsStr('GMT', String(Result)), 'Test 2: Ends with GMT');
  Assert.IsMatch('^[A-Za-z]{3}, \d{2} [A-Za-z]{3} \d{4} \d{2}:\d{2}:\d{2} GMT$', String(Result), 'Test 2: RFC 1123 format');

  // Test 5: Midnight on a known date
  DateTime := EncodeDateTime(2025, 9, 15, 0, 0, 0, 0);
  Result := ALUtcDateTimeToRfc1123StrW(DateTime);
  Assert.AreEqual(String('Mon, 15 Sep 2025 00:00:00 GMT'), Result, 'Test 5: Midnight 2025-09-15');

  // Test 6: End of day
  DateTime := EncodeDateTime(2025, 9, 15, 23, 59, 59, 999);
  Result := ALUtcDateTimeToRfc1123StrW(DateTime);
  Assert.AreEqual(String('Mon, 15 Sep 2025 23:59:59 GMT'), Result, 'Test 6: End of day 2025-09-15');

  // Test 7: Leap year date
  DateTime := EncodeDateTime(2024, 2, 29, 12, 0, 0, 0);
  Result := ALUtcDateTimeToRfc1123StrW(DateTime);
  Assert.AreEqual(String('Thu, 29 Feb 2024 12:00:00 GMT'), Result, 'Test 7: Leap year date');

  // Test 8: First day of year
  DateTime := EncodeDateTime(2025, 1, 1, 0, 0, 0, 0);
  Result := ALUtcDateTimeToRfc1123StrW(DateTime);
  Assert.AreEqual(String('Wed, 01 Jan 2025 00:00:00 GMT'), Result, 'Test 8: First day of 2025');

  // Test 9: Last day of year
  DateTime := EncodeDateTime(2025, 12, 31, 23, 59, 59, 0);
  Result := ALUtcDateTimeToRfc1123StrW(DateTime);
  Assert.AreEqual(String('Wed, 31 Dec 2025 23:59:59 GMT'), Result, 'Test 9: Last day of 2025');

  // Test 10: Non-UTC input (local time, should convert to UTC)
  DateTime := EncodeDateTime(2025, 9, 15, 10, 0, 0, 0); // Local time
  DateTime := TTimeZone.Local.ToUniversalTime(DateTime);
  Result := ALUtcDateTimeToRfc1123StrW(DateTime);
  Assert.IsTrue(EndsStr('GMT', String(Result)), 'Test 10: Non-UTC input ends with GMT');
  Assert.IsMatch('^[A-Za-z]{3}, 15 Sep 2025 \d{2}:\d{2}:\d{2} GMT$', String(Result), 'Test 10: Non-UTC input format');

  // Test 11: Date with milliseconds (should ignore milliseconds)
  DateTime := EncodeDateTime(2025, 9, 15, 14, 30, 45, 500);
  Result := ALUtcDateTimeToRfc1123StrW(DateTime);
  Assert.AreEqual(String('Mon, 15 Sep 2025 14:30:45 GMT'), Result, 'Test 11: Milliseconds ignored');

  // Test 12: First second of a century
  DateTime := EncodeDateTime(2000, 1, 1, 0, 0, 0, 0);
  Result := ALUtcDateTimeToRfc1123StrW(DateTime);
  Assert.AreEqual(String('Sat, 01 Jan 2000 00:00:00 GMT'), Result, 'Test 12: First second of 2000');

  // Test 13: Invalid date (0.0, should handle gracefully)
  DateTime := 0.0; // 1899-12-30 00:00:00
  Result := ALUtcDateTimeToRfc1123StrW(DateTime);
  Assert.AreEqual(String('Sat, 30 Dec 1899 00:00:00 GMT'), Result, 'Test 13: Zero date');

  // Test 15: Date with single-digit day
  DateTime := EncodeDateTime(2025, 1, 1, 8, 5, 3, 0);
  Result := ALUtcDateTimeToRfc1123StrW(DateTime);
  Assert.AreEqual(String('Wed, 01 Jan 2025 08:05:03 GMT'), Result, 'Test 15: Single-digit day');

  // Test 16: Date with single-digit hour
  DateTime := EncodeDateTime(2025, 1, 15, 9, 5, 3, 0);
  Result := ALUtcDateTimeToRfc1123StrW(DateTime);
  Assert.AreEqual(String('Wed, 15 Jan 2025 09:05:03 GMT'), Result, 'Test 16: Single-digit hour');

  // Test 17: Date with single-digit minute
  DateTime := EncodeDateTime(2025, 1, 15, 14, 7, 3, 0);
  Result := ALUtcDateTimeToRfc1123StrW(DateTime);
  Assert.AreEqual(String('Wed, 15 Jan 2025 14:07:03 GMT'), Result, 'Test 17: Single-digit minute');

  // Test 18: Date with single-digit second
  DateTime := EncodeDateTime(2025, 1, 15, 14, 15, 9, 0);
  Result := ALUtcDateTimeToRfc1123StrW(DateTime);
  Assert.AreEqual(String('Wed, 15 Jan 2025 14:15:09 GMT'), Result, 'Test 18: Single-digit second');

  // Test 19: Early 20th century
  DateTime := EncodeDateTime(1900, 1, 1, 0, 0, 0, 0);
  Result := ALUtcDateTimeToRfc1123StrW(DateTime);
  Assert.AreEqual(String('Mon, 01 Jan 1900 00:00:00 GMT'), Result, 'Test 19: Early 20th century');
end;

{***************************************************************}
procedure TALDUnitXTestHttp.TestALTryRfc822DateStrToUtcDateTimeA;
var
  DateTime: TDateTime;
  Result: Boolean;
  ExpectedDateTime: TDateTime;
begin
  // Test 1: Standard RFC 1123 date (Sun, 06 Nov 1994 08:49:37 GMT)
  Result := ALTryRfc822DateStrToUtcDateTime('Sun, 06 Nov 1994 08:49:37 GMT', DateTime);
  ExpectedDateTime := EncodeDateTime(1994, 11, 6, 8, 49, 37, 0);
  Assert.IsTrue(Result, 'Test 1: Should parse standard RFC 1123 date');
  Assert.AreEqual(ExpectedDateTime, DateTime, 'Test 1: Parsed date matches');

  // Test 2: RFC 1123 with hyphenated month (Sun, 06-Nov-1994 08:49:37 GMT)
  Result := ALTryRfc822DateStrToUtcDateTime('Sun, 06-Nov-1994 08:49:37 GMT', DateTime);
  ExpectedDateTime := EncodeDateTime(1994, 11, 6, 8, 49, 37, 0);
  Assert.IsTrue(Result, 'Test 2: Should parse hyphenated RFC 1123 date');
  Assert.AreEqual(ExpectedDateTime, DateTime, 'Test 2: Parsed date matches');

  // Test 3: RFC 2822 with +0200 offset (Sun, 06 Nov 1994 08:49:37 +0200)
  Result := ALTryRfc822DateStrToUtcDateTime('Sun, 06 Nov 1994 08:49:37 +0200', DateTime);
  ExpectedDateTime := EncodeDateTime(1994, 11, 6, 6, 49, 37, 0); // UTC = local - offset
  Assert.IsTrue(Result, 'Test 3: Should parse RFC 2822 with +0200 offset');
  Assert.AreEqual(ExpectedDateTime, DateTime, 'Test 3: Parsed date matches');

  // Test 4: RFC 2822 with -0700 offset (Tue, 23 Aug 2004 06:48:46 -0700)
  Result := ALTryRfc822DateStrToUtcDateTime('Tue, 23 Aug 2004 06:48:46 -0700', DateTime);
  ExpectedDateTime := EncodeDateTime(2004, 8, 23, 13, 48, 46, 0); // UTC = local + offset
  Assert.IsTrue(Result, 'Test 4: Should parse RFC 2822 with -0700 offset');
  Assert.AreEqual(ExpectedDateTime, DateTime, 'Test 4: Parsed date matches');

  // Test 5: Current date (2025-09-15 23:00:00 GMT)
  Result := ALTryRfc822DateStrToUtcDateTime('Mon, 15 Sep 2025 23:00:00 GMT', DateTime);
  ExpectedDateTime := EncodeDateTime(2025, 9, 15, 23, 0, 0, 0);
  Assert.IsTrue(Result, 'Test 5: Should parse current date');
  Assert.AreEqual(ExpectedDateTime, DateTime, 'Test 5: Parsed date matches');

  // Test 8: Single-digit day (Wed, 01 Jan 2025 08:05:03 GMT)
  Result := ALTryRfc822DateStrToUtcDateTime('Wed, 01 Jan 2025 08:05:03 GMT', DateTime);
  ExpectedDateTime := EncodeDateTime(2025, 1, 1, 8, 5, 3, 0);
  Assert.IsTrue(Result, 'Test 8: Should parse single-digit day');
  Assert.AreEqual(ExpectedDateTime, DateTime, 'Test 8: Parsed date matches');

  // Test 9: Single-digit hour (Wed, 15 Jan 2025 09:05:03 GMT)
  Result := ALTryRfc822DateStrToUtcDateTime('Wed, 15 Jan 2025 09:05:03 GMT', DateTime);
  ExpectedDateTime := EncodeDateTime(2025, 1, 15, 9, 5, 3, 0);
  Assert.IsTrue(Result, 'Test 9: Should parse single-digit hour');
  Assert.AreEqual(ExpectedDateTime, DateTime, 'Test 9: Parsed date matches');

  // Test 10: Single-digit minute (Wed, 15 Jan 2025 14:07:03 GMT)
  Result := ALTryRfc822DateStrToUtcDateTime('Wed, 15-Jan-2025 14:07:03 GMT', DateTime);
  ExpectedDateTime := EncodeDateTime(2025, 1, 15, 14, 7, 3, 0);
  Assert.IsTrue(Result, 'Test 10: Should parse single-digit minute');
  Assert.AreEqual(ExpectedDateTime, DateTime, 'Test 10: Parsed date matches');

  // Test 11: Single-digit second (Wed, 15 Jan 2025 14:15:09 GMT)
  Result := ALTryRfc822DateStrToUtcDateTime('Wed, 15 Jan 2025 14:15:09 GMT', DateTime);
  ExpectedDateTime := EncodeDateTime(2025, 1, 15, 14, 15, 9, 0);
  Assert.IsTrue(Result, 'Test 11: Should parse single-digit second');
  Assert.AreEqual(ExpectedDateTime, DateTime, 'Test 11: Parsed date matches');

  // Test 12: Leap year date (Thu, 29 Feb 2024 12:00:00 GMT)
  Result := ALTryRfc822DateStrToUtcDateTime('Thu, 29 Feb 2024 12:00:00 GMT', DateTime);
  ExpectedDateTime := EncodeDateTime(2024, 2, 29, 12, 0, 0, 0);
  Assert.IsTrue(Result, 'Test 12: Should parse leap year date');
  Assert.AreEqual(ExpectedDateTime, DateTime, 'Test 12: Parsed date matches');

  // Test 13: UTC instead of GMT (Sun, 06 Nov 1994 08:49:37 UTC)
  Result := ALTryRfc822DateStrToUtcDateTime('Sun, 06-Nov-1994 08:49:37 UTC', DateTime);
  ExpectedDateTime := EncodeDateTime(1994, 11, 6, 8, 49, 37, 0);
  Assert.IsTrue(Result, 'Test 13: Should parse UTC timezone');
  Assert.AreEqual(ExpectedDateTime, DateTime, 'Test 13: Parsed date matches');

  // Test 14: UT instead of GMT (Sun, 06 Nov 1994 08:49:37 UT)
  Result := ALTryRfc822DateStrToUtcDateTime('Sun, 06 Nov 1994 08:49:37 UT', DateTime);
  ExpectedDateTime := EncodeDateTime(1994, 11, 6, 8, 49, 37, 0);
  Assert.IsTrue(Result, 'Test 14: Should parse UT timezone');
  Assert.AreEqual(ExpectedDateTime, DateTime, 'Test 14: Parsed date matches');

  // Test 15: EST (-0500) timezone (Sun, 06 Nov 1994 08:49:37 EST)
  Result := ALTryRfc822DateStrToUtcDateTime('Sun, 06 Nov 1994 08:49:37 EST', DateTime);
  ExpectedDateTime := EncodeDateTime(1994, 11, 6, 13, 49, 37, 0); // UTC = EST + 5 hours
  Assert.IsTrue(Result, 'Test 15: Should parse EST timezone');
  Assert.AreEqual(ExpectedDateTime, DateTime, 'Test 15: Parsed date matches');

  // Test 16: EDT (-0400) timezone (Sun, 06 Nov 1994 08:49:37 EDT)
  Result := ALTryRfc822DateStrToUtcDateTime('Sun, 06 Nov 1994 08:49:37 EDT', DateTime);
  ExpectedDateTime := EncodeDateTime(1994, 11, 6, 12, 49, 37, 0); // UTC = EDT + 4 hours
  Assert.IsTrue(Result, 'Test 16: Should parse EDT timezone');
  Assert.AreEqual(ExpectedDateTime, DateTime, 'Test 16: Parsed date matches');

  // Test 17: CST (-0600) timezone (Sun, 06 Nov 1994 08:49:37 CST)
  Result := ALTryRfc822DateStrToUtcDateTime('Sun, 06 Nov 1994 08:49:37 CST', DateTime);
  ExpectedDateTime := EncodeDateTime(1994, 11, 6, 14, 49, 37, 0); // UTC = CST + 6 hours
  Assert.IsTrue(Result, 'Test 17: Should parse CST timezone');
  Assert.AreEqual(ExpectedDateTime, DateTime, 'Test 17: Parsed date matches');

  // Test 18: CDT (-0500) timezone (Sun, 06 Nov 1994 08:49:37 CDT)
  Result := ALTryRfc822DateStrToUtcDateTime('Sun, 06 Nov 1994 08:49:37 CDT', DateTime);
  ExpectedDateTime := EncodeDateTime(1994, 11, 6, 13, 49, 37, 0); // UTC = CDT + 5 hours
  Assert.IsTrue(Result, 'Test 18: Should parse CDT timezone');
  Assert.AreEqual(ExpectedDateTime, DateTime, 'Test 18: Parsed date matches');

  // Test 19: MST (-0700) timezone (Sun, 06 Nov 1994 08:49:37 MST)
  Result := ALTryRfc822DateStrToUtcDateTime('Sun, 06 Nov 1994 08:49:37 MST', DateTime);
  ExpectedDateTime := EncodeDateTime(1994, 11, 6, 15, 49, 37, 0); // UTC = MST + 7 hours
  Assert.IsTrue(Result, 'Test 19: Should parse MST timezone');
  Assert.AreEqual(ExpectedDateTime, DateTime, 'Test 19: Parsed date matches');

  // Test 20: MDT (-0600) timezone (Sun, 06 Nov 1994 08:49:37 MDT)
  Result := ALTryRfc822DateStrToUtcDateTime('Sun, 06 Nov 1994 08:49:37 MDT', DateTime);
  ExpectedDateTime := EncodeDateTime(1994, 11, 6, 14, 49, 37, 0); // UTC = MDT + 6 hours
  Assert.IsTrue(Result, 'Test 20: Should parse MDT timezone');
  Assert.AreEqual(ExpectedDateTime, DateTime, 'Test 20: Parsed date matches');

  // Test 21: PST (-0800) timezone (Sun, 06 Nov 1994 08:49:37 PST)
  Result := ALTryRfc822DateStrToUtcDateTime('Sun, 06 Nov 1994 08:49:37 PST', DateTime);
  ExpectedDateTime := EncodeDateTime(1994, 11, 6, 16, 49, 37, 0); // UTC = PST + 8 hours
  Assert.IsTrue(Result, 'Test 21: Should parse PST timezone');
  Assert.AreEqual(ExpectedDateTime, DateTime, 'Test 21: Parsed date matches');

  // Test 22: PDT (-0700) timezone (Sun, 06 Nov 1994 08:49:37 PDT)
  Result := ALTryRfc822DateStrToUtcDateTime('Sun, 06 Nov 1994 08:49:37 PDT', DateTime);
  ExpectedDateTime := EncodeDateTime(1994, 11, 6, 15, 49, 37, 0); // UTC = PDT + 7 hours
  Assert.IsTrue(Result, 'Test 22: Should parse PDT timezone');
  Assert.AreEqual(ExpectedDateTime, DateTime, 'Test 22: Parsed date matches');

  // Test 23: No day of week (23 Aug 2004 06:48:46 -0700)
  Result := ALTryRfc822DateStrToUtcDateTime('23 Aug 2004 06:48:46 -0700', DateTime);
  ExpectedDateTime := EncodeDateTime(2004, 8, 23, 13, 48, 46, 0); // UTC = local + offset
  Assert.IsTrue(Result, 'Test 23: Should parse no day of week with -0700');
  Assert.AreEqual(ExpectedDateTime, DateTime, 'Test 23: Parsed date matches');

  // Test 25: Invalid month
  Result := ALTryRfc822DateStrToUtcDateTime('Sun, 06 Foo 1994 08:49:37 GMT', DateTime);
  Assert.IsFalse(Result, 'Test 25: Should fail on invalid month');

  // Test 26: Missing timezone
  Result := ALTryRfc822DateStrToUtcDateTime('Sun, 06 Nov 1994 08:49:37', DateTime);
  Assert.IsFalse(Result, 'Test 26: Should fail on missing timezone');

  // Test 27: Invalid date (day 32)
  Result := ALTryRfc822DateStrToUtcDateTime('Sun, 32 Nov 1994 08:49:37 GMT', DateTime);
  Assert.IsFalse(Result, 'Test 27: Should fail on invalid day');

  // Test 28: Invalid hour (24)
  Result := ALTryRfc822DateStrToUtcDateTime('Sun, 06 Nov 1994 24:49:37 GMT', DateTime);
  Assert.IsFalse(Result, 'Test 28: Should fail on invalid hour');

  // Test 29: Invalid minute (60)
  Result := ALTryRfc822DateStrToUtcDateTime('Sun, 06 Nov 1994 08:60:37 GMT', DateTime);
  Assert.IsFalse(Result, 'Test 29: Should fail on invalid minute');

  // Test 30: Invalid second (60)
  Result := ALTryRfc822DateStrToUtcDateTime('Sun, 06 Nov 1994 08:49:60 GMT', DateTime);
  Assert.IsFalse(Result, 'Test 30: Should fail on invalid second');

  // Test 31: Empty string
  Result := ALTryRfc822DateStrToUtcDateTime('', DateTime);
  Assert.IsFalse(Result, 'Test 31: Should fail on empty string');

  // Test 32: Non-RFC 1123/2822 format
  Result := ALTryRfc822DateStrToUtcDateTime('2025-09-15 23:00:00', DateTime);
  Assert.IsFalse(Result, 'Test 32: Should fail on non-RFC format');

  // Test 33: Missing comma after day
  Result := ALTryRfc822DateStrToUtcDateTime('Sun 06 Nov 1994 08:49:37 GMT', DateTime);
  Assert.IsFalse(Result, 'Test 33: Should fail on missing comma');

  // Test 35: Extra whitespace
  Result := ALTryRfc822DateStrToUtcDateTime('Sun,  06  Nov  1994  08:49:37  GMT', DateTime);
  ExpectedDateTime := EncodeDateTime(1994, 11, 6, 8, 49, 37, 0);
  Assert.IsTrue(Result, 'Test 35: Should parse with extra whitespace');
  Assert.AreEqual(ExpectedDateTime, DateTime, 'Test 35: Parsed date matches');

  // Test 36: Case insensitive day/month
  Result := ALTryRfc822DateStrToUtcDateTime('SUN, 06 NOV 1994 08:49:37 GMT', DateTime);
  ExpectedDateTime := EncodeDateTime(1994, 11, 6, 8, 49, 37, 0);
  Assert.IsTrue(Result, 'Test 36: Should parse case-insensitive day/month');
  Assert.AreEqual(ExpectedDateTime, DateTime, 'Test 36: Parsed date matches');

  // Test 39: Non-ASCII characters
  Result := ALTryRfc822DateStrToUtcDateTime('Sun, 06 Növ 1994 08:49:37 GMT', DateTime);
  Assert.IsFalse(Result, 'Test 39: Should fail on non-ASCII characters');

  // Test 40: Missing time
  Result := ALTryRfc822DateStrToUtcDateTime('Sun, 06 Nov 1994 GMT', DateTime);
  Assert.IsFalse(Result, 'Test 40: Should fail on missing time');

  // Test 41: Missing date
  Result := ALTryRfc822DateStrToUtcDateTime('Sun, 08:49:37 GMT', DateTime);
  Assert.IsFalse(Result, 'Test 41: Should fail on missing date');

  // Test 42: Malformed time (no colons)
  Result := ALTryRfc822DateStrToUtcDateTime('Sun, 06 Nov 1994 084937 GMT', DateTime);
  Assert.IsFalse(Result, 'Test 42: Should fail on malformed time');

  // Test 43: Invalid timezone (XYZ)
  Result := ALTryRfc822DateStrToUtcDateTime('Sun, 06 Nov 1994 08:49:37 XYZ', DateTime);
  Assert.IsFalse(Result, 'Test 43: Should fail on invalid timezone');

  // Test 44: Offset with no sign (+/-)
  Result := ALTryRfc822DateStrToUtcDateTime('Sun, 06 Nov 1994 08:49:37 0200', DateTime);
  Assert.IsFalse(Result, 'Test 44: Should fail on offset without sign');

  // Test 45: RFC 2822 with +0000 (equivalent to GMT)
  Result := ALTryRfc822DateStrToUtcDateTime('Sun, 06 Nov 1994 08:49:37 +0000', DateTime);
  ExpectedDateTime := EncodeDateTime(1994, 11, 6, 8, 49, 37, 0);
  Assert.IsTrue(Result, 'Test 45: Should parse +0000 offset');
  Assert.AreEqual(ExpectedDateTime, DateTime, 'Test 45: Parsed date matches');

  // Test 46: Hyphenated month with offset (Sun, 06-Nov-1994 08:49:37 +0200)
  Result := ALTryRfc822DateStrToUtcDateTime('Sun, 06-Nov-1994 08:49:37 +0200', DateTime);
  ExpectedDateTime := EncodeDateTime(1994, 11, 6, 6, 49, 37, 0); // UTC = local - offset
  Assert.IsTrue(Result, 'Test 46: Should parse hyphenated month with +0200');
  Assert.AreEqual(ExpectedDateTime, DateTime, 'Test 46: Parsed date matches');

  // Test 47: No day of week with GMT (06 Nov 1994 08:49:37 GMT)
  Result := ALTryRfc822DateStrToUtcDateTime('06 Nov 1994 08:49:37 GMT', DateTime);
  ExpectedDateTime := EncodeDateTime(1994, 11, 6, 8, 49, 37, 0);
  Assert.IsTrue(Result, 'Test 47: Should parse no day of week with GMT');
  Assert.AreEqual(ExpectedDateTime, DateTime, 'Test 47: Parsed date matches');

  // Test 48: Invalid offset format (e.g., +02:00)
  Result := ALTryRfc822DateStrToUtcDateTime('Sun, 06 Nov 1994 08:49:37 +02:00', DateTime);
  Assert.IsFalse(Result, 'Test 48: Should fail on invalid offset format');

  // Test 49: Extreme offset (+1200)
  Result := ALTryRfc822DateStrToUtcDateTime('Sun, 06 Nov 1994 08:49:37 +1200', DateTime);
  ExpectedDateTime := EncodeDateTime(1994, 11, 5, 20, 49, 37, 0); // UTC = local - 12 hours
  Assert.IsTrue(Result, 'Test 49: Should parse +1200 offset');
  Assert.AreEqual(ExpectedDateTime, DateTime, 'Test 49: Parsed date matches');

  // Test 50: Extreme offset (-1200)
  Result := ALTryRfc822DateStrToUtcDateTime('Sun, 06 Nov 1994 08:49:37 -1200', DateTime);
  ExpectedDateTime := EncodeDateTime(1994, 11, 6, 20, 49, 37, 0); // UTC = local + 12 hours
  Assert.IsTrue(Result, 'Test 50: Should parse -1200 offset');
  Assert.AreEqual(ExpectedDateTime, DateTime, 'Test 50: Parsed date matches');
end;

{***************************************************************}
procedure TALDUnitXTestHttp.TestALTryRfc822DateStrToUtcDateTimeW;
var
  DateTime: TDateTime;
  Result: Boolean;
  ExpectedDateTime: TDateTime;
begin
  // Test 1: Standard RFC 1123 date (Sun, 06 Nov 1994 08:49:37 GMT)
  Result := ALTryRfc822DateStrToUtcDateTime('Sun, 06 Nov 1994 08:49:37 GMT', DateTime);
  ExpectedDateTime := EncodeDateTime(1994, 11, 6, 8, 49, 37, 0);
  Assert.IsTrue(Result, 'Test 1: Should parse standard RFC 1123 date');
  Assert.AreEqual(ExpectedDateTime, DateTime, 'Test 1: Parsed date matches');

  // Test 2: RFC 1123 with hyphenated month (Sun, 06-Nov-1994 08:49:37 GMT)
  Result := ALTryRfc822DateStrToUtcDateTime('Sun, 06-Nov-1994 08:49:37 GMT', DateTime);
  ExpectedDateTime := EncodeDateTime(1994, 11, 6, 8, 49, 37, 0);
  Assert.IsTrue(Result, 'Test 2: Should parse hyphenated RFC 1123 date');
  Assert.AreEqual(ExpectedDateTime, DateTime, 'Test 2: Parsed date matches');

  // Test 3: RFC 2822 with +0200 offset (Sun, 06 Nov 1994 08:49:37 +0200)
  Result := ALTryRfc822DateStrToUtcDateTime('Sun, 06 Nov 1994 08:49:37 +0200', DateTime);
  ExpectedDateTime := EncodeDateTime(1994, 11, 6, 6, 49, 37, 0); // UTC = local - offset
  Assert.IsTrue(Result, 'Test 3: Should parse RFC 2822 with +0200 offset');
  Assert.AreEqual(ExpectedDateTime, DateTime, 'Test 3: Parsed date matches');

  // Test 4: RFC 2822 with -0700 offset (Tue, 23 Aug 2004 06:48:46 -0700)
  Result := ALTryRfc822DateStrToUtcDateTime('Tue, 23 Aug 2004 06:48:46 -0700', DateTime);
  ExpectedDateTime := EncodeDateTime(2004, 8, 23, 13, 48, 46, 0); // UTC = local + offset
  Assert.IsTrue(Result, 'Test 4: Should parse RFC 2822 with -0700 offset');
  Assert.AreEqual(ExpectedDateTime, DateTime, 'Test 4: Parsed date matches');

  // Test 5: Current date (2025-09-15 23:00:00 GMT)
  Result := ALTryRfc822DateStrToUtcDateTime('Mon, 15 Sep 2025 23:00:00 GMT', DateTime);
  ExpectedDateTime := EncodeDateTime(2025, 9, 15, 23, 0, 0, 0);
  Assert.IsTrue(Result, 'Test 5: Should parse current date');
  Assert.AreEqual(ExpectedDateTime, DateTime, 'Test 5: Parsed date matches');

  // Test 8: Single-digit day (Wed, 01 Jan 2025 08:05:03 GMT)
  Result := ALTryRfc822DateStrToUtcDateTime('Wed, 01 Jan 2025 08:05:03 GMT', DateTime);
  ExpectedDateTime := EncodeDateTime(2025, 1, 1, 8, 5, 3, 0);
  Assert.IsTrue(Result, 'Test 8: Should parse single-digit day');
  Assert.AreEqual(ExpectedDateTime, DateTime, 'Test 8: Parsed date matches');

  // Test 9: Single-digit hour (Wed, 15 Jan 2025 09:05:03 GMT)
  Result := ALTryRfc822DateStrToUtcDateTime('Wed, 15 Jan 2025 09:05:03 GMT', DateTime);
  ExpectedDateTime := EncodeDateTime(2025, 1, 15, 9, 5, 3, 0);
  Assert.IsTrue(Result, 'Test 9: Should parse single-digit hour');
  Assert.AreEqual(ExpectedDateTime, DateTime, 'Test 9: Parsed date matches');

  // Test 10: Single-digit minute (Wed, 15 Jan 2025 14:07:03 GMT)
  Result := ALTryRfc822DateStrToUtcDateTime('Wed, 15-Jan-2025 14:07:03 GMT', DateTime);
  ExpectedDateTime := EncodeDateTime(2025, 1, 15, 14, 7, 3, 0);
  Assert.IsTrue(Result, 'Test 10: Should parse single-digit minute');
  Assert.AreEqual(ExpectedDateTime, DateTime, 'Test 10: Parsed date matches');

  // Test 11: Single-digit second (Wed, 15 Jan 2025 14:15:09 GMT)
  Result := ALTryRfc822DateStrToUtcDateTime('Wed, 15 Jan 2025 14:15:09 GMT', DateTime);
  ExpectedDateTime := EncodeDateTime(2025, 1, 15, 14, 15, 9, 0);
  Assert.IsTrue(Result, 'Test 11: Should parse single-digit second');
  Assert.AreEqual(ExpectedDateTime, DateTime, 'Test 11: Parsed date matches');

  // Test 12: Leap year date (Thu, 29 Feb 2024 12:00:00 GMT)
  Result := ALTryRfc822DateStrToUtcDateTime('Thu, 29 Feb 2024 12:00:00 GMT', DateTime);
  ExpectedDateTime := EncodeDateTime(2024, 2, 29, 12, 0, 0, 0);
  Assert.IsTrue(Result, 'Test 12: Should parse leap year date');
  Assert.AreEqual(ExpectedDateTime, DateTime, 'Test 12: Parsed date matches');

  // Test 13: UTC instead of GMT (Sun, 06 Nov 1994 08:49:37 UTC)
  Result := ALTryRfc822DateStrToUtcDateTime('Sun, 06-Nov-1994 08:49:37 UTC', DateTime);
  ExpectedDateTime := EncodeDateTime(1994, 11, 6, 8, 49, 37, 0);
  Assert.IsTrue(Result, 'Test 13: Should parse UTC timezone');
  Assert.AreEqual(ExpectedDateTime, DateTime, 'Test 13: Parsed date matches');

  // Test 14: UT instead of GMT (Sun, 06 Nov 1994 08:49:37 UT)
  Result := ALTryRfc822DateStrToUtcDateTime('Sun, 06 Nov 1994 08:49:37 UT', DateTime);
  ExpectedDateTime := EncodeDateTime(1994, 11, 6, 8, 49, 37, 0);
  Assert.IsTrue(Result, 'Test 14: Should parse UT timezone');
  Assert.AreEqual(ExpectedDateTime, DateTime, 'Test 14: Parsed date matches');

  // Test 15: EST (-0500) timezone (Sun, 06 Nov 1994 08:49:37 EST)
  Result := ALTryRfc822DateStrToUtcDateTime('Sun, 06 Nov 1994 08:49:37 EST', DateTime);
  ExpectedDateTime := EncodeDateTime(1994, 11, 6, 13, 49, 37, 0); // UTC = EST + 5 hours
  Assert.IsTrue(Result, 'Test 15: Should parse EST timezone');
  Assert.AreEqual(ExpectedDateTime, DateTime, 'Test 15: Parsed date matches');

  // Test 16: EDT (-0400) timezone (Sun, 06 Nov 1994 08:49:37 EDT)
  Result := ALTryRfc822DateStrToUtcDateTime('Sun, 06 Nov 1994 08:49:37 EDT', DateTime);
  ExpectedDateTime := EncodeDateTime(1994, 11, 6, 12, 49, 37, 0); // UTC = EDT + 4 hours
  Assert.IsTrue(Result, 'Test 16: Should parse EDT timezone');
  Assert.AreEqual(ExpectedDateTime, DateTime, 'Test 16: Parsed date matches');

  // Test 17: CST (-0600) timezone (Sun, 06 Nov 1994 08:49:37 CST)
  Result := ALTryRfc822DateStrToUtcDateTime('Sun, 06 Nov 1994 08:49:37 CST', DateTime);
  ExpectedDateTime := EncodeDateTime(1994, 11, 6, 14, 49, 37, 0); // UTC = CST + 6 hours
  Assert.IsTrue(Result, 'Test 17: Should parse CST timezone');
  Assert.AreEqual(ExpectedDateTime, DateTime, 'Test 17: Parsed date matches');

  // Test 18: CDT (-0500) timezone (Sun, 06 Nov 1994 08:49:37 CDT)
  Result := ALTryRfc822DateStrToUtcDateTime('Sun, 06 Nov 1994 08:49:37 CDT', DateTime);
  ExpectedDateTime := EncodeDateTime(1994, 11, 6, 13, 49, 37, 0); // UTC = CDT + 5 hours
  Assert.IsTrue(Result, 'Test 18: Should parse CDT timezone');
  Assert.AreEqual(ExpectedDateTime, DateTime, 'Test 18: Parsed date matches');

  // Test 19: MST (-0700) timezone (Sun, 06 Nov 1994 08:49:37 MST)
  Result := ALTryRfc822DateStrToUtcDateTime('Sun, 06 Nov 1994 08:49:37 MST', DateTime);
  ExpectedDateTime := EncodeDateTime(1994, 11, 6, 15, 49, 37, 0); // UTC = MST + 7 hours
  Assert.IsTrue(Result, 'Test 19: Should parse MST timezone');
  Assert.AreEqual(ExpectedDateTime, DateTime, 'Test 19: Parsed date matches');

  // Test 20: MDT (-0600) timezone (Sun, 06 Nov 1994 08:49:37 MDT)
  Result := ALTryRfc822DateStrToUtcDateTime('Sun, 06 Nov 1994 08:49:37 MDT', DateTime);
  ExpectedDateTime := EncodeDateTime(1994, 11, 6, 14, 49, 37, 0); // UTC = MDT + 6 hours
  Assert.IsTrue(Result, 'Test 20: Should parse MDT timezone');
  Assert.AreEqual(ExpectedDateTime, DateTime, 'Test 20: Parsed date matches');

  // Test 21: PST (-0800) timezone (Sun, 06 Nov 1994 08:49:37 PST)
  Result := ALTryRfc822DateStrToUtcDateTime('Sun, 06 Nov 1994 08:49:37 PST', DateTime);
  ExpectedDateTime := EncodeDateTime(1994, 11, 6, 16, 49, 37, 0); // UTC = PST + 8 hours
  Assert.IsTrue(Result, 'Test 21: Should parse PST timezone');
  Assert.AreEqual(ExpectedDateTime, DateTime, 'Test 21: Parsed date matches');

  // Test 22: PDT (-0700) timezone (Sun, 06 Nov 1994 08:49:37 PDT)
  Result := ALTryRfc822DateStrToUtcDateTime('Sun, 06 Nov 1994 08:49:37 PDT', DateTime);
  ExpectedDateTime := EncodeDateTime(1994, 11, 6, 15, 49, 37, 0); // UTC = PDT + 7 hours
  Assert.IsTrue(Result, 'Test 22: Should parse PDT timezone');
  Assert.AreEqual(ExpectedDateTime, DateTime, 'Test 22: Parsed date matches');

  // Test 23: No day of week (23 Aug 2004 06:48:46 -0700)
  Result := ALTryRfc822DateStrToUtcDateTime('23 Aug 2004 06:48:46 -0700', DateTime);
  ExpectedDateTime := EncodeDateTime(2004, 8, 23, 13, 48, 46, 0); // UTC = local + offset
  Assert.IsTrue(Result, 'Test 23: Should parse no day of week with -0700');
  Assert.AreEqual(ExpectedDateTime, DateTime, 'Test 23: Parsed date matches');

  // Test 25: Invalid month
  Result := ALTryRfc822DateStrToUtcDateTime('Sun, 06 Foo 1994 08:49:37 GMT', DateTime);
  Assert.IsFalse(Result, 'Test 25: Should fail on invalid month');

  // Test 26: Missing timezone
  Result := ALTryRfc822DateStrToUtcDateTime('Sun, 06 Nov 1994 08:49:37', DateTime);
  Assert.IsFalse(Result, 'Test 26: Should fail on missing timezone');

  // Test 27: Invalid date (day 32)
  Result := ALTryRfc822DateStrToUtcDateTime('Sun, 32 Nov 1994 08:49:37 GMT', DateTime);
  Assert.IsFalse(Result, 'Test 27: Should fail on invalid day');

  // Test 28: Invalid hour (24)
  Result := ALTryRfc822DateStrToUtcDateTime('Sun, 06 Nov 1994 24:49:37 GMT', DateTime);
  Assert.IsFalse(Result, 'Test 28: Should fail on invalid hour');

  // Test 29: Invalid minute (60)
  Result := ALTryRfc822DateStrToUtcDateTime('Sun, 06 Nov 1994 08:60:37 GMT', DateTime);
  Assert.IsFalse(Result, 'Test 29: Should fail on invalid minute');

  // Test 30: Invalid second (60)
  Result := ALTryRfc822DateStrToUtcDateTime('Sun, 06 Nov 1994 08:49:60 GMT', DateTime);
  Assert.IsFalse(Result, 'Test 30: Should fail on invalid second');

  // Test 31: Empty string
  Result := ALTryRfc822DateStrToUtcDateTime('', DateTime);
  Assert.IsFalse(Result, 'Test 31: Should fail on empty string');

  // Test 32: Non-RFC 1123/2822 format
  Result := ALTryRfc822DateStrToUtcDateTime('2025-09-15 23:00:00', DateTime);
  Assert.IsFalse(Result, 'Test 32: Should fail on non-RFC format');

  // Test 33: Missing comma after day
  Result := ALTryRfc822DateStrToUtcDateTime('Sun 06 Nov 1994 08:49:37 GMT', DateTime);
  Assert.IsFalse(Result, 'Test 33: Should fail on missing comma');

  // Test 35: Extra whitespace
  Result := ALTryRfc822DateStrToUtcDateTime('Sun,  06  Nov  1994  08:49:37  GMT', DateTime);
  ExpectedDateTime := EncodeDateTime(1994, 11, 6, 8, 49, 37, 0);
  Assert.IsTrue(Result, 'Test 35: Should parse with extra whitespace');
  Assert.AreEqual(ExpectedDateTime, DateTime, 'Test 35: Parsed date matches');

  // Test 36: Case insensitive day/month
  Result := ALTryRfc822DateStrToUtcDateTime('SUN, 06 NOV 1994 08:49:37 GMT', DateTime);
  ExpectedDateTime := EncodeDateTime(1994, 11, 6, 8, 49, 37, 0);
  Assert.IsTrue(Result, 'Test 36: Should parse case-insensitive day/month');
  Assert.AreEqual(ExpectedDateTime, DateTime, 'Test 36: Parsed date matches');

  // Test 39: Non-ASCII characters
  Result := ALTryRfc822DateStrToUtcDateTime('Sun, 06 Növ 1994 08:49:37 GMT', DateTime);
  Assert.IsFalse(Result, 'Test 39: Should fail on non-ASCII characters');

  // Test 40: Missing time
  Result := ALTryRfc822DateStrToUtcDateTime('Sun, 06 Nov 1994 GMT', DateTime);
  Assert.IsFalse(Result, 'Test 40: Should fail on missing time');

  // Test 41: Missing date
  Result := ALTryRfc822DateStrToUtcDateTime('Sun, 08:49:37 GMT', DateTime);
  Assert.IsFalse(Result, 'Test 41: Should fail on missing date');

  // Test 42: Malformed time (no colons)
  Result := ALTryRfc822DateStrToUtcDateTime('Sun, 06 Nov 1994 084937 GMT', DateTime);
  Assert.IsFalse(Result, 'Test 42: Should fail on malformed time');

  // Test 43: Invalid timezone (XYZ)
  Result := ALTryRfc822DateStrToUtcDateTime('Sun, 06 Nov 1994 08:49:37 XYZ', DateTime);
  Assert.IsFalse(Result, 'Test 43: Should fail on invalid timezone');

  // Test 44: Offset with no sign (+/-)
  Result := ALTryRfc822DateStrToUtcDateTime('Sun, 06 Nov 1994 08:49:37 0200', DateTime);
  Assert.IsFalse(Result, 'Test 44: Should fail on offset without sign');

  // Test 45: RFC 2822 with +0000 (equivalent to GMT)
  Result := ALTryRfc822DateStrToUtcDateTime('Sun, 06 Nov 1994 08:49:37 +0000', DateTime);
  ExpectedDateTime := EncodeDateTime(1994, 11, 6, 8, 49, 37, 0);
  Assert.IsTrue(Result, 'Test 45: Should parse +0000 offset');
  Assert.AreEqual(ExpectedDateTime, DateTime, 'Test 45: Parsed date matches');

  // Test 46: Hyphenated month with offset (Sun, 06-Nov-1994 08:49:37 +0200)
  Result := ALTryRfc822DateStrToUtcDateTime('Sun, 06-Nov-1994 08:49:37 +0200', DateTime);
  ExpectedDateTime := EncodeDateTime(1994, 11, 6, 6, 49, 37, 0); // UTC = local - offset
  Assert.IsTrue(Result, 'Test 46: Should parse hyphenated month with +0200');
  Assert.AreEqual(ExpectedDateTime, DateTime, 'Test 46: Parsed date matches');

  // Test 47: No day of week with GMT (06 Nov 1994 08:49:37 GMT)
  Result := ALTryRfc822DateStrToUtcDateTime('06 Nov 1994 08:49:37 GMT', DateTime);
  ExpectedDateTime := EncodeDateTime(1994, 11, 6, 8, 49, 37, 0);
  Assert.IsTrue(Result, 'Test 47: Should parse no day of week with GMT');
  Assert.AreEqual(ExpectedDateTime, DateTime, 'Test 47: Parsed date matches');

  // Test 48: Invalid offset format (e.g., +02:00)
  Result := ALTryRfc822DateStrToUtcDateTime('Sun, 06 Nov 1994 08:49:37 +02:00', DateTime);
  Assert.IsFalse(Result, 'Test 48: Should fail on invalid offset format');

  // Test 49: Extreme offset (+1200)
  Result := ALTryRfc822DateStrToUtcDateTime('Sun, 06 Nov 1994 08:49:37 +1200', DateTime);
  ExpectedDateTime := EncodeDateTime(1994, 11, 5, 20, 49, 37, 0); // UTC = local - 12 hours
  Assert.IsTrue(Result, 'Test 49: Should parse +1200 offset');
  Assert.AreEqual(ExpectedDateTime, DateTime, 'Test 49: Parsed date matches');

  // Test 50: Extreme offset (-1200)
  Result := ALTryRfc822DateStrToUtcDateTime('Sun, 06 Nov 1994 08:49:37 -1200', DateTime);
  ExpectedDateTime := EncodeDateTime(1994, 11, 6, 20, 49, 37, 0); // UTC = local + 12 hours
  Assert.IsTrue(Result, 'Test 50: Should parse -1200 offset');
  Assert.AreEqual(ExpectedDateTime, DateTime, 'Test 50: Parsed date matches');
end;

initialization
  TDUnitX.RegisterTestFixture(TALDUnitXTestHttp);

end.