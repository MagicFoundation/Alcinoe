unit ALTestCipher;

interface

uses
  System.Diagnostics,
  Alcinoe.Common,
  Alcinoe.StringUtils,
  DUnitX.TestFramework;

type

  [TestFixture]
  TALTestCipher = class
  strict private
    fFullAsciiCharset: TArray<AnsiChar>;
    fStopWatchAlcinoe: TStopwatch;
    fStopWatchDELPHI: TStopwatch;
    procedure CheckExecutionTime(const ARatio: single = 1.2);
  public
    constructor create;
    [Setup]
    procedure Setup;
    //[TearDown]
    //procedure TearDown;
    [Test]
    procedure TestALStringHashCrc32;
  end;

implementation

uses
  IdHashCRC,
  IDGlobal,
  Alcinoe.Cipher;

{*******************************}
constructor TALTestCipher.create;
begin
  Setlength(fFullAsciiCharset, 255);
  for var I := Low(fFullAsciiCharset) to High(fFullAsciiCharset) do
    fFullAsciiCharset[i] := AnsiChar(i); // will contain some invalid UTF-8 chars
end;

{****************************}
procedure TALTestCipher.Setup;
begin
  fStopWatchAlcinoe := TStopwatch.Create;
  fStopWatchDELPHI := TStopwatch.Create;
end;

{*********************************************************************}
procedure TALTestCipher.CheckExecutionTime(const ARatio: single = 1.2);
begin
  {$IF defined(debug) or defined(Win32)}
  //In debug we have overflow checking and range checking so that mean
  //that the execution time will be much slower that the Delphi RTL so skip it
  //In Win32 we remove all ASM (fastcode heritage) than the delphi RTL have
  //so we will be othen much more slower than the Delphi RTL
  Writeln(ALFormatW('CheckExecutionTime Skipped - %0.0f ms for Alcinoe vs %0.0f ms for Delphi (%0.1fx faster)', [fStopWatchAlcinoe.Elapsed.TotalMilliseconds, fStopWatchDELPHI.Elapsed.TotalMilliseconds, fStopWatchDELPHI.Elapsed.TotalMilliseconds / fStopWatchAlcinoe.Elapsed.TotalMilliseconds], ALDefaultFormatSettingsW));
  {$ELSE}
  if fStopWatchAlcinoe.Elapsed.TotalMilliseconds > fStopWatchDELPHI.Elapsed.TotalMilliseconds * ARatio then
    Assert.Fail(ALFormatW('Time too long (%0.0f ms for Alcinoe vs %0.0f ms for Delphi)', [fStopWatchAlcinoe.Elapsed.TotalMilliseconds, fStopWatchDELPHI.Elapsed.TotalMilliseconds], ALDefaultFormatSettingsW))
  else
    //https://github.com/VSoftTechnologies/DUnitX/issues/319
    Writeln(ALFormatW('%0.0f ms for Alcinoe vs %0.0f ms for Delphi (%0.1fx faster)', [fStopWatchAlcinoe.Elapsed.TotalMilliseconds, fStopWatchDELPHI.Elapsed.TotalMilliseconds, fStopWatchDELPHI.Elapsed.TotalMilliseconds / fStopWatchAlcinoe.Elapsed.TotalMilliseconds], ALDefaultFormatSettingsW));
  {$ENDIF}
end;

{********************************************}
procedure TALTestCipher.TestALStringHashCrc32;
var LArrayOfByte: TIdBytes;
begin
  if not ALTestCrc32cImplementation then
    Assert.Fail;
  //--
  for var I := 0 to 10000 do begin
    var LStrIn := ALRandomStrA(ALRandom32(16384), FFullAsciiCharset);
    if length(LStrIn) = 0 then continue;
    var LHash := TIdHashCRC32.Create;
    try
      setlength(LArrayOfByte, length(LStrIn));
      ALmove(pbyte(LStrIn)[low(LStrIn)],LArrayOfByte[0],length(LStrIn));
      fStopWatchDELPHI.Start; {-} LHash.HashBytesAsHex(LArrayOfByte); {-} fStopWatchDELPHI.Stop;
    finally
      LHash.Free;
    end;
    //--
    fStopWatchAlcinoe.Start; {-} ALStringHashCrc32c(LStrIn); {-} fStopWatchAlcinoe.Stop;
  end;
  //--
  CheckExecutionTime(0.04{ARatio});
end;

end.
