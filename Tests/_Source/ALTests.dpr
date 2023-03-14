program ALTests;

{$I Alcinoe.inc}

{$IFDEF MSWINDOWS}
  {$IFNDEF TESTINSIGHT}
  {$APPTYPE CONSOLE}
  {$ENDIF}
  {$STRONGLINKTYPES ON}
{$ENDIF}

uses
  {$IFDEF MSWINDOWS}
    {$IFDEF TESTINSIGHT}
    TestInsight.DUnitX,
    {$ELSE}
    DUnitX.Loggers.Console,
    DUnitX.Loggers.Xml.NUnit,
    {$ENDIF}
    DUnitX.TestFramework,
  {$ENDIF}
  //---------------------
  //include all units to be sure
  //that everything compile well
  {$IFDEF MSWINDOWS}
  Alcinoe.CGI,
  Alcinoe.Execute,
  Alcinoe.FBX.Base,
  Alcinoe.FBX.Client,
  Alcinoe.FBX.Consts,
  Alcinoe.FBX.Error,
  Alcinoe.FBX.Lib,
  Alcinoe.FMX.Trayicon,
  Alcinoe.FTP.Client,
  Alcinoe.FTP.Client.WinINet,
  Alcinoe.GSMComm,
  Alcinoe.HTTP.Client.WinHTTP,
  Alcinoe.HTTP.Client.WinINet,
  Alcinoe.ImageMagick,
  Alcinoe.IniFiles,
  Alcinoe.IsapiHTTP,
  Alcinoe.LibPhoneNumber,
  Alcinoe.MemCached.Client,
  Alcinoe.MongoDB.Client,
  Alcinoe.MySql.Client,
  Alcinoe.MySql.Wrapper,
  Alcinoe.NNTP.Client,
  Alcinoe.POP3.Client,
  Alcinoe.PhpRunner,
  Alcinoe.SMTP.Client,
  Alcinoe.SphinxQL.Client,
  Alcinoe.Sqlite3.Client,
  Alcinoe.Sqlite3.Wrapper,
  //Alcinoe.TbbMM,
  Alcinoe.WebSocket.Client.WinHTTP,
  Alcinoe.WebSpider,
  Alcinoe.WinApi.Common,
  Alcinoe.WinSock,
  Alcinoe.ZLibEx,
  Alcinoe.ZLibExGZ,
  ZLibEx,
  ZLibExApi,
  ZLibExGZ,
  {$ENDIF}
  {$IFDEF ANDROID}
  Alcinoe.AndroidApi.AndroidX,
  Alcinoe.AndroidApi.BillingClient,
  Alcinoe.AndroidApi.Common,
  Alcinoe.AndroidApi.ExoPlayer,
  Alcinoe.AndroidApi.Facebook,
  Alcinoe.AndroidApi.Firebase,
  Alcinoe.AndroidApi.Google,
  Alcinoe.AndroidApi.InstallReferrer,
  Alcinoe.AndroidApi.VKontakte,
  Alcinoe.AndroidApi.WebRTC,
  Alcinoe.FMX.NativeView.Android,
  {$ENDIF}
  {$IFDEF iOS}
  Alcinoe.FMX.NativeView.iOS,
  Alcinoe.FMX.ScrollBox.iOS,
  Alcinoe.iOSApi.AdSupport,
  Alcinoe.iOSApi.AudioToolbox,
  Alcinoe.iOSApi.AuthenticationServices,
  Alcinoe.iOSApi.BackgroundTasks,
  Alcinoe.iOSApi.FacebookCoreKit,
  Alcinoe.iOSApi.FacebookLoginKit,
  Alcinoe.iOSApi.FacebookShareKit,
  Alcinoe.iOSApi.FirebaseCore,
  Alcinoe.iOSApi.FirebaseMessaging,
  Alcinoe.iOSApi.ImageIO,
  Alcinoe.iOSApi.MessageUI,
  Alcinoe.iOSApi.Photos,
  {$IFNDEF IOSSIMULATOR}
  //[DCC Error] E2597 ld: building for iOS Simulator, but linking in dylib built for iOS, for architecture arm64
  //https://stackoverflow.com/questions/63607158/xcode-building-for-ios-simulator-but-linking-in-an-object-file-built-for-ios-f
  {$IFNDEF ALCompilerVersionSupported}
    {$MESSAGE WARN 'Check if a new version of VKontakte/WebRTC are available with a support for IOSSIMULATOR and adjust the IFDEF'}
  {$IFEND}
  Alcinoe.iOSApi.VKontakte,
  Alcinoe.iOSApi.WebRTC,
  {$ENDIF}
  {$ENDIF}
  {$IFNDEF IOSSIMULATOR}
  //[DCC Error] E2597 ld: building for iOS Simulator, but linking in dylib built for iOS, for architecture arm64
  //https://stackoverflow.com/questions/63607158/xcode-building-for-ios-simulator-but-linking-in-an-object-file-built-for-ios-f
  {$IFNDEF ALCompilerVersionSupported}
    {$MESSAGE WARN 'Check if a new version of VKontakte/WebRTC are available with a support for IOSSIMULATOR and adjust the IFDEF'}
  {$IFEND}
  Alcinoe.FMX.VKontakte,
  Alcinoe.FMX.WebRTC,
  {$ENDIF}
  Alcinoe.AVLBinaryTree,
  Alcinoe.ExprEval,
  Alcinoe.InternetMessages,
  Alcinoe.Mime,
  Alcinoe.MultiPartParser,
  Alcinoe.RTTI,
  Alcinoe.WebSocket.Client,
  Alcinoe.Cipher,
  Alcinoe.Common,
  Alcinoe.FMX.Ani,
  Alcinoe.FMX.BreakText,
  Alcinoe.FMX.Common,
  Alcinoe.FMX.Confetti,
  Alcinoe.FMX.DatePickerDialog,
  //Alcinoe.FMX.DesignEditors,
  Alcinoe.FMX.Edit,
  Alcinoe.FMX.FacebookCore,
  Alcinoe.FMX.FacebookLogin,
  Alcinoe.FMX.FacebookShare,
  Alcinoe.FMX.FilterEffects,
  Alcinoe.FMX.FirebaseCore,
  Alcinoe.FMX.FirebaseMessaging,
  Alcinoe.FMX.Graphics,
  Alcinoe.FMX.InertialMovement,
  Alcinoe.FMX.Layouts,
  Alcinoe.FMX.Memo,
  Alcinoe.FMX.Objects,
  Alcinoe.FMX.StdCtrls,
  Alcinoe.FMX.TabControl,
  Alcinoe.FMX.Types3D,
  Alcinoe.FMX.VideoPlayer,
  Alcinoe.Files,
  Alcinoe.HTML,
  Alcinoe.HTTP.Client,
  Alcinoe.HTTP.Client.Net,
  Alcinoe.HTTP.Client.Net.Pool,
  Alcinoe.JSONDoc,
  Alcinoe.QuickSortList,
  Alcinoe.StringList,
  Alcinoe.StringUtils,
  Alcinoe.XMLDoc,
  Grijjy.ErrorReporting,
  Grijjy.SymbolTranslator,
  //---------------------
  {$IFDEF MSWINDOWS}
  ALTestStrings in 'ALTestStrings.pas',
  ALTestCipher in 'ALTestCipher.pas',
  {$ENDIF}
  System.SysUtils;

{$IFDEF MSWINDOWS}
  {$IFNDEF TESTINSIGHT}
  var
    runner: ITestRunner;
    results: IRunResults;
    logger: ITestLogger;
    nunitLogger : ITestLogger;
  {$ENDIF}
{$ENDIF}
begin
{$IFDEF MSWINDOWS}

    {$IFDEF DEBUG}
    ReportMemoryLeaksOnShutdown := True;
    {$ELSE}
    ReportMemoryLeaksOnShutdown := False;
    {$ENDIF}
    SetMultiByteConversionCodePage(CP_UTF8);

    {$IFDEF TESTINSIGHT}
    TestInsight.DUnitX.RunRegisteredTests;
    {$ELSE}
    try
      //Check command line options, will exit if invalid
      TDUnitX.CheckCommandLine;
      //Create the test runner
      runner := TDUnitX.CreateRunner;
      //Tell the runner to use RTTI to find Fixtures
      runner.UseRTTI := True;
      //When true, Assertions must be made during tests;
      runner.FailsOnNoAsserts := False;

      //tell the runner how we will log things
      //Log to the console window if desired
      if TDUnitX.Options.ConsoleMode <> TDunitXConsoleMode.Off then
      begin
        logger := TDUnitXConsoleLogger.Create(TDUnitX.Options.ConsoleMode = TDunitXConsoleMode.Quiet);
        runner.AddLogger(logger);
      end;
      //Generate an NUnit compatible XML File
      nunitLogger := TDUnitXXMLNUnitFileLogger.Create(TDUnitX.Options.XMLOutputFile);
      runner.AddLogger(nunitLogger);

      //Run tests
      results := runner.Execute;
      if not results.AllPassed then
        System.ExitCode := EXIT_ERRORS;

      {$IFNDEF CI}
      //We don't want this happening when running under CI.
      if TDUnitX.Options.ExitBehavior = TDUnitXExitBehavior.Pause then
      begin
        System.Write('Done.. press <Enter> key to quit.');
        System.Readln;
      end;
      {$ENDIF}
    except
      on E: Exception do
        System.Writeln(E.ClassName, ': ', E.Message);
    end;
    {$ENDIF}

{$ENDIF}
end.

