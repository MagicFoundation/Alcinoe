program ALTests;

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
  Alcinoe.AVLBinaryTree,
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
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  Alcinoe.CGI,
  {$ENDIF}
  Alcinoe.Cipher,
  Alcinoe.Common,
  {$IFDEF MSWINDOWS}
  Alcinoe.Execute,
  Alcinoe.ExprEval,
  Alcinoe.FBX.Base,
  Alcinoe.FBX.Client,
  Alcinoe.FBX.Consts,
  Alcinoe.FBX.Error,
  Alcinoe.FBX.Lib,
  {$ENDIF}
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
  {$IFDEF ANDROID}
  Alcinoe.FMX.NativeView.Android,
  {$ENDIF}
  {$IFDEF iOS}
  Alcinoe.FMX.NativeView.iOS,
  {$ENDIF}
  Alcinoe.FMX.Objects,
  {$IFDEF iOS}
  Alcinoe.FMX.ScrollBox.iOS,
  {$ENDIF}
  Alcinoe.FMX.StdCtrls,
  Alcinoe.FMX.TabControl,
  {$IFDEF MSWINDOWS}
  Alcinoe.FMX.Trayicon,
  {$ENDIF}
  Alcinoe.FMX.Types3D,
  Alcinoe.FMX.VKontakte,
  Alcinoe.FMX.VideoPlayer,
  Alcinoe.FMX.WebRTC,
  {$IFDEF MSWINDOWS}
  Alcinoe.FTP.Client,
  Alcinoe.FTP.Client.WinINet,
  {$ENDIF}
  Alcinoe.Files,
  {$IFDEF MSWINDOWS}
  Alcinoe.GSMComm,
  {$ENDIF}
  Alcinoe.HTML,
  Alcinoe.HTTP.Client,
  Alcinoe.HTTP.Client.Net,
  Alcinoe.HTTP.Client.Net.Pool,
  {$IFDEF MSWINDOWS}
  Alcinoe.HTTP.Client.WinHTTP,
  Alcinoe.HTTP.Client.WinINet,
  Alcinoe.ImageMagick,
  Alcinoe.IniFiles,
  Alcinoe.InternetMessages,
  Alcinoe.IsapiHTTP,
  {$ENDIF}
  Alcinoe.JSONDoc,
  {$IFDEF MSWINDOWS}
  Alcinoe.LibPhoneNumber,
  Alcinoe.MemCached.Client,
  Alcinoe.Mime,
  Alcinoe.MongoDB.Client,
  Alcinoe.MultiPartParser,
  Alcinoe.MySql.Client,
  Alcinoe.MySql.Wrapper,
  Alcinoe.NNTP.Client,
  Alcinoe.POP3.Client,
  Alcinoe.PhpRunner,
  {$ENDIF}
  Alcinoe.QuickSortList,
  {$IFDEF MSWINDOWS}
  Alcinoe.RTTI,
  Alcinoe.SMTP.Client,
  Alcinoe.SphinxQL.Client,
  Alcinoe.Sqlite3.Client,
  Alcinoe.Sqlite3.Wrapper,
  {$ENDIF}
  Alcinoe.StringList,
  Alcinoe.StringUtils,
  {$IFDEF MSWINDOWS}
  //Alcinoe.TbbMM,
  Alcinoe.WebSocket.Client,
  Alcinoe.WebSocket.Client.WinHTTP,
  Alcinoe.WebSpider,
  Alcinoe.WinApi.Common,
  Alcinoe.WinSock,
  Alcinoe.XMLDoc,
  Alcinoe.ZLibEx,
  Alcinoe.ZLibExGZ,
  {$ENDIF}
  {$IFDEF IOS}
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
  Alcinoe.iOSApi.VKontakte,
  Alcinoe.iOSApi.WebRTC,
  {$ENDIF}
  Grijjy.ErrorReporting,
  Grijjy.SymbolTranslator,
  {$IFDEF MSWINDOWS}
  ZLibEx,
  ZLibExApi,
  ZLibExGZ,
  {$ENDIF}
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

