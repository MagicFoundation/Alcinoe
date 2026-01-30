program ALDUnitXTests;

{$I Alcinoe.inc}

{$IFDEF MSWINDOWS}
  {$IFNDEF TESTINSIGHT}
  {$APPTYPE CONSOLE}
  {$ENDIF}
  {$STRONGLINKTYPES ON}
{$ENDIF}

uses
  {$IF defined(SKIA)}
  FMX.Skia,
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  {$IFDEF TESTINSIGHT}
  TestInsight.DUnitX,
  {$ELSE}
  DUnitX.Loggers.Console,
  DUnitX.Loggers.Xml.NUnit,
  {$ENDIF}
  DUnitX.TestFramework,
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  //Alcinoe.TbbMM,
  Alcinoe.Execute,
  Alcinoe.FMX.NativeView.Win,
  Alcinoe.FMX.Trayicon,
  Alcinoe.HTTP.Client.WinHTTP,
  Alcinoe.HTTP.Server.HttpSys,
  Alcinoe.ImageMagick,
  Alcinoe.IniFiles,
  Alcinoe.MongoDB.Client,
  Alcinoe.MongoDB.Wrapper,
  Alcinoe.Net.Socket.Client.Win,
  Alcinoe.Net.TLS.Client.SChannel,
  Alcinoe.SMTP.Client,
  Alcinoe.ServiceUtils,
  Alcinoe.WinApi.Http,
  Alcinoe.WinApi.SChannel,
  Alcinoe.WinApi.SSPI,
  Alcinoe.WinApi.Windows,
  Alcinoe.WinApi.WinError,
  {$ENDIF}
  {$IFDEF ANDROID}
  Alcinoe.AndroidApi.AndroidX,
  Alcinoe.AndroidApi.AndroidX.Media3,
  Alcinoe.AndroidApi.App,
  Alcinoe.AndroidApi.AppCompat,
  Alcinoe.AndroidApi.BillingClient,
  Alcinoe.AndroidApi.Crypto,
  Alcinoe.AndroidApi.Facebook,
  Alcinoe.AndroidApi.Firebase.Analytics,
  Alcinoe.AndroidApi.Firebase.Messaging,
  Alcinoe.AndroidApi.Google,
  Alcinoe.AndroidApi.InstallReferrer,
  Alcinoe.AndroidApi.JavaTypes,
  Alcinoe.AndroidApi.Os,
  Alcinoe.AndroidApi.RenderScript,
  Alcinoe.AndroidApi.Security,
  Alcinoe.AndroidApi.VKontakte,
  Alcinoe.AndroidApi.WebKit,
  Alcinoe.AndroidApi.Widget,
  Alcinoe.Androidapi.GraphicsContentViewText,
  Alcinoe.FMX.NativeView.Android,
  {$ENDIF}
  {$IFDEF iOS}
  Alcinoe.FMX.NativeView.iOS,
  Alcinoe.iOSApi.AdSupport,
  Alcinoe.iOSApi.AudioToolbox,
  Alcinoe.iOSApi.AuthenticationServices,
  Alcinoe.iOSApi.BackgroundTasks,
  Alcinoe.iOSApi.FacebookCoreKit,
  Alcinoe.iOSApi.FacebookLoginKit,
  Alcinoe.iOSApi.FacebookShareKit,
  Alcinoe.iOSApi.FirebaseCore,
  Alcinoe.iOSApi.FirebaseMessaging,
  Alcinoe.iOSApi.MessageUI,
  Alcinoe.iOSApi.Photos,
  Alcinoe.iOSapi.PhotosUI,
  {$IFNDEF IOSSIMULATOR}
  Alcinoe.iOSApi.VKontakte,
  {$ENDIF}
  Alcinoe.iOSapi.CoreFoundation,
  Alcinoe.iOSapi.CoreVideo,
  {$ENDIF}
  {$IFNDEF IOSSIMULATOR}
  Alcinoe.FMX.VKontakte,
  {$ENDIF}
  {$IFDEF ALMacOS}
  Alcinoe.FMX.NativeView.Mac,
  Alcinoe.Macapi.AppKit,
  Alcinoe.Macapi.QuartzCore,
  {$ENDIF}
  //Alcinoe.CodeProfiler,
  //Alcinoe.FMX.DesignEditors,
  Alcinoe.BroadcastReceiver,
  Alcinoe.Cipher,
  Alcinoe.Common,
  Alcinoe.FMX.Ani,
  Alcinoe.FMX.BreakText,
  Alcinoe.FMX.CacheEngines,
  Alcinoe.FMX.Common,
  Alcinoe.FMX.Confetti,
  Alcinoe.FMX.Controls,
  Alcinoe.FMX.CustomStyles,
  Alcinoe.FMX.DatePickerDialog,
  Alcinoe.FMX.Dialogs,
  Alcinoe.FMX.Dynamic.Common,
  Alcinoe.FMX.Dynamic.Controls,
  Alcinoe.FMX.Dynamic.Layouts,
  Alcinoe.FMX.Dynamic.ListBox,
  Alcinoe.FMX.Dynamic.Objects,
  Alcinoe.FMX.Dynamic.PageController,
  Alcinoe.FMX.Dynamic.StdCtrls,
  Alcinoe.FMX.Dynamic.VideoPlayer,
  Alcinoe.FMX.Edit,
  Alcinoe.FMX.ErrorReporting,
  Alcinoe.FMX.Facebook.Core,
  Alcinoe.FMX.Facebook.Login,
  Alcinoe.FMX.Facebook.Share,
  Alcinoe.FMX.FilterEffects,
  Alcinoe.FMX.Firebase.Core,
  Alcinoe.FMX.Firebase.Messaging,
  Alcinoe.FMX.Forms,
  Alcinoe.FMX.GeoLocation.Sensor,
  Alcinoe.FMX.Graphics,
  Alcinoe.FMX.Layouts,
  Alcinoe.FMX.LoadingOverlay,
  Alcinoe.FMX.Materials.Canvas,
  Alcinoe.FMX.MediaPicker,
  Alcinoe.FMX.Memo,
  Alcinoe.FMX.NativeControl,
  Alcinoe.FMX.NotificationService,
  Alcinoe.FMX.Objects,
  Alcinoe.FMX.PageController,
  Alcinoe.FMX.ScrollEngine,
  Alcinoe.FMX.Sheets,
  Alcinoe.FMX.Snackbar,
  Alcinoe.FMX.StdCtrls,
  Alcinoe.FMX.Styles,
  Alcinoe.FMX.Types3D,
  Alcinoe.FMX.UserPreferences,
  Alcinoe.FMX.VideoPlayer,
  Alcinoe.FMX.WebBrowser,
  Alcinoe.Files,
  Alcinoe.GuardianThread,
  Alcinoe.HTML,
  Alcinoe.HTTP,
  Alcinoe.HTTP.Client,
  Alcinoe.HTTP.Client.Net,
  Alcinoe.HTTP.Client.Net.Pool,
  Alcinoe.HTTP.Server,
  Alcinoe.HTTP.Worker,
  Alcinoe.InternetMessages,
  Alcinoe.JSONDoc,
  Alcinoe.Localization,
  Alcinoe.Mime.ContentTypes,
  Alcinoe.Mime.Multipart,
  Alcinoe.Net,
  Alcinoe.Net.Socket.Client,
  Alcinoe.Net.TLS.Client,
  Alcinoe.RTTI,
  Alcinoe.StringList,
  Alcinoe.StringUtils,
  Alcinoe.Url,
  Alcinoe.XMLDoc,
  Grijjy.ErrorReporting,
  Grijjy.SymbolTranslator,
  {$IFDEF MSWINDOWS}
  ALDUnitXTestStringUtils in 'ALDUnitXTestStringUtils.pas',
  ALDUnitXTestCipher in 'ALDUnitXTestCipher.pas',
  ALDUnitXTestHtml in 'ALDUnitXTestHtml.pas',
  {$ENDIF}
  System.SysUtils,
  ALDUnitXTestHttp in 'ALDUnitXTestHttp.pas',
  ALDUnitXTestXmlDoc in 'ALDUnitXTestXmlDoc.pas',
  ALDUnitXTestNet in 'ALDUnitXTestNet.pas',
  ALDUnitXTestUrl in 'ALDUnitXTestUrl.pas';

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

{$IF defined(SKIA)}
  GlobalUseSkia := True;
{$ENDIF}

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

      // When the test executable is started under the Delphi debugger,
      // DebugHook is non-zero. In that case we enable the Pause exit behavior
      // so the console window stays open and the results remain visible.
      {$IF defined(WIN32) or defined(WIN64)}
      {$WARN SYMBOL_PLATFORM OFF}
      if DebugHook <> 0 then
        TDUnitX.Options.ExitBehavior := TDUnitXExitBehavior.Pause;
      {$WARN SYMBOL_PLATFORM ON}
      {$ENDIF}

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