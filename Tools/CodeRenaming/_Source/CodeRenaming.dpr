program CodeRenaming;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.Classes,
  System.SysUtils,
  System.ioutils,
  System.types,
  System.Character,
  system.AnsiStrings, // H2443 Inline function 'ALStringReplaceA' has not been expanded because unit 'System.AnsiStrings' is not specified in USES list
  Alcinoe.StringList,
  Alcinoe.Files,
  Alcinoe.StringUtils,
  Alcinoe.Execute,
  Alcinoe.Common;

begin

  try

    {$IFDEF DEBUG}
    ReportMemoryleaksOnSHutdown := True;
    {$ENDIF}
    SetMultiByteConversionCodePage(CP_UTF8);

    Writeln('This tool is designed to rename the old Alcinoe function/unit name (e.g. ALHTTPClient)');
    Writeln('to the new Alcinoe function/unit name (e.g. Alcinoe.Http.Client) in your source code.');
    Writeln('This can help ensure that your code remains compatible with the latest version of');
    Writeln('Alcinoe and that you can take advantage of new features and improvements.');
    Writeln('');
    Writeln('Enter the root directory (Please make backup before):');
    var LRootDirectory: String;
    readln(LRootDirectory);
    if (LRootDirectory = '') or (not TDirectory.Exists(LRootDirectory)) then
      raise Exception.Create('Root directory does not exist');
    Writeln('');

    var LOldToNewNames := TALStringListA.Create;
    try

      //Unit names
      LOldToNewNames.add('ALAndroidApi=Alcinoe.AndroidApi.Common');
      LOldToNewNames.add('ALAndroidBillingClientApi=Alcinoe.AndroidApi.BillingClient');
      LOldToNewNames.add('ALAndroidExoPlayerApi=Alcinoe.AndroidApi.ExoPlayer');
      LOldToNewNames.add('ALAndroidFacebookApi=Alcinoe.AndroidApi.Facebook');
      LOldToNewNames.add('ALAndroidFirebaseApi=Alcinoe.AndroidApi.Firebase');
      LOldToNewNames.add('ALAndroidGoogleApi=Alcinoe.AndroidApi.Google');
      LOldToNewNames.add('ALAndroidInstallReferrerApi=Alcinoe.AndroidApi.InstallReferrer');
      LOldToNewNames.add('ALAndroidVKontakteApi=Alcinoe.AndroidApi.VKontakte');
      LOldToNewNames.add('ALAndroidWebRTCApi=Alcinoe.AndroidApi.WebRTC');
      LOldToNewNames.add('ALAndroidXApi=Alcinoe.AndroidApi.AndroidX');
      LOldToNewNames.add('ALIosAdSupportApi=Alcinoe.iOSApi.AdSupport');
      LOldToNewNames.add('ALIosAudioToolboxApi=Alcinoe.iOSApi.AudioToolbox');
      LOldToNewNames.add('ALIosAuthenticationServicesApi=Alcinoe.iOSApi.AuthenticationServices');
      LOldToNewNames.add('ALIosBackgroundTasksApi=Alcinoe.iOSApi.BackgroundTasks');
      LOldToNewNames.add('ALIosFacebookCoreKitApi=Alcinoe.iOSApi.FacebookCoreKit');
      LOldToNewNames.add('ALIosFacebookLoginKitApi=Alcinoe.iOSApi.FacebookLoginKit');
      LOldToNewNames.add('ALIosFacebookShareKitApi=Alcinoe.iOSApi.FacebookShareKit');
      LOldToNewNames.add('ALIosFirebaseCoreApi=Alcinoe.iOSApi.FirebaseCore');
      LOldToNewNames.add('ALIosFirebaseMessagingApi=Alcinoe.iOSApi.FirebaseMessaging');
      LOldToNewNames.add('ALIosImageIOApi=Alcinoe.iOSApi.ImageIO');
      LOldToNewNames.add('ALIosMessageUIApi=Alcinoe.iOSApi.MessageUI');
      LOldToNewNames.add('ALIosPhotosApi=Alcinoe.iOSApi.Photos');
      LOldToNewNames.add('ALIosVKontakteApi=Alcinoe.iOSApi.VKontakte');
      LOldToNewNames.add('ALIosWebRTCApi=Alcinoe.iOSApi.WebRTC');
      LOldToNewNames.add('ALAVLBinaryTree=Alcinoe.AVLBinaryTree');
      LOldToNewNames.add('ALCGI=Alcinoe.CGI');
      LOldToNewNames.add('ALCipher=Alcinoe.Cipher');
      LOldToNewNames.add('ALCommon=Alcinoe.Common');
      LOldToNewNames.add('ALExecute=Alcinoe.Execute');
      LOldToNewNames.add('ALExprEval=Alcinoe.ExprEval');
      LOldToNewNames.add('ALFBXBase=Alcinoe.FBX.Base');
      LOldToNewNames.add('ALFBXClient=Alcinoe.FBX.Client');
      LOldToNewNames.add('ALFBXConst=Alcinoe.FBX.Consts');
      LOldToNewNames.add('ALFBXError=Alcinoe.FBX.Error');
      LOldToNewNames.add('ALFBXLib=Alcinoe.FBX.Lib');
      LOldToNewNames.add('ALFiles=Alcinoe.Files');
      LOldToNewNames.add('ALFmxAni=Alcinoe.FMX.Ani');
      LOldToNewNames.add('ALFmxBreakText=Alcinoe.FMX.BreakText');
      LOldToNewNames.add('ALFmxCommon=Alcinoe.FMX.Common');
      LOldToNewNames.add('ALFmxConfetti=Alcinoe.FMX.Confetti');
      LOldToNewNames.add('ALFmxDatePickerDialog=Alcinoe.FMX.DatePickerDialog');
      LOldToNewNames.add('ALFmxDesignEditors=Alcinoe.FMX.DesignEditors');
      LOldToNewNames.add('ALFmxEdit=Alcinoe.FMX.Edit');
      LOldToNewNames.add('ALFmxFacebookCore=Alcinoe.FMX.Facebook.Core');
      LOldToNewNames.add('ALFmxFacebookLogin=Alcinoe.FMX.Facebook.Login');
      LOldToNewNames.add('ALFmxFacebookShare=Alcinoe.FMX.Facebook.Share');
      LOldToNewNames.add('Alcinoe.FMX.FacebookCore=Alcinoe.FMX.Facebook.Core');
      LOldToNewNames.add('Alcinoe.FMX.FacebookLogin=Alcinoe.FMX.Facebook.Login');
      LOldToNewNames.add('Alcinoe.FMX.FacebookShare=Alcinoe.FMX.Facebook.Share');
      LOldToNewNames.add('ALFmxFilterEffects=Alcinoe.FMX.FilterEffects');
      LOldToNewNames.add('ALFmxFirebaseCore=Alcinoe.FMX.Firebase.Core');
      LOldToNewNames.add('ALFmxFirebaseMessaging=Alcinoe.FMX.Firebase.Messaging');
      LOldToNewNames.add('Alcinoe.FMX.FirebaseCore=Alcinoe.FMX.Firebase.Core');
      LOldToNewNames.add('Alcinoe.FMX.FirebaseMessaging=Alcinoe.FMX.Firebase.Messaging');
      LOldToNewNames.add('ALFmxGraphics=Alcinoe.FMX.Graphics');
      LOldToNewNames.add('ALFmxInertialMovement=Alcinoe.FMX.InertialMovement');
      LOldToNewNames.add('ALFmxAndroidNativeView=Alcinoe.FMX.NativeView.Android');
      LOldToNewNames.add('ALFmxIosNativeView=Alcinoe.FMX.NativeView.iOS');
      LOldToNewNames.add('ALFmxIosScrollBox=Alcinoe.FMX.ScrollBox.iOS');
      LOldToNewNames.add('ALFmxLayouts=Alcinoe.FMX.Layouts');
      LOldToNewNames.add('ALFmxMemo=Alcinoe.FMX.Memo');
      LOldToNewNames.add('ALFmxObjects=Alcinoe.FMX.Objects');
      LOldToNewNames.add('ALFmxStdCtrls=Alcinoe.FMX.StdCtrls');
      LOldToNewNames.add('ALFmxTabControl=Alcinoe.FMX.TabControl');
      LOldToNewNames.add('ALFmxTrayicon=Alcinoe.FMX.Trayicon');
      LOldToNewNames.add('ALFmxTypes3D=Alcinoe.FMX.Types3D');
      LOldToNewNames.add('ALFmxVideoPlayer=Alcinoe.FMX.VideoPlayer');
      LOldToNewNames.add('ALFmxVKontakte=Alcinoe.FMX.VKontakte');
      LOldToNewNames.add('ALFmxWebRTC=Alcinoe.FMX.WebRTC');
      LOldToNewNames.add('ALGSMComm=Alcinoe.GSMComm');
      LOldToNewNames.add('ALHTML=Alcinoe.HTML');
      LOldToNewNames.add('ALImageMagick=Alcinoe.ImageMagick');
      LOldToNewNames.add('ALIniFiles=Alcinoe.IniFiles');
      LOldToNewNames.add('ALInternetMessages=Alcinoe.InternetMessages');
      LOldToNewNames.add('ALIsapiHTTP=Alcinoe.IsapiHTTP');
      LOldToNewNames.add('ALJSONDoc=Alcinoe.JSONDoc');
      LOldToNewNames.add('ALLibPhoneNumber=Alcinoe.LibPhoneNumber');
      LOldToNewNames.add('ALMemCachedClient=Alcinoe.MemCached.Client');
      LOldToNewNames.add('ALMime=Alcinoe.Mime');
      LOldToNewNames.add('ALMongoDBClient=Alcinoe.MongoDB.Client');
      LOldToNewNames.add('ALMultiPartParser=Alcinoe.MultiPartParser');
      LOldToNewNames.add('ALMySqlClient=Alcinoe.MySql.Client');
      LOldToNewNames.add('ALMySqlWrapper=Alcinoe.MySql.Wrapper');
      LOldToNewNames.add('ALNNTPClient=Alcinoe.NNTP.Client');
      LOldToNewNames.add('ALPhpRunner=Alcinoe.PhpRunner');
      LOldToNewNames.add('ALPOP3Client=Alcinoe.POP3.Client');
      LOldToNewNames.add('ALQuickSortList=Alcinoe.QuickSortList');
      LOldToNewNames.add('ALRTTI=Alcinoe.RTTI');
      LOldToNewNames.add('ALSMTPClient=Alcinoe.SMTP.Client');
      LOldToNewNames.add('ALSphinxQLClient=Alcinoe.SphinxQL.Client');
      LOldToNewNames.add('ALSqlite3Client=Alcinoe.Sqlite3.Client');
      LOldToNewNames.add('ALSqlite3Wrapper=Alcinoe.Sqlite3.Wrapper');
      LOldToNewNames.add('ALString=Alcinoe.StringUtils');
      LOldToNewNames.add('ALStringList=Alcinoe.StringList');
      LOldToNewNames.add('ALTbbMM=Alcinoe.TbbMM');
      LOldToNewNames.add('ALWebSpider=Alcinoe.WebSpider');
      LOldToNewNames.add('ALWindows=Alcinoe.WinApi.Common');
      LOldToNewNames.add('ALWinSock=Alcinoe.WinSock');
      LOldToNewNames.add('ALXmlDoc=Alcinoe.XMLDoc');
      LOldToNewNames.add('ALZLibEx=Alcinoe.ZLibEx');
      LOldToNewNames.add('ALZLibExGZ=Alcinoe.ZLibExGZ');
      LOldToNewNames.add('ALFTPClient=Alcinoe.FTP.Client');
      LOldToNewNames.add('ALWinInetFTPClient=Alcinoe.FTP.Client.WinINet');
      LOldToNewNames.add('ALHttpClient=Alcinoe.HTTP.Client');
      LOldToNewNames.add('ALWinHttpClient=Alcinoe.HTTP.Client.WinHTTP');
      LOldToNewNames.add('ALWininetHttpClient=Alcinoe.HTTP.Client.WinINet');
      LOldToNewNames.add('ALNetHttpClient=Alcinoe.HTTP.Client.Net');
      LOldToNewNames.add('ALNetHttpClientPool=Alcinoe.HTTP.Client.Net.Pool');
      LOldToNewNames.add('ALWebSocketClient=Alcinoe.WebSocket.Client');
      LOldToNewNames.add('ALWinHTTPWebSocketClient=Alcinoe.WebSocket.Client.WinHTTP');

      //Function names
      LOldToNewNames.add('ALUTF8ExtractHTMLText=ALExtractHTMLText');
      LOldToNewNames.add('ALUTF8ExtractHTMLText=ALExtractHTMLText');
      LOldToNewNames.add('ALUTF8XMLTextElementDecodeV=ALXMLTextElementDecodeV');
      LOldToNewNames.add('ALUTF8XMLTextElementDecode=ALXMLTextElementDecode');
      LOldToNewNames.add('ALUTF8HTMLEncode=ALHTMLEncode');
      LOldToNewNames.add('ALUTF8HTMLDecode=ALHTMLDecode');
      LOldToNewNames.add('ALUTF8JavascriptDecodeV=ALJavascriptDecodeV');
      LOldToNewNames.add('ALUTF8JavascriptDecode=ALJavascriptDecode');
      LOldToNewNames.add('ALUTF8BGNPCGN1947CyrillicToLatin=ALBGNPCGN1947CyrillicToLatin');
      LOldToNewNames.add('ALUTF8Decode=ALStringDecode');
      LOldToNewNames.add('ALUTF8ExpandLigatures=ALExpandLigatures');
      LOldToNewNames.add('ALUTF8ISO91995CyrillicToLatin=ALISO91995CyrillicToLatin');
      LOldToNewNames.add('ALUTF8LowerCase=ALUnicodeLowerCase');
      LOldToNewNames.add('ALUTF8LowerCaseNoDiacritic=ALUnicodeLowerCaseNoDiacritic');
      LOldToNewNames.add('ALUTF8Normalize=ALNormalize');
      LOldToNewNames.add('ALUTF8RemoveDiacritic=ALRemoveDiacritic');
      LOldToNewNames.add('ALUTF8SentenceCase=ALSentenceCase');
      LOldToNewNames.add('ALUTF8TitleCase=ALTitleCase');
      LOldToNewNames.add('ALUTF8UpperCase=ALUnicodeUpperCase');
      LOldToNewNames.add('ALUTF8UpperCaseNoDiacritic=ALUnicodeUpperCaseNoDiacritic');
      LOldToNewNames.add('ALUTF8UpperFirstChar=ALUnicodeUpperFirstChar');
      LOldToNewNames.add('ALBase64DecodeBytesU=ALBase64DecodeBytes');
      LOldToNewNames.add('ALBase64DecodeStringU=ALBase64DecodeString');
      LOldToNewNames.add('ALBase64EncodeBytesU=ALBase64EncodeBytesW');
      LOldToNewNames.add('ALBase64EncodeStringU=ALBase64EncodeString');
      LOldToNewNames.add('ALBinToHex=ALBinToHexA');
      LOldToNewNames.add('ALBinToHexU=ALBinToHexW');
      LOldToNewNames.add('ALBoolToStr=ALBoolToStrA');
      LOldToNewNames.add('ALBoolToStrU=ALBoolToStrW');
      LOldToNewNames.add('ALCompareStr=ALCompareStrA');
      LOldToNewNames.add('ALCompareStrU=ALCompareStrW');
      LOldToNewNames.add('ALCompareText=ALCompareTextA');
      LOldToNewNames.add('ALCompareTextU=ALCompareTextW');
      LOldToNewNames.add('ALCopyStrU=ALCopyStr');
      LOldToNewNames.add('ALCurrToStr=ALCurrToStrA');
      LOldToNewNames.add('ALCurrToStrU=ALCurrToStrW');
      LOldToNewNames.add('ALDateTimeToStr=ALDateTimeToStrA');
      LOldToNewNames.add('ALDateTimeToStrU=ALDateTimeToStrW');
      LOldToNewNames.add('ALDateToStr=ALDateToStrA');
      LOldToNewNames.add('ALDateToStrU=ALDateToStrW');
      LOldToNewNames.add('ALDequotedStrU=ALDequotedStr');
      LOldToNewNames.add('ALExcludeLeadingPathDelimiter=ALExcludeLeadingPathDelimiterA');
      LOldToNewNames.add('ALExcludeLeadingPathDelimiterU=ALExcludeLeadingPathDelimiterW');
      LOldToNewNames.add('ALExcludeTrailingPathDelimiter=ALExcludeTrailingPathDelimiterA');
      LOldToNewNames.add('ALExcludeTrailingPathDelimiterU=ALExcludeTrailingPathDelimiterW');
      LOldToNewNames.add('ALExtractExpression=ALExtractExpressionA');
      LOldToNewNames.add('ALExtractFileDirU=ALExtractFileDir');
      LOldToNewNames.add('ALExtractFileDriveU=ALExtractFileDrive');
      LOldToNewNames.add('ALExtractFileExtU=ALExtractFileExt');
      LOldToNewNames.add('ALExtractFileNameU=ALExtractFileName');
      LOldToNewNames.add('ALExtractFilePathU=ALExtractFilePath');
      LOldToNewNames.add('ALExtractHeaderFieldsWithQuoteEscapedU=ALExtractHeaderFieldsWithQuoteEscaped');
      LOldToNewNames.add('ALExtractQuotedStrU=ALExtractQuotedStr');
      LOldToNewNames.add('ALExtractTagParams=ALExtractTagParamsA');
      LOldToNewNames.add('ALFastTagReplace=ALFastTagReplaceA');
      LOldToNewNames.add('ALFastTagReplacePrecompile=ALFastTagReplacePrecompileA');
      LOldToNewNames.add('ALFloatToStr=ALFloatToStrA');
      LOldToNewNames.add('ALFloatToStrF=ALFloatToStrFA');
      LOldToNewNames.add('ALFloatToStrU=ALFloatToStrW');
      LOldToNewNames.add('ALFormat=ALFormatA');
      LOldToNewNames.add('ALFormatCurr=ALFormatCurrA');
      LOldToNewNames.add('ALFormatCurrU=ALFormatCurrW');
      LOldToNewNames.add('ALFormatDateTime=ALFormatDateTimeA');
      LOldToNewNames.add('ALFormatDateTimeU=ALFormatDateTimeW');
      LOldToNewNames.add('ALFormatFloat=ALFormatFloatA');
      LOldToNewNames.add('ALFormatFloatU=ALFormatFloatW');
      LOldToNewNames.add('ALFormatU=ALFormatW');
      LOldToNewNames.add('ALGetBytesFromFileU=ALGetBytesFromFile');
      LOldToNewNames.add('ALGetStringFromBufferU=ALGetStringFromBuffer');
      LOldToNewNames.add('ALGetStringFromFileU=ALGetStringFromFile');
      LOldToNewNames.add('ALGetStringFromStreamU=ALGetStringFromStream');
      LOldToNewNames.add('ALGUIDToString=ALGUIDToStringA');
      LOldToNewNames.add('ALGUIDToStringU=ALGUIDToStringW');
      LOldToNewNames.add('ALHexToBinU=ALHexToBin');
      LOldToNewNames.add('ALHTTPDecodeU=ALHTTPDecode');
      LOldToNewNames.add('ALHTTPEncodeU=ALHTTPEncode');
      LOldToNewNames.add('ALIncludeLeadingPathDelimiter=ALIncludeLeadingPathDelimiterA');
      LOldToNewNames.add('ALIncludeLeadingPathDelimiterU=ALIncludeLeadingPathDelimiterW');
      LOldToNewNames.add('ALIncludeTrailingPathDelimiter=ALIncludeTrailingPathDelimiterA');
      LOldToNewNames.add('ALIncludeTrailingPathDelimiterU=ALIncludeTrailingPathDelimiterW');
      LOldToNewNames.add('ALIntToBit=ALIntToBitA');
      LOldToNewNames.add('ALIntToHex=ALIntToHexA');
      LOldToNewNames.add('ALIntToStr=ALIntToStrA');
      LOldToNewNames.add('ALIntToStrU=ALIntToStrW');
      LOldToNewNames.add('ALIsDecimalU=ALIsDecimal');
      LOldToNewNames.add('ALIsFloatU=ALIsFloat');
      LOldToNewNames.add('ALIsInt64U=ALIsInt64');
      LOldToNewNames.add('ALIsIntegerU=ALIsInteger');
      LOldToNewNames.add('ALIsPathDelimiterU=ALIsPathDelimiter');
      LOldToNewNames.add('ALIsSmallIntU=ALIsSmallInt');
      LOldToNewNames.add('ALLastDelimiter=ALLastDelimiterA');
      LOldToNewNames.add('ALLastDelimiterU=ALLastDelimiterW');
      LOldToNewNames.add('AlLoCaseU=AlLoCase');
      LOldToNewNames.add('AlLowerCaseU=AlLowerCase');
      LOldToNewNames.add('ALMatchesMask=ALMatchesMaskA');
      LOldToNewNames.add('ALMatchesMaskU=ALMatchesMaskW');
      LOldToNewNames.add('ALMatchStr=ALMatchStrA');
      LOldToNewNames.add('ALMatchText=ALMatchTextA');
      LOldToNewNames.add('ALNewGUIDString=ALNewGUIDStringA');
      LOldToNewNames.add('ALNewGUIDStringU=ALNewGUIDStringW');
      LOldToNewNames.add('ALPos=ALPosA');
      LOldToNewNames.add('ALPosEx=ALPosA');
      LOldToNewNames.add('ALPosExIgnoreCase=ALPosIgnoreCaseA');
      LOldToNewNames.add('ALPosExIgnoreCaseU=ALPosIgnoreCaseW');
      LOldToNewNames.add('ALPosExU=ALPosW');
      LOldToNewNames.add('ALPosU=ALPosW');
      LOldToNewNames.add('ALQuotedStrU=ALQuotedStr');
      LOldToNewNames.add('ALRandomStr=ALRandomStrA');
      LOldToNewNames.add('ALRandomStrU=ALRandomStrW');
      LOldToNewNames.add('ALSameStr=ALSameStrA');
      LOldToNewNames.add('ALSameStrU=ALSameStrW');
      LOldToNewNames.add('ALSameText=ALSameTextA');
      LOldToNewNames.add('ALSameTextU=ALSameTextW');
      LOldToNewNames.add('ALSaveStringtoFileU=ALSaveStringtoFile');
      LOldToNewNames.add('ALSentenceCaseU=ALSentenceCase');
      LOldToNewNames.add('ALSplitTextAndTag=ALSplitTextAndTagA');
      LOldToNewNames.add('ALStringNormalize=ALNormalize');
      LOldToNewNames.add('ALStringReplace=ALStringReplaceA');
      LOldToNewNames.add('ALStringReplaceU=ALStringReplaceW');
      LOldToNewNames.add('ALStrMoveU=ALStrMove');
      LOldToNewNames.add('AlStrToBoolU=AlStrToBool');
      LOldToNewNames.add('ALStrToCurrDefU=ALStrToCurrDef');
      LOldToNewNames.add('ALStrToCurrU=ALStrToCurr');
      LOldToNewNames.add('ALStrToDateTimeU=ALStrToDateTime');
      LOldToNewNames.add('ALStrToDateU=ALStrToDate');
      LOldToNewNames.add('ALStrToFloatDefU=ALStrToFloatDef');
      LOldToNewNames.add('ALStrToFloatU=ALStrToFloat');
      LOldToNewNames.add('ALStrToInt64DefU=ALStrToInt64Def');
      LOldToNewNames.add('ALStrToInt64U=ALStrToInt64');
      LOldToNewNames.add('ALStrToIntDefU=ALStrToIntDef');
      LOldToNewNames.add('ALStrToIntU=ALStrToInt');
      LOldToNewNames.add('ALStrToTimeU=ALStrToTime');
      LOldToNewNames.add('ALStrToUInt64DefU=ALStrToUInt64Def');
      LOldToNewNames.add('ALStrToUInt64U=ALStrToUInt64');
      LOldToNewNames.add('ALTimeToStr=ALTimeToStrA');
      LOldToNewNames.add('ALTimeToStrU=ALTimeToStrW');
      LOldToNewNames.add('ALTitleCaseU=ALTitleCase');
      LOldToNewNames.add('ALTrimLeftU=ALTrimLeft');
      LOldToNewNames.add('ALTrimRightU=ALTrimRight');
      LOldToNewNames.add('ALTrimU=ALTrim');
      LOldToNewNames.add('ALTryBinToHexU=ALTryBinToHex');
      LOldToNewNames.add('ALTryHexToBinU=ALTryHexToBin');
      LOldToNewNames.add('ALTryStrToBoolU=ALTryStrToBool');
      LOldToNewNames.add('ALTryStrToCurrU=ALTryStrToCurr');
      LOldToNewNames.add('ALTryStrToDateTimeU=ALTryStrToDateTime');
      LOldToNewNames.add('ALTryStrToDateU=ALTryStrToDate');
      LOldToNewNames.add('ALTryStrToFloatU=ALTryStrToFloat');
      LOldToNewNames.add('ALTryStrToInt64U=ALTryStrToInt64');
      LOldToNewNames.add('ALTryStrToIntU=ALTryStrToInt');
      LOldToNewNames.add('ALTryStrToTimeU=ALTryStrToTime');
      LOldToNewNames.add('ALTryStrToUInt64U=ALTryStrToUInt64');
      LOldToNewNames.add('ALUIntToStr=ALUIntToStrA');
      LOldToNewNames.add('ALUIntToStrU=ALUIntToStrW');
      LOldToNewNames.add('AlUpCaseU=AlUpCase');
      LOldToNewNames.add('AlUpperCaseU=AlUpperCase');
      LOldToNewNames.add('ALWideExpandLigatures=ALExpandLigatures');
      LOldToNewNames.add('ALWideLowerCaseNoDiacritic=ALUnicodeLowerCaseNoDiacritic');
      LOldToNewNames.add('ALWideNormalize=ALNormalize');
      LOldToNewNames.add('ALWideRemoveDiacritic=ALRemoveDiacritic');
      LOldToNewNames.add('ALWideUpperCaseNoDiacritic=ALUnicodeUpperCaseNoDiacritic');
      LOldToNewNames.add('ALIfThenU=ALIfThenW');
      LOldToNewNames.add('ALJSONToTStrings=ALJSONToTStringsA');
      LOldToNewNames.add('ALTStringsToJson=ALTStringsToJsonA');
      LOldToNewNames.add('ALJSONToXML=ALJSONToXMLA');
      LOldToNewNames.add('ALJsonEncodeFloatWithNodeSubTypeHelper=ALJsonEncodeFloatWithNodeSubTypeHelperA');
      LOldToNewNames.add('ALJsonEncodeTextWithNodeSubTypeHelper=ALJsonEncodeTextWithNodeSubTypeHelperA');
      LOldToNewNames.add('ALJsonEncodeBinaryWithNodeSubTypeHelper=ALJsonEncodeBinaryWithNodeSubTypeHelperA');
      LOldToNewNames.add('ALJsonEncodeObjectIDWithNodeSubTypeHelper=ALJsonEncodeObjectIDWithNodeSubTypeHelperA');
      LOldToNewNames.add('ALJsonEncodeBooleanWithNodeSubTypeHelper=ALJsonEncodeBooleanWithNodeSubTypeHelperA');
      LOldToNewNames.add('ALJsonEncodeDateTimeWithNodeSubTypeHelper=ALJsonEncodeDateTimeWithNodeSubTypeHelperA');
      LOldToNewNames.add('ALJsonEncodeJavascriptWithNodeSubTypeHelper=ALJsonEncodeJavascriptWithNodeSubTypeHelperA');
      LOldToNewNames.add('ALJsonEncodeInt64WithNodeSubTypeHelper=ALJsonEncodeInt64WithNodeSubTypeHelperA');
      LOldToNewNames.add('ALJsonEncodeInt32WithNodeSubTypeHelper=ALJsonEncodeInt32WithNodeSubTypeHelperA');
      LOldToNewNames.add('ALJsonEncodeNullWithNodeSubTypeHelper=ALJsonEncodeNullWithNodeSubTypeHelperA');
      LOldToNewNames.add('ALJsonEncodeWithNodeSubTypeHelper=ALJsonEncodeWithNodeSubTypeHelperA');
      LOldToNewNames.add('ALJSONTryStrToRegEx=ALJSONTryStrToRegExA');
      LOldToNewNames.add('ALJSONTryStrTobinary=ALJSONTryStrToBinaryA');
      LOldToNewNames.add('ALJSONTryStrToDateTime=ALJSONTryStrToDateTimeA');
      LOldToNewNames.add('ALJSONTryStrToObjectID=ALJSONTryStrToObjectIDA');
      LOldToNewNames.add('ALJSONTryStrToTimestamp=ALJSONTryStrToTimestampA');
      LOldToNewNames.add('ALJSONTryStrToInt32=ALJSONTryStrToInt32A');
      LOldToNewNames.add('ALJSONTryStrToInt64=ALJSONTryStrToInt64A');
      LOldToNewNames.add('ALFindJsonNodeByInt32ChildNodeValue=ALFindJsonNodeByInt32ChildNodeValueA');
      LOldToNewNames.add('ALFindJsonNodeByTextChildNodeValue=ALFindJsonNodeByTextChildNodeValueA');
      LOldToNewNames.add('ALJSONToTStringsU=ALJSONToTStringsW');
      LOldToNewNames.add('ALTStringsToJsonU=ALTStringsToJsonW');
      LOldToNewNames.add('ALJsonEncodeFloatWithNodeSubTypeHelperU=ALJsonEncodeFloatWithNodeSubTypeHelperW');
      LOldToNewNames.add('ALJsonEncodeTextWithNodeSubTypeHelperU=ALJsonEncodeTextWithNodeSubTypeHelperW');
      LOldToNewNames.add('ALJsonEncodeBinaryWithNodeSubTypeHelperU=ALJsonEncodeBinaryWithNodeSubTypeHelperW');
      LOldToNewNames.add('ALJsonEncodeObjectIDWithNodeSubTypeHelperU=ALJsonEncodeObjectIDWithNodeSubTypeHelperW');
      LOldToNewNames.add('ALJsonEncodeBooleanWithNodeSubTypeHelperU=ALJsonEncodeBooleanWithNodeSubTypeHelperW');
      LOldToNewNames.add('ALJsonEncodeDateTimeWithNodeSubTypeHelperU=ALJsonEncodeDateTimeWithNodeSubTypeHelperW');
      LOldToNewNames.add('ALJsonEncodeJavascriptWithNodeSubTypeHelperU=ALJsonEncodeJavascriptWithNodeSubTypeHelperW');
      LOldToNewNames.add('ALJsonEncodeInt64WithNodeSubTypeHelperU=ALJsonEncodeInt64WithNodeSubTypeHelperW');
      LOldToNewNames.add('ALJsonEncodeInt32WithNodeSubTypeHelperU=ALJsonEncodeInt32WithNodeSubTypeHelperW');
      LOldToNewNames.add('ALJsonEncodeNullWithNodeSubTypeHelperU=ALJsonEncodeNullWithNodeSubTypeHelperW');
      LOldToNewNames.add('ALJsonEncodeWithNodeSubTypeHelperU=ALJsonEncodeWithNodeSubTypeHelperW');
      LOldToNewNames.add('ALJSONTryStrToRegExU=ALJSONTryStrToRegExW');
      LOldToNewNames.add('ALJSONTryStrTobinaryU=ALJSONTryStrToBinaryW');
      LOldToNewNames.add('ALJSONTryStrToDateTimeU=ALJSONTryStrToDateTimeW');
      LOldToNewNames.add('ALJSONTryStrToObjectIDU=ALJSONTryStrToObjectIDW');
      LOldToNewNames.add('ALJSONTryStrToTimestampU=ALJSONTryStrToTimestampW');
      LOldToNewNames.add('ALJSONTryStrToInt32U=ALJSONTryStrToInt32W');
      LOldToNewNames.add('ALJSONTryStrToInt64U=ALJSONTryStrToInt64W');
      LOldToNewNames.add('ALFindJsonNodeByInt32ChildNodeValueU=ALFindJsonNodeByInt32ChildNodeValueW');
      LOldToNewNames.add('ALFindJsonNodeByTextChildNodeValueU=ALFindJsonNodeByTextChildNodeValueW');
      LOldToNewNames.add('ALAddCallStackCustomLogU=ALAddCallStackCustomLog');
      LOldToNewNames.add('ALGetCallStackCustomLogsU=ALGetCallStackCustomLogs');
      LOldToNewNames.add('AlGetEnvironmentString=AlGetEnvironmentStringA');
      LOldToNewNames.add('AlGetEnvironmentStringU=AlGetEnvironmentStringW');
      LOldToNewNames.add('ALWinExec=ALWinExecA');
      LOldToNewNames.add('ALWinExecU=ALWinExecW');
      LOldToNewNames.add('ALRenameFile=ALRenameFileA');
      LOldToNewNames.add('ALRenameFileU=ALRenameFileW');
      LOldToNewNames.add('ALDeleteFileU=ALDeleteFile');
      LOldToNewNames.add('ALRemoveDirU=ALRemoveDir');
      LOldToNewNames.add('ALCreateDirU=ALCreateDir');
      LOldToNewNames.add('ALDirectoryExistsU=ALDirectoryExists');
      LOldToNewNames.add('ALFileExistsU=ALFileExists');
      LOldToNewNames.add('ALIsDirectoryEmptyU=ALIsDirectoryEmpty');
      LOldToNewNames.add('ALSetFileLastAccessDateTimeU=ALSetFileLastAccessDateTime');
      LOldToNewNames.add('ALSetFileLastWriteDateTimeU=ALSetFileLastWriteDateTime');
      LOldToNewNames.add('ALSetFileCreationDateTimeU=ALSetFileCreationDateTime');
      LOldToNewNames.add('ALGetFileLastAccessDateTimeU=ALGetFileLastAccessDateTime');
      LOldToNewNames.add('ALGetFileLastWriteDateTimeU=ALGetFileLastWriteDateTime');
      LOldToNewNames.add('ALGetFileCreationDateTimeU=ALGetFileCreationDateTime');
      LOldToNewNames.add('ALGetFileSizeU=ALGetFileSize');
      LOldToNewNames.add('ALGetModulePathU=ALGetModulePathW');
      LOldToNewNames.add('ALGetModulePath=ALGetModulePathA');
      LOldToNewNames.add('ALGetModuleNameU=ALGetModuleNameW');
      LOldToNewNames.add('ALGetModuleName=ALGetModuleNameA');
      LOldToNewNames.add('ALGetModuleFileNameWithoutExtension=ALGetModuleFileNameA');
      LOldToNewNames.add('AlCopyDirectory=AlCopyDirectoryA');
      LOldToNewNames.add('AlCopyDirectoryU=AlCopyDirectoryW');
      LOldToNewNames.add('AlEmptyDirectory=AlEmptyDirectoryA');
      LOldToNewNames.add('AlEmptyDirectoryU=AlEmptyDirectoryW');
      LOldToNewNames.add('ALRDLEncryptString=ALRDLEncryptStringA');
      LOldToNewNames.add('ALRDLEncryptStringU=ALRDLEncryptStringW');
      LOldToNewNames.add('ALRDLEncryptStringCBC=ALRDLEncryptStringCBCA');
      LOldToNewNames.add('ALRDLEncryptStringCBCU=ALRDLEncryptStringCBCW');
      LOldToNewNames.add('ALRDLEncryptStreamU=ALRDLEncryptStream');
      LOldToNewNames.add('ALRDLEncryptStreamCBCU=ALRDLEncryptStreamCBC');
      LOldToNewNames.add('ALStringHashMD5U=ALStringHashMD5');
      LOldToNewNames.add('ALStringHashSHA1U=ALStringHashSHA1');
      LOldToNewNames.add('ALStringHashSHA2U=ALStringHashSHA2');
      LOldToNewNames.add('ALFnv1aInt32U=ALFnv1aInt32');
      LOldToNewNames.add('ALFnv1aInt64U=ALFnv1aInt64');
      LOldToNewNames.add('ALJavascriptEncodeU=ALJavascriptEncode');
      LOldToNewNames.add('ALJavascriptDecodeVU=ALJavascriptDecodeV');
      LOldToNewNames.add('ALJavascriptDecodeU=ALJavascriptDecode');
      LOldToNewNames.add('ALDecompressHttpResponseContentU=ALDecompressHttpResponseContent');
      LOldToNewNames.add('AlIsValidEmailU=AlIsValidEmail');
      LOldToNewNames.add('AlDetectImageExtensionU=AlDetectImageExtension');

      //type names
      LOldToNewNames.add('pALFormatSettings=pALFormatSettingsA');
      LOldToNewNames.add('TALFormatSettings=TALFormatSettingsA');
      LOldToNewNames.add('pALFormatSettingsU=pALFormatSettingsW');
      LOldToNewNames.add('TALFormatSettingsU=TALFormatSettingsW');
      LOldToNewNames.add('TALStringStream=TALStringStreamA');
      LOldToNewNames.add('TALMask=TALMaskA');
      LOldToNewNames.add('TALTagParamsClass=TALTagParamsClassA');
      LOldToNewNames.add('TALBasePrecompiledTag=TALBasePrecompiledTagA');
      LOldToNewNames.add('TALPrecompiledTag=TALPrecompiledTagA');
      LOldToNewNames.add('TALHandleTagfunct=TALHandleTagfunctA');
      LOldToNewNames.add('TALHandleTagExtendedfunct=TALHandleTagExtendedfunctA');
      LOldToNewNames.add('TALHandleTagPrecompileFunct=TALHandleTagPrecompileFunctA');
      LOldToNewNames.add('EALExceptionU=EALException');
      LOldToNewNames.add('EALCipherExceptionU=EALCipherException');
      LOldToNewNames.add('TALStrings=TALStringsA');
      LOldToNewNames.add('TALStringsEnumerator=TALStringsEnumeratorA');
      LOldToNewNames.add('TALStringList=TALStringListA');
      LOldToNewNames.add('PALStringItem=PALStringItemA');
      LOldToNewNames.add('TALStringItem=TALStringItemA');
      LOldToNewNames.add('TALStringItemList=TALStringItemListA');
      LOldToNewNames.add('TALStringListSortCompare=TALStringListSortCompareA');
      LOldToNewNames.add('TALNVStringList=TALNVStringListA');
      LOldToNewNames.add('PALNVStringItem=PALNVStringItemA');
      LOldToNewNames.add('TALNVStringItem=TALNVStringItemA');
      LOldToNewNames.add('TALNVStringItemList=TALNVStringItemListA');
      LOldToNewNames.add('TALNVStringListSortCompare=TALNVStringListSortCompareA');
      LOldToNewNames.add('TALAVLStringList=TALAVLStringListA');
      LOldToNewNames.add('TALAVLStringListSortCompare=TALAVLStringListSortCompareA');
      LOldToNewNames.add('TALAVLStringListBinaryTreeNode=TALAVLStringListBinaryTreeNodeA');
      LOldToNewNames.add('TALHashedStringList=TALHashedStringListA');
      LOldToNewNames.add('TALHashedStringListSortCompare=TALHashedStringListSortCompareA');
      LOldToNewNames.add('TALHashedStringListDictionaryNode=TALHashedStringListDictionaryNodeA');
      LOldToNewNames.add('TALStringsU=TALStringsW');
      LOldToNewNames.add('TALStringsEnumeratorU=TALStringsEnumeratorW');
      LOldToNewNames.add('TALStringListU=TALStringListW');
      LOldToNewNames.add('PALStringItemU=PALStringItemW');
      LOldToNewNames.add('TALStringItemU=TALStringItemW');
      LOldToNewNames.add('TALStringItemListU=TALStringItemListW');
      LOldToNewNames.add('TALStringListSortCompareU=TALStringListSortCompareW');
      LOldToNewNames.add('TALNVStringListU=TALNVStringListW');
      LOldToNewNames.add('PALNVStringItemU=PALNVStringItemW');
      LOldToNewNames.add('TALNVStringItemU=TALNVStringItemW');
      LOldToNewNames.add('TALNVStringItemListU=TALNVStringItemListW');
      LOldToNewNames.add('TALNVStringListSortCompareU=TALNVStringListSortCompareW');
      LOldToNewNames.add('TAlJSONParseDocument=TAlJSONParseDocumentA');
      LOldToNewNames.add('TAlJSONParseTextEvent=TAlJSONParseTextEventA');
      LOldToNewNames.add('TAlJSONParseObjectEvent=TAlJSONParseObjectEventA');
      LOldToNewNames.add('TAlJSONParseArrayEvent=TAlJSONParseArrayEventA');
      LOldToNewNames.add('TALJSONNodeListSortCompare=TALJSONNodeListSortCompareA');
      LOldToNewNames.add('TALJSONPointerList=TALJSONPointerListA');
      LOldToNewNames.add('TALJSONNodeList=TALJSONNodeListA');
      LOldToNewNames.add('TALJSONNode=TALJSONNodeA');
      LOldToNewNames.add('TALJSONObjectNode=TALJSONObjectNodeA');
      LOldToNewNames.add('TALJSONArrayNode=TALJSONArrayNodeA');
      LOldToNewNames.add('TALJSONTextNode=TALJSONTextNodeA');
      LOldToNewNames.add('TALJSONDocument=TALJSONDocumentA');
      LOldToNewNames.add('TAlJSONParseDocumentU=TAlJSONParseDocumentW');
      LOldToNewNames.add('TAlJSONParseTextEventU=TAlJSONParseTextEventW');
      LOldToNewNames.add('TAlJSONParseObjectEventU=TAlJSONParseObjectEventW');
      LOldToNewNames.add('TAlJSONParseArrayEventU=TAlJSONParseArrayEventW');
      LOldToNewNames.add('TALJSONNodeListSortCompareU=TALJSONNodeListSortCompareW');
      LOldToNewNames.add('TALJSONPointerListU=TALJSONPointerListW');
      LOldToNewNames.add('TALJSONNodeListU=TALJSONNodeListW');
      LOldToNewNames.add('TALJSONNodeU=TALJSONNodeW');
      LOldToNewNames.add('TALJSONObjectNodeU=TALJSONObjectNodeW');
      LOldToNewNames.add('TALJSONArrayNodeU=TALJSONArrayNodeW');
      LOldToNewNames.add('TALJSONTextNodeU=TALJSONTextNodeW');
      LOldToNewNames.add('TALJSONDocumentU=TALJSONDocumentW');

      //var names
      LOldToNewNames.add('ALDefaultFormatSettings=ALDefaultFormatSettingsA');
      LOldToNewNames.add('ALDefaultFormatSettingsU=ALDefaultFormatSettingsW');
      LOldToNewNames.add('vALJsonISODateFormatSettings=ALJsonISODateFormatSettingsA');
      LOldToNewNames.add('vALJsonISODateFormatSettingsU=ALJsonISODateFormatSettingsW');
      LOldToNewNames.add('vALDefaultNodeIndentU=ALDefaultJsonNodeIndentW');
      LOldToNewNames.add('ALGlobalNetHttpClientPool=TALNetHttpClientPool.Instance');
      LOldToNewNames.add('ALGlobalGraphicThreadPool=TALGraphicThreadPool.Instance');

      var LFiles := TALStringListW.Create;
      try

        var LPasFiles := TDirectory.GetFiles(string(LRootDirectory), '*.pas', TSearchOption.soAllDirectories);
        for var I := Low(LPasFiles) to High(LPasFiles) do LFiles.Add(LPasFiles[i]);
        var LDprFiles := TDirectory.GetFiles(string(LRootDirectory), '*.dpr', TSearchOption.soAllDirectories);
        for var I := Low(LDprFiles) to High(LDprFiles) do LFiles.Add(LDprFiles[i]);
        var LDprojFiles := TDirectory.GetFiles(string(LRootDirectory), '*.dproj', TSearchOption.soAllDirectories);
        for var I := Low(LDprojFiles) to High(LDprojFiles) do LFiles.Add(LDprojFiles[i]);
        var LDpkFiles := TDirectory.GetFiles(string(LRootDirectory), '*.dpk', TSearchOption.soAllDirectories);
        for var I := Low(LDpkFiles) to High(LDpkFiles) do LFiles.Add(LDpkFiles[i]);
        var LpatchFiles := TDirectory.GetFiles(string(LRootDirectory), '*.patch', TSearchOption.soAllDirectories);
        for var I := Low(LpatchFiles) to High(LpatchFiles) do LFiles.Add(LpatchFiles[i]);
        var LMDFiles := TDirectory.GetFiles(string(LRootDirectory), '*.md', TSearchOption.soAllDirectories);
        for var I := Low(LMDFiles) to High(LMDFiles) do LFiles.Add(LMDFiles[i]);

        for var I := 0 to LFiles.Count - 1 do begin
          if AlSameTextW(ALExtractFileName(LFiles[i]), 'CodeRenaming.dpr') then continue;
          var LSourceStr := ALGetStringFromFile(LFiles[i]);
          var LOriginalSourceStr := LSourceStr;
          for var J := 0 to LOldToNewNames.Count - 1 do begin
            var LOldName := LOldToNewNames.Names[j];
            var LNewName := LOldToNewNames.ValueFromIndex[j];
            if (LOldName = '') or (LNewName='') then
              raise Exception.Create('Error 84F2AFB4-E0DC-4265-BD52-F8F8C77C80F4');
            //----
            var P1 := ALPosIgnoreCaseA(LOldName, LSourceStr);
            while P1 > 0 do begin
              var P2 := P1 + length(LOldName);
              if ((P1 = 1) or ((not Char(LSourceStr[P1-1]).IsLetterOrDigit) and (LSourceStr[P1-1] <> '_'))) and
                 ((P2 > length(LSourceStr)) or ((not Char(LSourceStr[P2]).IsLetterOrDigit)  and (LSourceStr[P2] <> '_'))) then begin
                delete(LSourceStr, P1, length(LOldName));
                insert(LNewName,LSourceStr, P1);
                P1 := P1 + length(LNewName) - 1;
              end;
              P1 := ALPosIgnoreCaseA(LOldName, LSourceStr, P1+1);
            end;
          end;
          if LOriginalSourceStr <> LSourceStr then begin
            ALSaveStringToFile(LSourceStr,LFiles[i]);
            Writeln('Updated '+ LFiles[i]);
          end;
        end;

      finally
        ALFreeAndNil(LFiles);
      end;

    finally
      ALFreeAndNil(LOldToNewNames);
    end;

    Writeln('');
    Writeln('');
    Writeln('Finished');
    Writeln('Note: The functions below are overloaded, which means there are');
    Writeln('multiple versions of them, and they cannot be renamed');
    Writeln('automatically. You will need to manually rename them in your code.');
    Writeln('ALIfThen=ALIfThenA');
    Writeln('Alcinoe.JSONDoc.vALDefaultNodeIndent=ALDefaultJsonNodeIndentA');
    Writeln('Alcinoe.XMLDoc.vALDefaultNodeIndent=ALDefaultXMLNodeIndent');

    Writeln('');
    Writeln('Press <Enter> key to quit');
    Readln;

  except
    on E: Exception do begin
      Writeln(E.Message);
      ExitCode := 1;
    end;
  end;

end.
