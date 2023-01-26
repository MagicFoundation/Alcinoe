program UnitRenaming;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.Classes,
  System.SysUtils,
  System.ioutils,
  System.types,
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

    Writeln('This tool will rename the old Alcinoe unit name (e.g. ALHTTPClient)');
    Writeln('to the new Alcinoe unit name (e.g. Alcinoe.Http.Client) in your source code.');
    Writeln('');
    Writeln('Enter the root directory (Please make backup before):');
    var LRootDirectory: String;
    readln(LRootDirectory);
    if (LRootDirectory = '') or (not TDirectory.Exists(LRootDirectory)) then
      raise Exception.Create('Root directory does not exist');
    Writeln('');

    var LUnitsToRename := TALStringList.Create;
    try

      LUnitsToRename.add('ALAndroidApi=Alcinoe.AndroidApi.Common');
      LUnitsToRename.add('ALAndroidBillingClientApi=Alcinoe.AndroidApi.BillingClient');
      LUnitsToRename.add('ALAndroidExoPlayerApi=Alcinoe.AndroidApi.ExoPlayer');
      LUnitsToRename.add('ALAndroidFacebookApi=Alcinoe.AndroidApi.Facebook');
      LUnitsToRename.add('ALAndroidFirebaseApi=Alcinoe.AndroidApi.Firebase');
      LUnitsToRename.add('ALAndroidGoogleApi=Alcinoe.AndroidApi.Google');
      LUnitsToRename.add('ALAndroidInstallReferrerApi=Alcinoe.AndroidApi.InstallReferrer');
      LUnitsToRename.add('ALAndroidVKontakteApi=Alcinoe.AndroidApi.VKontakte');
      LUnitsToRename.add('ALAndroidWebRTCApi=Alcinoe.AndroidApi.WebRTC');
      LUnitsToRename.add('ALAndroidXApi=Alcinoe.AndroidApi.AndroidX');
      LUnitsToRename.add('ALIosAdSupportApi=Alcinoe.iOSApi.AdSupport');
      LUnitsToRename.add('ALIosAudioToolboxApi=Alcinoe.iOSApi.AudioToolbox');
      LUnitsToRename.add('ALIosAuthenticationServicesApi=Alcinoe.iOSApi.AuthenticationServices');
      LUnitsToRename.add('ALIosBackgroundTasksApi=Alcinoe.iOSApi.BackgroundTasks');
      LUnitsToRename.add('ALIosFacebookCoreKitApi=Alcinoe.iOSApi.FacebookCoreKit');
      LUnitsToRename.add('ALIosFacebookLoginKitApi=Alcinoe.iOSApi.FacebookLoginKit');
      LUnitsToRename.add('ALIosFacebookShareKitApi=Alcinoe.iOSApi.FacebookShareKit');
      LUnitsToRename.add('ALIosFirebaseCoreApi=Alcinoe.iOSApi.FirebaseCore');
      LUnitsToRename.add('ALIosFirebaseMessagingApi=Alcinoe.iOSApi.FirebaseMessaging');
      LUnitsToRename.add('ALIosImageIOApi=Alcinoe.iOSApi.ImageIO');
      LUnitsToRename.add('ALIosMessageUIApi=Alcinoe.iOSApi.MessageUI');
      LUnitsToRename.add('ALIosPhotosApi=Alcinoe.iOSApi.Photos');
      LUnitsToRename.add('ALIosVKontakteApi=Alcinoe.iOSApi.VKontakte');
      LUnitsToRename.add('ALIosWebRTCApi=Alcinoe.iOSApi.WebRTC');
      LUnitsToRename.add('ALAVLBinaryTree=Alcinoe.AVLBinaryTree');
      LUnitsToRename.add('ALCGI=Alcinoe.CGI');
      LUnitsToRename.add('ALCipher=Alcinoe.Cipher');
      LUnitsToRename.add('ALCommon=Alcinoe.Common');
      LUnitsToRename.add('ALExecute=Alcinoe.Execute');
      LUnitsToRename.add('ALExprEval=Alcinoe.ExprEval');
      LUnitsToRename.add('ALFBXBase=Alcinoe.FBX.Base');
      LUnitsToRename.add('ALFBXClient=Alcinoe.FBX.Client');
      LUnitsToRename.add('ALFBXConst=Alcinoe.FBX.Consts');
      LUnitsToRename.add('ALFBXError=Alcinoe.FBX.Error');
      LUnitsToRename.add('ALFBXLib=Alcinoe.FBX.Lib');
      LUnitsToRename.add('ALFiles=Alcinoe.Files');
      LUnitsToRename.add('ALFmxAni=Alcinoe.FMX.Ani');
      LUnitsToRename.add('ALFmxBreakText=Alcinoe.FMX.BreakText');
      LUnitsToRename.add('ALFmxCommon=Alcinoe.FMX.Common');
      LUnitsToRename.add('ALFmxConfetti=Alcinoe.FMX.Confetti');
      LUnitsToRename.add('ALFmxDatePickerDialog=Alcinoe.FMX.DatePickerDialog');
      LUnitsToRename.add('ALFmxDesignEditors=Alcinoe.FMX.DesignEditors');
      LUnitsToRename.add('ALFmxEdit=Alcinoe.FMX.Edit');
      LUnitsToRename.add('ALFmxFacebookCore=Alcinoe.FMX.FacebookCore');
      LUnitsToRename.add('ALFmxFacebookLogin=Alcinoe.FMX.FacebookLogin');
      LUnitsToRename.add('ALFmxFacebookShare=Alcinoe.FMX.FacebookShare');
      LUnitsToRename.add('ALFmxFilterEffects=Alcinoe.FMX.FilterEffects');
      LUnitsToRename.add('ALFmxFirebaseCore=Alcinoe.FMX.FirebaseCore');
      LUnitsToRename.add('ALFmxFirebaseMessaging=Alcinoe.FMX.FirebaseMessaging');
      LUnitsToRename.add('ALFmxGraphics=Alcinoe.FMX.Graphics');
      LUnitsToRename.add('ALFmxInertialMovement=Alcinoe.FMX.InertialMovement');
      LUnitsToRename.add('ALFmxAndroidNativeView=Alcinoe.FMX.NativeView.Android');
      LUnitsToRename.add('ALFmxIosNativeView=Alcinoe.FMX.NativeView.iOS');
      LUnitsToRename.add('ALFmxIosScrollBox=Alcinoe.FMX.ScrollBox.iOS');
      LUnitsToRename.add('ALFmxLayouts=Alcinoe.FMX.Layouts');
      LUnitsToRename.add('ALFmxMemo=Alcinoe.FMX.Memo');
      LUnitsToRename.add('ALFmxObjects=Alcinoe.FMX.Objects');
      LUnitsToRename.add('ALFmxStdCtrls=Alcinoe.FMX.StdCtrls');
      LUnitsToRename.add('ALFmxTabControl=Alcinoe.FMX.TabControl');
      LUnitsToRename.add('ALFmxTrayicon=Alcinoe.FMX.Trayicon');
      LUnitsToRename.add('ALFmxTypes3D=Alcinoe.FMX.Types3D');
      LUnitsToRename.add('ALFmxVideoPlayer=Alcinoe.FMX.VideoPlayer');
      LUnitsToRename.add('ALFmxVKontakte=Alcinoe.FMX.VKontakte');
      LUnitsToRename.add('ALFmxWebRTC=Alcinoe.FMX.WebRTC');
      LUnitsToRename.add('ALGSMComm=Alcinoe.GSMComm');
      LUnitsToRename.add('ALHTML=Alcinoe.HTML');
      LUnitsToRename.add('ALImageMagick=Alcinoe.ImageMagick');
      LUnitsToRename.add('ALIniFiles=Alcinoe.IniFiles');
      LUnitsToRename.add('ALInternetMessages=Alcinoe.InternetMessages');
      LUnitsToRename.add('ALIsapiHTTP=Alcinoe.IsapiHTTP');
      LUnitsToRename.add('ALJSONDoc=Alcinoe.JSONDoc');
      LUnitsToRename.add('ALLibPhoneNumber=Alcinoe.LibPhoneNumber');
      LUnitsToRename.add('ALMemCachedClient=Alcinoe.MemCached.Client');
      LUnitsToRename.add('ALMime=Alcinoe.Mime');
      LUnitsToRename.add('ALMongoDBClient=Alcinoe.MongoDB.Client');
      LUnitsToRename.add('ALMultiPartParser=Alcinoe.MultiPartParser');
      LUnitsToRename.add('ALMySqlClient=Alcinoe.MySql.Client');
      LUnitsToRename.add('ALMySqlWrapper=Alcinoe.MySql.Wrapper');
      LUnitsToRename.add('ALNNTPClient=Alcinoe.NNTP.Client');
      LUnitsToRename.add('ALPhpRunner=Alcinoe.PhpRunner');
      LUnitsToRename.add('ALPOP3Client=Alcinoe.POP3.Client');
      LUnitsToRename.add('ALQuickSortList=Alcinoe.QuickSortList');
      LUnitsToRename.add('ALRTTI=Alcinoe.RTTI');
      LUnitsToRename.add('ALSMTPClient=Alcinoe.SMTP.Client');
      LUnitsToRename.add('ALSphinxQLClient=Alcinoe.SphinxQL.Client');
      LUnitsToRename.add('ALSqlite3Client=Alcinoe.Sqlite3.Client');
      LUnitsToRename.add('ALSqlite3Wrapper=Alcinoe.Sqlite3.Wrapper');
      LUnitsToRename.add('ALString=Alcinoe.StringUtils');
      LUnitsToRename.add('ALStringList=Alcinoe.StringList');
      LUnitsToRename.add('ALTbbMM=Alcinoe.TbbMM');
      LUnitsToRename.add('ALWebSpider=Alcinoe.WebSpider');
      LUnitsToRename.add('ALWindows=Alcinoe.WinApi.Common');
      LUnitsToRename.add('ALWinSock=Alcinoe.WinSock');
      LUnitsToRename.add('ALXmlDoc=Alcinoe.XMLDoc');
      LUnitsToRename.add('ALZLibEx=Alcinoe.ZLibEx');
      LUnitsToRename.add('ALZLibExGZ=Alcinoe.ZLibExGZ');
      LUnitsToRename.add('ALFTPClient=Alcinoe.FTP.Client');
      LUnitsToRename.add('ALWinInetFTPClient=Alcinoe.FTP.Client.WinINet');
      LUnitsToRename.add('ALHttpClient=Alcinoe.HTTP.Client');
      LUnitsToRename.add('ALWinHttpClient=Alcinoe.HTTP.Client.WinHTTP');
      LUnitsToRename.add('ALWininetHttpClient=Alcinoe.HTTP.Client.WinINet');
      LUnitsToRename.add('ALNetHttpClient=Alcinoe.HTTP.Client.Net');
      LUnitsToRename.add('ALNetHttpClientPool=Alcinoe.HTTP.Client.Net.Pool');
      LUnitsToRename.add('ALWebSocketClient=Alcinoe.WebSocket.Client');
      LUnitsToRename.add('ALWinHTTPWebSocketClient=Alcinoe.WebSocket.Client.WinHTTP');

      var LFiles := TALStringListU.Create;
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
          if AlSameTextU(ALExtractFileNameU(LFiles[i]), 'UnitRenaming.dpr') then continue;
          var LSourceStr := ALGetStringFromFile(LFiles[i]);
          var LOriginalSourceStr := LSourceStr;
          for var J := 0 to LUnitsToRename.Count - 1 do begin
            var LOldUnitName := LUnitsToRename.Names[j];
            var LNewUnitName := LUnitsToRename.ValueFromIndex[j];
            if (LOldUnitName = '') or (LNewUnitName='') then
              raise Exception.Create('Error 84F2AFB4-E0DC-4265-BD52-F8F8C77C80F4');
            LSourceStr := ALStringReplace(LSourceStr,','+LOldUnitName+','    ,     ','+LNewUnitName+',',[rfReplaceALL, RFIgnoreCase]);
            LSourceStr := ALStringReplace(LSourceStr,','+LOldUnitName+';'    ,     ','+LNewUnitName+';',[rfReplaceALL, RFIgnoreCase]);
            LSourceStr := ALStringReplace(LSourceStr,','+LOldUnitName+' '    ,     ','+LNewUnitName+' ',[rfReplaceALL, RFIgnoreCase]);
            LSourceStr := ALStringReplace(LSourceStr,','+LOldUnitName+'.'    ,     ','+LNewUnitName+'.',[rfReplaceALL, RFIgnoreCase]);
            LSourceStr := ALStringReplace(LSourceStr,','+LOldUnitName+')'    ,     ','+LNewUnitName+')',[rfReplaceALL, RFIgnoreCase]);
            LSourceStr := ALStringReplace(LSourceStr,','+LOldUnitName+']'    ,     ','+LNewUnitName+']',[rfReplaceALL, RFIgnoreCase]);

            LSourceStr := ALStringReplace(LSourceStr,'('+LOldUnitName+','    ,     '('+LNewUnitName+',',[rfReplaceALL, RFIgnoreCase]);
            LSourceStr := ALStringReplace(LSourceStr,'('+LOldUnitName+';'    ,     '('+LNewUnitName+';',[rfReplaceALL, RFIgnoreCase]);
            LSourceStr := ALStringReplace(LSourceStr,'('+LOldUnitName+' '    ,     '('+LNewUnitName+' ',[rfReplaceALL, RFIgnoreCase]);
            LSourceStr := ALStringReplace(LSourceStr,'('+LOldUnitName+'.'    ,     '('+LNewUnitName+'.',[rfReplaceALL, RFIgnoreCase]);
            LSourceStr := ALStringReplace(LSourceStr,'('+LOldUnitName+')'    ,     '('+LNewUnitName+')',[rfReplaceALL, RFIgnoreCase]);
            LSourceStr := ALStringReplace(LSourceStr,'('+LOldUnitName+']'    ,     '('+LNewUnitName+']',[rfReplaceALL, RFIgnoreCase]);

            LSourceStr := ALStringReplace(LSourceStr,'['+LOldUnitName+','    ,     '['+LNewUnitName+',',[rfReplaceALL, RFIgnoreCase]);
            LSourceStr := ALStringReplace(LSourceStr,'['+LOldUnitName+';'    ,     '['+LNewUnitName+';',[rfReplaceALL, RFIgnoreCase]);
            LSourceStr := ALStringReplace(LSourceStr,'['+LOldUnitName+' '    ,     '['+LNewUnitName+' ',[rfReplaceALL, RFIgnoreCase]);
            LSourceStr := ALStringReplace(LSourceStr,'['+LOldUnitName+'.'    ,     '['+LNewUnitName+'.',[rfReplaceALL, RFIgnoreCase]);
            LSourceStr := ALStringReplace(LSourceStr,'['+LOldUnitName+')'    ,     '['+LNewUnitName+')',[rfReplaceALL, RFIgnoreCase]);
            LSourceStr := ALStringReplace(LSourceStr,'['+LOldUnitName+']'    ,     '['+LNewUnitName+']',[rfReplaceALL, RFIgnoreCase]);

            LSourceStr := ALStringReplace(LSourceStr,'/'+LOldUnitName+'.'    ,     '/'+LNewUnitName+'.',[rfReplaceALL, RFIgnoreCase]);
            LSourceStr := ALStringReplace(LSourceStr,'\'+LOldUnitName+'.'    ,     '\'+LNewUnitName+'.',[rfReplaceALL, RFIgnoreCase]);

            LSourceStr := ALStringReplace(LSourceStr,' '+LOldUnitName+','    ,     ' '+LNewUnitName+',',[rfReplaceALL, RFIgnoreCase]);
            LSourceStr := ALStringReplace(LSourceStr,' '+LOldUnitName+';'    ,     ' '+LNewUnitName+';',[rfReplaceALL, RFIgnoreCase]);
            LSourceStr := ALStringReplace(LSourceStr,' '+LOldUnitName+' '    ,     ' '+LNewUnitName+' ',[rfReplaceALL, RFIgnoreCase]);
            LSourceStr := ALStringReplace(LSourceStr,' '+LOldUnitName+'.'    ,     ' '+LNewUnitName+'.',[rfReplaceALL, RFIgnoreCase]);
            LSourceStr := ALStringReplace(LSourceStr,' '+LOldUnitName+')'    ,     ' '+LNewUnitName+')',[rfReplaceALL, RFIgnoreCase]);
            LSourceStr := ALStringReplace(LSourceStr,' '+LOldUnitName+']'    ,     ' '+LNewUnitName+']',[rfReplaceALL, RFIgnoreCase]);

            LSourceStr := ALStringReplace(LSourceStr,#9+LOldUnitName+','     ,     #9+LNewUnitName+',',[rfReplaceALL, RFIgnoreCase]);
            LSourceStr := ALStringReplace(LSourceStr,#9+LOldUnitName+';'     ,     #9+LNewUnitName+';',[rfReplaceALL, RFIgnoreCase]);
            LSourceStr := ALStringReplace(LSourceStr,#9+LOldUnitName+' '     ,     #9+LNewUnitName+' ',[rfReplaceALL, RFIgnoreCase]);
            LSourceStr := ALStringReplace(LSourceStr,#9+LOldUnitName+'.'     ,     #9+LNewUnitName+'.',[rfReplaceALL, RFIgnoreCase]);
            LSourceStr := ALStringReplace(LSourceStr,#9+LOldUnitName+')'     ,     #9+LNewUnitName+')',[rfReplaceALL, RFIgnoreCase]);
            LSourceStr := ALStringReplace(LSourceStr,#9+LOldUnitName+']'     ,     #9+LNewUnitName+']',[rfReplaceALL, RFIgnoreCase]);

            LSourceStr := ALStringReplace(LSourceStr,#13+LOldUnitName+','    ,     #13+LNewUnitName+',',[rfReplaceALL, RFIgnoreCase]);
            LSourceStr := ALStringReplace(LSourceStr,#13+LOldUnitName+';'    ,     #13+LNewUnitName+';',[rfReplaceALL, RFIgnoreCase]);
            LSourceStr := ALStringReplace(LSourceStr,#13+LOldUnitName+' '    ,     #13+LNewUnitName+' ',[rfReplaceALL, RFIgnoreCase]);
            LSourceStr := ALStringReplace(LSourceStr,#13+LOldUnitName+'.'    ,     #13+LNewUnitName+'.',[rfReplaceALL, RFIgnoreCase]);
            LSourceStr := ALStringReplace(LSourceStr,#13+LOldUnitName+')'    ,     #13+LNewUnitName+')',[rfReplaceALL, RFIgnoreCase]);
            LSourceStr := ALStringReplace(LSourceStr,#13+LOldUnitName+']'    ,     #13+LNewUnitName+']',[rfReplaceALL, RFIgnoreCase]);

            LSourceStr := ALStringReplace(LSourceStr,#10+LOldUnitName+','    ,     #10+LNewUnitName+',',[rfReplaceALL, RFIgnoreCase]);
            LSourceStr := ALStringReplace(LSourceStr,#10+LOldUnitName+';'    ,     #10+LNewUnitName+';',[rfReplaceALL, RFIgnoreCase]);
            LSourceStr := ALStringReplace(LSourceStr,#10+LOldUnitName+' '    ,     #10+LNewUnitName+' ',[rfReplaceALL, RFIgnoreCase]);
            LSourceStr := ALStringReplace(LSourceStr,#10+LOldUnitName+'.'    ,     #10+LNewUnitName+'.',[rfReplaceALL, RFIgnoreCase]);
            LSourceStr := ALStringReplace(LSourceStr,#10+LOldUnitName+')'    ,     #10+LNewUnitName+')',[rfReplaceALL, RFIgnoreCase]);
            LSourceStr := ALStringReplace(LSourceStr,#10+LOldUnitName+']'    ,     #10+LNewUnitName+']',[rfReplaceALL, RFIgnoreCase]);

            LSourceStr := ALStringReplace(LSourceStr,''''+LOldUnitName+'.'   ,     ''''+LNewUnitName+'.',[rfReplaceALL, RFIgnoreCase]);
            LSourceStr := ALStringReplace(LSourceStr,'"'+LOldUnitName+'.'    ,     '"'+LNewUnitName+'.',[rfReplaceALL, RFIgnoreCase]);
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
      ALFreeAndNil(LUnitsToRename);
    end;

    Writeln('');
    Writeln('Finished');
    Writeln('Press any key to exit');
    Readln;

  except
    on E: Exception do begin
      Writeln(E.Message);
      ExitCode := 1;
    end;
  end;

end.
