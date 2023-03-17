unit main;

//
// This demo is very drafty, normally we must not use http server for this and
// must use socket client (or similar like notification), but i was lazzy when
// doing this demo. do not forget to launch the ALLiveVideoChatServer
//

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, Alcinoe.FMX.WebRTC,
  FMX.Controls.Presentation, FMX.StdCtrls, Alcinoe.Common, Alcinoe.FMX.Types3D, FMX.Objects,
  FMX.Canvas.GPU, Alcinoe.FMX.Graphics, FMX.Types3D, FMX.Effects, FMX.Filter.Effects,
  Alcinoe.FMX.StdCtrls, Alcinoe.FMX.Layouts, FMX.Layouts, system.messaging,
  IdHTTP, Alcinoe.FMX.FilterEffects, FMX.Edit, Alcinoe.FMX.Objects;

type
  TForm1 = class(TForm)
    LocalCameraLayout: TALLayout;
    ButtonCall: TButton;
    RemoteCameraLayout: TALLayout;
    ButtonHangUp: TButton;
    ServerUrlEdit: TEdit;
    TextIntro: TALText;
    procedure FormCreate(Sender: TObject);
    procedure LocalCameraLayoutPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
    procedure ButtonCallClick(Sender: TObject);
    procedure RemoteCameraLayoutPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
    procedure FormResize(Sender: TObject);
    procedure ButtonHangUpClick(Sender: TObject);
  private
    fMustcheckoffer: boolean;
    fMustcheckanswer: boolean;
    fUserID: int64;
    fWebRTC: TalWebRTC;
    {$IF Defined(IOS) or Defined(ANDROID)}
    procedure ApplicationExceptionHandler(const Sender: TObject; const M: TMessage);
    {$ENDIF}
    {$IF Defined(MSWINDOWS) or Defined(_MACOS)}
    procedure ApplicationExceptionHandler(Sender: TObject; E: Exception);
    {$ENDIF}
    procedure OnLocalFrameAvailable(Sender: TObject);
    procedure OnRemoteFrameAvailable(Sender: TObject);
    procedure onLocalDescription(Sender: TObject; const aType: TALWebRTCSDPType; const aDescription: String);
    procedure onIceCandidate(Sender: TObject; const aIceCandidate: TALWebRTCIceCandidate);
    procedure onIceConnectionChange(Sender: TObject; const aNewState: TALWebRTCIceConnectionState);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  system.math,
  system.math.vectors,
  fmx.platform,
  Grijjy.ErrorReporting,
  {$IF defined(ANDROID)}
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.JavaTypes,
  FMX.Canvas.GPU.Helpers,
  Androidapi.Gles2,
  Androidapi.JNI.Net,
  Androidapi.JNIBridge,
  Androidapi.Helpers,
  Androidapi.JNI.Os,
  Androidapi.JNI.Location,
  Androidapi.JNI.Provider,
  Androidapi.JNI.App,
  Posix.Sched,
  FMX.Helpers.Android,
  FMX.Platform.Android,
  fmx.Context.GLES.Android,
  Alcinoe.AndroidApi.Common,
  {$ENDIF}
  Alcinoe.StringUtils;

{********************************************}
function _EnableDeviceCameraAndAudio: boolean;
{$IF defined(ANDROID)}
var aIntent: JIntent;
    aPermissions: TJavaObjectArray<JString>;
    aSharedPreferences: JSharedPreferences;
    aPreferenceEditor: JSharedPreferences_Editor;
    aUri: Jnet_Uri;
{$ENDIF}
begin

  result := True;

  {$IF defined(ANDROID)}
  if ((TJBuild_VERSION.JavaClass.SDK_INT >= 23) and {marshmallow}
      ((MainActivity.checkSelfPermission(StringToJString('android.permission.CAMERA')) <> TJPackageManager.JavaClass.PERMISSION_GRANTED) or
       (MainActivity.checkSelfPermission(StringToJString('android.permission.RECORD_AUDIO')) <> TJPackageManager.JavaClass.PERMISSION_GRANTED))) then begin

    result := False;

    //shouldShowRequestPermissionRationale return
    // * true �  if the permission is already requested before but was denied
    // * false � If the permission is requested first time.
    //           If the permission is disabled by some device policy or the permission is already requested but the user denied it with checking Never ask again option in the permission dialog
    aSharedPreferences := TJPreferenceManager.javaclass.getDefaultSharedPreferences(TAndroidHelper.Context.getApplicationContext);
    if (not aSharedPreferences.getBoolean(StringToJString('permission_access_camera_and_audio_requested'), false)) or // << we never requested the camera and audio permission
       (((MainActivity.checkSelfPermission(StringToJString('android.permission.CAMERA')) = TJPackageManager.JavaClass.PERMISSION_GRANTED) or
         (MainActivity.shouldShowRequestPermissionRationale(StringToJString('android.permission.CAMERA')))) and // << the CAMERA permission was denied without checking Never ask
        ((MainActivity.checkSelfPermission(StringToJString('android.permission.RECORD_AUDIO')) = TJPackageManager.JavaClass.PERMISSION_GRANTED) or
         (MainActivity.shouldShowRequestPermissionRationale(StringToJString('android.permission.RECORD_AUDIO'))))) then begin  // << the RECORD_AUDIO permission was denied without checking Never ask

      aPreferenceEditor := aSharedPreferences.edit;
      aPreferenceEditor.putBoolean(StringToJstring('permission_access_camera_and_audio_requested'), true);
      aPreferenceEditor.commit;
      //----
      aPermissions := TJavaObjectArray<JString>.create(2);
      try
        aPermissions.Items[0] := StringToJString('android.permission.CAMERA');
        aPermissions.Items[1] := StringToJString('android.permission.RECORD_AUDIO');
        MainActivity.requestPermissions(aPermissions, 0{requestCode});
      finally
        ALFreeAndNil(aPermissions);
      end;

    end
    else begin

      aIntent := TJIntent.JavaClass.init(TJSettings.JavaClass.ACTION_APPLICATION_DETAILS_SETTINGS);
      aUri := TJnet_Uri.JavaClass.fromParts(StringToJString('package'), TAndroidHelper.Context.getPackageName(), nil);
      aIntent.setData(aUri);
      TAndroidHelper.Context.startActivity(aIntent);

    end;
  end;
  {$ENDIF}

end;

{************************************************}
procedure TForm1.ButtonCallClick(Sender: TObject);
var LIceServers: TALWebRTCIceServers;
    LServerUrl: String;
begin

  if not _EnableDeviceCameraAndAudio then exit;
  LServerUrl := ServerUrlEdit.text;
  ServerUrlEdit.visible := False;
  TextIntro.Visible := False;
  ButtonCall.Enabled := false;
  ButtonHangUp.Enabled := True;

  Randomize;
  fUserID := Random(Maxint) * Random(maxint);
  fMustcheckoffer := false;
  fMustcheckanswer := false;

  setlength(LIceServers, 6);
  with LIceServers[0] do begin
    uri := 'stun:eu-turn1.xirsys.com';
    username := '';
    password := '';
  end;
  //-----
  with LIceServers[0] do begin
    uri := 'turn:eu-turn1.xirsys.com:80?transport=udp';
    username := '54c4553a-0427-11e9-bbd8-e8cee7e120e9';
    password := '54c455b2-0427-11e9-909e-55b82d8eaa45';
  end;
  //-----
  with LIceServers[1] do begin
    uri := 'turn:eu-turn1.xirsys.com:3478?transport=udp';
    username := '54c4553a-0427-11e9-bbd8-e8cee7e120e9';
    password := '54c455b2-0427-11e9-909e-55b82d8eaa45';
  end;
  //-----
  with LIceServers[2] do begin
    uri := 'turn:eu-turn1.xirsys.com:80?transport=tcp';
    username := '54c4553a-0427-11e9-bbd8-e8cee7e120e9';
    password := '54c455b2-0427-11e9-909e-55b82d8eaa45';
  end;
  //-----
  with LIceServers[3] do begin
    uri := 'turn:eu-turn1.xirsys.com:3478?transport=tcp';
    username := '54c4553a-0427-11e9-bbd8-e8cee7e120e9';
    password := '54c455b2-0427-11e9-909e-55b82d8eaa45';
  end;
  //-----
  with LIceServers[4] do begin
    uri := 'turns:eu-turn1.xirsys.com:443?transport=tcp';
    username := '54c4553a-0427-11e9-bbd8-e8cee7e120e9';
    password := '54c455b2-0427-11e9-909e-55b82d8eaa45';
  end;
  //-----
  with LIceServers[5] do begin
    uri := 'turns:eu-turn1.xirsys.com:5349?transport=tcp';
    username := '54c4553a-0427-11e9-bbd8-e8cee7e120e9';
    password := '54c455b2-0427-11e9-909e-55b82d8eaa45';
  end;

  fWebRTC := TalWebRTC.Create(LIceServers, TALWebRTCPeerConnectionParameters.Create);
  fWebRTC.OnLocalFrameAvailable := OnLocalFrameAvailable;
  fWebRTC.OnRemoteFrameAvailable := OnRemoteFrameAvailable;
  fWebRTC.OnLocalDescription := OnLocalDescription;
  fWebRTC.OnIceCandidate := OnIceCandidate;
  fWebRTC.onIceConnectionChange := onIceConnectionChange;
  fWebRTC.Start;

  TThread.CreateAnonymousThread(
    procedure
    var lHTTP: TIdHTTP;
        lParamList: TStringList;
        S1: String;
    begin

      try

        lParamList := TStringList.Create;
        lParamList.Add('user_id=' + inttostr(fUserID));
        lParamList.Add('action=enter');

        lHTTP := TIdHTTP.Create;
        try
          ALLog('alwebrtc', 'enter', TALLogType.VERBOSE);
          S1 := lHTTP.Post(LServerUrl, lParamList);
          if S1 <> '' then begin
            fMustcheckoffer := false;
            fMustcheckanswer := true;
            TThread.Synchronize(nil,
              procedure
              begin
                if fWebRTC = nil then exit;
                ALLog('alwebrtc', 'createOffer', TALLogType.VERBOSE);
                fWebRTC.createOffer;
              end);
          end
          else begin
            fMustcheckoffer := true;
            fMustcheckanswer := false;
          end;
        finally
          lHTTP.Free;
          lParamList.Free;
        end;

        while True do begin

          if not ButtonHangUp.Enabled then begin
            TThread.Synchronize(nil,
              procedure
              begin
                ButtonCall.Enabled := True;
              end);
            exit;
          end;

          sleep(3000);

          if fMustcheckoffer then begin

            lParamList := TStringList.Create;
            lParamList.Add('user_id=' + inttostr(fUserID));
            lParamList.Add('action=check_offer');

            lHTTP := TIdHTTP.Create;
            try
              ALLog('alwebrtc', 'check_offer', TALLogType.VERBOSE);
              S1 := lHTTP.Post(LServerUrl, lParamList);
              if (S1 <> '') then begin
                TThread.Synchronize(nil,
                  procedure
                  begin
                    if fWebRTC = nil then exit;
                    ALLog('alwebrtc', 'setRemoteDescription(offer)', TALLogType.VERBOSE);
                    fWebRTC.setRemoteDescription(TALWebRTCSDPType.OFFER, S1);
                    fWebRTC.createAnswer;
                    fMustcheckoffer := false;
                  end);
              end;
            finally
              lHTTP.Free;
              lParamList.Free;
            end;

          end
          else if fMustcheckanswer then begin

            lParamList := TStringList.Create;
            lParamList.Add('user_id=' + inttostr(fUserID));
            lParamList.Add('action=check_answer');

            lHTTP := TIdHTTP.Create;
            try
              ALLog('alwebrtc', 'check_answer', TALLogType.VERBOSE);
              S1 := lHTTP.Post(LServerUrl, lParamList);
              if (S1 <> '') then begin
                TThread.Synchronize(nil,
                  procedure
                  begin
                    if fWebRTC = nil then exit;
                    ALLog('alwebrtc', 'setRemoteDescription(answer)', TALLogType.VERBOSE);
                    fWebRTC.setRemoteDescription(TALWebRTCSDPType.answer, S1);
                    fMustcheckanswer := False;
                  end);
              end;
            finally
              lHTTP.Free;
              lParamList.Free;
            end;

          end;

          lParamList := TStringList.Create;
          lParamList.Add('user_id=' + inttostr(fUserID));
          lParamList.Add('action=check_candidate');

          lHTTP := TIdHTTP.Create;
          try
            ALLog('alwebrtc', 'check_candidate', TALLogType.VERBOSE);
            S1 := lHTTP.Post(LServerUrl, lParamList);
            if (S1 <> '') then begin
              TThread.Synchronize(nil,
                procedure
                var aLst: TstringList;
                    j: integer;
                begin
                  if fWebRTC = nil then exit;
                  aLst := TstringList.Create;
                  try
                    aLst.Text := S1;
                    J := 0;
                    while j < aLst.Count - 2  do begin
                      ALLog('alwebrtc', 'addRemoteIceCandidate', TALLogType.VERBOSE);
                      fWebRTC.addRemoteIceCandidate(
                        TALWebRTCIceCandidate.Create(
                          aLst[j],
                          StrToInt(aLst[j+1]),
                          aLst[j+2]));
                      j := j + 3;
                    end;
                  finally
                    aLst.Free;
                  end;
                end);
            end;
          finally
            lHTTP.Free;
            lParamList.Free;
          end;

        end;

      except
        on E: exception do begin
          allog('TForm1.Button2Click', E.Message, TalLogType.ERROR);
        end;

      end;
    end).Start;

end;

{**************************************************}
procedure TForm1.ButtonHangUpClick(Sender: TObject);
begin
  ServerUrlEdit.visible := true;
  ButtonHangUp.Enabled := false;
  ALFreeAndNil(fWebRTC);
end;

{*******************************************}
procedure TForm1.FormCreate(Sender: TObject);
begin

  ALLog('TForm1.FormCreate', 'TForm1.FormCreate', TalLogType.verbose);

  {$IF Defined(IOS) or Defined(ANDROID)}
  Application.OnException := TgoExceptionReporter.ExceptionHandler;
  TMessageManager.DefaultManager.SubscribeToMessage(TgoExceptionReportMessage, ApplicationExceptionHandler);
  {$ELSE}
  Application.OnException := ApplicationExceptionHandler;
  {$ENDIF}

  fWebRTC := nil;

end;

{*******************************************}
procedure TForm1.FormResize(Sender: TObject);
begin
  RemoteCameraLayout.Width := max(height, width);
  RemoteCameraLayout.height := RemoteCameraLayout.Width;
  RemoteCameraLayout.Position.X := - (RemoteCameraLayout.Width - width) / 2;
  RemoteCameraLayout.Position.Y := - (RemoteCameraLayout.height - height) / 2;
  LocalCameraLayout.Position.X := width - LocalCameraLayout.width - 16;
  LocalCameraLayout.Position.Y := Height - LocalCameraLayout.Height - 16;
end;

{**************************************************************************************************************}
procedure TForm1.onLocalDescription(Sender: TObject; const aType: TALWebRTCSDPType; const aDescription: String);
var LServerUrl: String;
begin
  LServerUrl := ServerUrlEdit.text;
  if aType = TALWebRTCSDPType.OFFER then begin

    TThread.CreateAnonymousThread(
      procedure
      var lHTTP: TIdHTTP;
          lParamList: TStringList;
          S1: String;
      begin

        try

          lParamList := TStringList.Create;
          lParamList.Add('user_id=' + inttostr(fUserID));
          lParamList.Add('action=set_offer');
          lParamList.Add('sdp_offer='+ aDescription);

          lHTTP := TIdHTTP.Create;
          try
            ALLog('alwebrtc.onLocalDescription', 'set_offer', TALLogType.VERBOSE);
            S1 := lHTTP.Post(LServerUrl, lParamList);
          finally
            lHTTP.Free;
            lParamList.Free;
          end;

        except
          on E: exception do begin
            allog('alwebrtc.onLocalDescription', E.Message, TalLogType.ERROR);
          end;

        end;
      end).Start;

  end
  else if aType = TALWebRTCSDPType.ANSWER then begin

    TThread.CreateAnonymousThread(
      procedure
      var lHTTP: TIdHTTP;
          lParamList: TStringList;
          S1: String;
      begin

        try

          lParamList := TStringList.Create;
          lParamList.Add('user_id=' + inttostr(fUserID));
          lParamList.Add('action=set_answer');
          lParamList.Add('sdp_answer='+ aDescription);

          lHTTP := TIdHTTP.Create;
          try
            ALLog('alwebrtc.onLocalDescription', 'set_answer', TALLogType.VERBOSE);
            S1 := lHTTP.Post(LServerUrl, lParamList);
            if S1 <> 'OK' then raise Exception.Create('Error 9A23C300-B64F-4C8F-989A-9D462B1D169D');
          finally
            lHTTP.Free;
            lParamList.Free;
          end;

        except
          on E: exception do begin
            allog('TForm1.onLocalDescription', E.Message, TalLogType.ERROR);
          end;

        end;
      end).Start;

  end;
end;

{*******************************************************************************************}
procedure TForm1.onIceCandidate(Sender: TObject; const aIceCandidate: TALWebRTCIceCandidate);
var LTMPIceCandidate: TALWebRTCIceCandidate;
    LServerUrl: String;
begin

  LServerUrl := ServerUrlEdit.text;
  LTMPIceCandidate := aIceCandidate; // else can not capture symbol error under win32

  TThread.CreateAnonymousThread(
    procedure
    var lHTTP: TIdHTTP;
        lParamList: TStringList;
        S1: String;
    begin

      try

        lParamList := TStringList.Create;
        lParamList.Add('user_id=' + inttostr(fUserID));
        lParamList.Add('action=set_candidate');
        lParamList.Add('SdpMid='+ LTMPIceCandidate.SdpMid);
        lParamList.Add('SdpMLineIndex='+ inttostr(LTMPIceCandidate.SdpMLineIndex));
        lParamList.Add('Sdp='+ LTMPIceCandidate.Sdp);

        lHTTP := TIdHTTP.Create;
        try
          ALLog('alwebrtc.onIceCandidate', 'set_candidate', TALLogType.VERBOSE);
          S1 := lHTTP.Post(LServerUrl, lParamList);
        finally
          lHTTP.Free;
          lParamList.Free;
        end;

      except
        on E: exception do begin
          allog('alwebrtc.onIceCandidate', E.Message, TalLogType.ERROR);
        end;

      end;
    end).Start;

end;

{****************************************************************************************************}
procedure TForm1.onIceConnectionChange(Sender: TObject; const aNewState: TALWebRTCIceConnectionState);
begin
  case aNewState of
    TALWebRTCIceConnectionState.CHECKING:;
    TALWebRTCIceConnectionState.CLOSED:;
    TALWebRTCIceConnectionState.COMPLETED:;
    TALWebRTCIceConnectionState.CONNECTED:;
    TALWebRTCIceConnectionState.DISCONNECTED:;
    TALWebRTCIceConnectionState.FAILED: begin
                                          ButtonHangUpClick(nil);
                                        end;
    TALWebRTCIceConnectionState.NEW:;
  end;
end;

{******************************************************}
procedure TForm1.OnLocalFrameAvailable(Sender: TObject);
begin
  if fWebRTC = nil then exit;
  LocalCameraLayout.RotationAngle := fWebRtc.LocalBitmapRotation;
  LocalCameraLayout.repaint;
end;

{*******************************************************}
procedure TForm1.OnRemoteFrameAvailable(Sender: TObject);
begin
  if fWebRTC = nil then exit;
  RemoteCameraLayout.RotationAngle := fWebRtc.remoteBitmapRotation;
  RemoteCameraLayout.repaint;
end;

{**********************************************************************************************}
procedure TForm1.RemoteCameraLayoutPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
{$IF defined(android) or defined(ios)}
var aDestRect: TrectF;
{$ENDIF}
begin
  {$IF defined(android) or defined(ios)}
  if (fWebRTC = nil) or (fWebRTC.Remotebitmap = nil) then exit;
  aDestRect := canvas.AlignToPixel(
                 TRectF.Create(0, 0, fWebRTC.Remotebitmap.Width, fWebRTC.Remotebitmap.Height).
                   FitInto(RemoteCameraLayout.LocalRect));
  TCustomCanvasGpu(Canvas).DrawTexture(
    aDestRect, // ATexRect
    TRectF.Create(
      0,
      0,
      fWebRTC.RemoteBitmap.Width,
      fWebRTC.RemoteBitmap.Height), // ARect
    ALPrepareColor(TCustomCanvasGpu.ModulateColor, RemoteCameraLayout.AbsoluteOpacity), // https://quality.embarcadero.com/browse/RSP-15432
    fWebRTC.RemoteBitmap);
  {$ENDIF}
end;

{*********************************************************************************************}
procedure TForm1.LocalCameraLayoutPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
{$IF defined(android) or defined(ios)}
var aDestRect: TrectF;
{$ENDIF}
begin
  {$IF defined(android) or defined(ios)}
  if (fWebRTC = nil) or (fWebRTC.LocalBitmap = nil) then exit;
  aDestRect := canvas.AlignToPixel(
                 TRectF.Create(0, 0, fWebRTC.LocalBitmap.Width, fWebRTC.LocalBitmap.Height).
                   FitInto(LocalCameraLayout.LocalRect));
  TCustomCanvasGpu(Canvas).DrawTexture(
    aDestRect, // ATexRect
    TRectF.Create(
      0,
      0,
      fWebRTC.LocalBitmap.Width,
      fWebRTC.LocalBitmap.Height), // ARect
    ALPrepareColor(TCustomCanvasGpu.ModulateColor, LocalCameraLayout.AbsoluteOpacity), // https://quality.embarcadero.com/browse/RSP-15432
    fWebRTC.LocalBitmap);
  {$ENDIF}
end;

{************************************}
{$IF Defined(IOS) or Defined(ANDROID)}
procedure TForm1.ApplicationExceptionHandler(const Sender: TObject; const M: TMessage);
var aReport: IgoExceptionReport;
begin

  aReport := TgoExceptionReportMessage(M).Report;
  allog('ERROR', aReport.Report, TalLogType.error);

  {$IF Defined(IOS)}
  TThread.CreateAnonymousThread(
    procedure
    begin
      TThread.Synchronize(nil,
        procedure
        begin
          Halt(1); // << This is the only way i found to crash the app :(
        end);
    end).Start;
  {$ELSE}
  Application.Terminate;
  {$ENDIF}

end;
{$ENDIF}

{*****************************************}
{$IF Defined(MSWINDOWS) or Defined(_MACOS)}
procedure TForm1.ApplicationExceptionHandler(Sender: TObject; E: Exception);
begin
  Application.Terminate;
end;
{$ENDIF}

initialization
  {$IFDEF DEBUG}
  ReportMemoryleaksOnSHutdown := True;
  {$ENDIF}
  SetMultiByteConversionCodePage(CP_UTF8);

end.
