unit main;

//
// This demo is very drafty, normally we must not use http server for this and
// must must use socket client (or similar like notification), but i was lazzy
// to do for this demo
//
// do not forget to launch the ALLiveVideoChatServer and update below the value
// of <Server_url>
//

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, ALWebRTC,
  FMX.Controls.Presentation, FMX.StdCtrls, ALCommon, ALFMXTypes3D, FMX.Objects,
  FMX.Canvas.GPU, algraphics, FMX.Types3D, FMX.Effects, FMX.Filter.Effects,
  ALFmxStdCtrls, ALFmxLayouts, FMX.Layouts, system.messaging,
  IdHTTP, ALFmxFilterEffects, FMX.Edit;

type
  TForm1 = class(TForm)
    LocalCameraLayout: TALLayout;
    ButtonCall: TButton;
    RemoteCameraLayout: TALLayout;
    ButtonHangUp: TButton;
    ServerUrlEdit: TEdit;
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

const Server_url = 'http://192.168.0.10:3030';

implementation

{$R *.fmx}

uses system.math,
     system.math.vectors,
     fmx.platform,
     Grijjy.ErrorReporting,
     alString;

{************************************************}
procedure TForm1.ButtonCallClick(Sender: TObject);
var aIceServers: TALWebRTCIceServers;
begin


  ServerUrlEdit.visible := False;
  ButtonCall.Enabled := false;
  ButtonHangUp.Enabled := True;

  Randomize;
  fUserID := Random(Maxint) * Random(maxint);
  fMustcheckoffer := false;
  fMustcheckanswer := false;

  setlength(aIceServers, 6);
  with aIceServers[0] do begin
    uri := 'stun:eu-turn1.xirsys.com';
    username := '';
    password := '';
  end;
  //-----
  with aIceServers[0] do begin
    uri := 'turn:eu-turn1.xirsys.com:80?transport=udp';
    username := '54c4553a-0427-11e9-bbd8-e8cee7e120e9';
    password := '54c455b2-0427-11e9-909e-55b82d8eaa45';
  end;
  //-----
  with aIceServers[1] do begin
    uri := 'turn:eu-turn1.xirsys.com:3478?transport=udp';
    username := '54c4553a-0427-11e9-bbd8-e8cee7e120e9';
    password := '54c455b2-0427-11e9-909e-55b82d8eaa45';
  end;
  //-----
  with aIceServers[2] do begin
    uri := 'turn:eu-turn1.xirsys.com:80?transport=tcp';
    username := '54c4553a-0427-11e9-bbd8-e8cee7e120e9';
    password := '54c455b2-0427-11e9-909e-55b82d8eaa45';
  end;
  //-----
  with aIceServers[3] do begin
    uri := 'turn:eu-turn1.xirsys.com:3478?transport=tcp';
    username := '54c4553a-0427-11e9-bbd8-e8cee7e120e9';
    password := '54c455b2-0427-11e9-909e-55b82d8eaa45';
  end;
  //-----
  with aIceServers[4] do begin
    uri := 'turns:eu-turn1.xirsys.com:443?transport=tcp';
    username := '54c4553a-0427-11e9-bbd8-e8cee7e120e9';
    password := '54c455b2-0427-11e9-909e-55b82d8eaa45';
  end;
  //-----
  with aIceServers[5] do begin
    uri := 'turns:eu-turn1.xirsys.com:5349?transport=tcp';
    username := '54c4553a-0427-11e9-bbd8-e8cee7e120e9';
    password := '54c455b2-0427-11e9-909e-55b82d8eaa45';
  end;

  fWebRTC := TalWebRTC.Create(aIceServers, TALWebRTCPeerConnectionParameters.Create);
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
          S1 := lHTTP.Post(Server_url, lParamList);
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
              S1 := lHTTP.Post(Server_url, lParamList);
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
              S1 := lHTTP.Post(Server_url, lParamList);
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
            S1 := lHTTP.Post(Server_url, lParamList);
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
  ServerUrlEdit.text := Server_url;

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
begin
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
            S1 := lHTTP.Post(ServerUrlEdit.text, lParamList);
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
            S1 := lHTTP.Post(ServerUrlEdit.text, lParamList);
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
var aTMPIceCandidate: TALWebRTCIceCandidate;
begin

  aTMPIceCandidate := aIceCandidate; // else can not capture symbol error under win32

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
        lParamList.Add('SdpMid='+ aTMPIceCandidate.SdpMid);
        lParamList.Add('SdpMLineIndex='+ inttostr(aTMPIceCandidate.SdpMLineIndex));
        lParamList.Add('Sdp='+ aTMPIceCandidate.Sdp);

        lHTTP := TIdHTTP.Create;
        try
          ALLog('alwebrtc.onIceCandidate', 'set_candidate', TALLogType.VERBOSE);
          S1 := lHTTP.Post(ServerUrlEdit.text, lParamList);
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
  TCustomCanvasGpu(Canvas).DrawTexture(aDestRect, // ATexRect
                                       TRectF.Create(0,
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
  TCustomCanvasGpu(Canvas).DrawTexture(aDestRect, // ATexRect
                                       TRectF.Create(0,
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

end.
