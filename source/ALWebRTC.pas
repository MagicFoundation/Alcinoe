unit ALWebRTC;

interface

// Mandatory permissions for android :
// <uses-permission android:name="android.permission.CAMERA" />
// <uses-permission android:name="android.permission.CHANGE_NETWORK_STATE" />
// <uses-permission android:name="android.permission.MODIFY_AUDIO_SETTINGS" />
// <uses-permission android:name="android.permission.RECORD_AUDIO" />
// <uses-permission android:name="android.permission.BLUETOOTH" />
// <uses-permission android:name="android.permission.INTERNET" />
// <uses-permission android:name="android.permission.WRITE_EXTERNAL_STORAGE"/>
// <uses-permission android:name="android.permission.ACCESS_NETWORK_STATE" />
// <uses-permission android:name="android.permission.CAPTURE_VIDEO_OUTPUT" />
//
// Common video Resolutions
// 4K (3840 x 2160)
// Full HD (1920 x 1080)
// HD (1280 x 720) => recommended
// VGA (640 x 480)
// QVGA (320 x 240)
//
// Common FPS settings
// 30 fps
// 15 fps

{$IF Defined(ANDROID) or defined(IOS)}
  {$DEFINE _USE_TEXTURE}
{$ENDIF}

{$IF defined(MACOS) and not defined(IOS)}
  {$DEFINE _MACOS}
{$ENDIF}

uses System.sysUtils,
     {$IF defined(android)}
     Androidapi.JNI.JavaTypes,
     Androidapi.JNIBridge,
     FMX.Types3D,
     ALAndroidWebRTCApi,
     {$ELSEIF defined(IOS)}
     System.syncObjs,
     System.generics.collections,
     Macapi.CoreFoundation,
     iOSapi.CoreVideo,
     iOSapi.CoreGraphics,
     iOSapi.Foundation,
     iOSapi.AVFoundation,
     iOSapi.CocoaTypes,
     Macapi.ObjectiveC,
     ALIosWebRTCApi,
     ALFmxTypes3D,
     {$ELSE}
     FMX.graphics,
     {$ENDIF}
     system.Classes;

type

  {~~~~~~~~~~~~~~~~}
  TALWebRTC = class;

  {~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALWebRTCIceServer = record
    uri: String;
    username: String;
    password: String;
    class function Create(const aUri: String;
                          const aUsername: String;
                          const aPassword: String): TALWebRTCIceServer; static;
  end;
  TALWebRTCIceServers = Tarray<TALWebRTCIceServer>;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALWebRTCIceCandidate = record
    SdpMid: String;
    SdpMLineIndex: Integer;
    Sdp: String;
    class function Create(const aSdpMid: String;
                          const aSdpMLineIndex: Integer;
                          const aSdp: String): TALWebRTCIceCandidate; static;
  end;
  TALWebRTCIceCandidates = Tarray<TALWebRTCIceCandidate>;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALWebRTCIceConnectionState = (CHECKING, //The ICE agent has been given one or more remote candidates and is checking pairs of
                                           //local and remote candidates against one another to try to find a compatible match,
                                           //but has not yet found a pair which will allow the peer connection to be made.
                                           //It's possible that gathering of candidates is also still underway.
                                 CLOSED, // The ICE agent for this RTCPeerConnection has shut down and is no longer handling requests.
                                 COMPLETED, // The ICE agent has finished gathering candidates, has checked all pairs against one another,
                                            // and has found a connection for all components.
                                 CONNECTED, // A usable pairing of local and remote candidates has been found for all components of the connection,
                                            // and the connection has been established. It's possible that gathering is still underway, and it's
                                            // also possible that the ICE agent is still checking candidates against one another looking for a
                                            // better connection to use.
                                 DISCONNECTED, // Checks to ensure that components are still connected failed for at least one component of the RTCPeerConnection.
                                               // This is a less stringent test than "failed" and may trigger intermittently and resolve just as spontaneously on
                                               // less reliable networks, or during temporary disconnections. When the problem resolves, the connection may
                                               // return to the "connected" state.
                                 FAILED, // The ICE candidate has checked all candidates pairs against one another and has failed to find compatible
                                         // matches for all components of the connection. It is, however, possible that the ICE agent did find
                                         // compatible connections for some components.
                                 NEW); // The ICE agent is gathering addresses or is waiting to be given remote candidates through calls to
                                       // RTCPeerConnection.addIceCandidate() (or both).
  TALWebRTCSDPType = (OFFER, // The session description object describes the initial proposal in an offer/answer exchange. The session negotiation
                             // process begins with an offer being sent from the caller to the callee.
                      ANSWER, // The SDP contained in the sdp property is the definitive choice in the exchange. In other words, this session
                              // description describes the agreed-upon configuration, and is being sent to finalize negotiation.
                      PRANSWER); // The session description object describes a provisional answer; that is, it's a response to a previous offer or provisional answer.
  TALWebRTCLocalDescriptionEvent = procedure(Sender: TObject; const aType: TALWebRTCSDPType; const aDescription: String) of object;
  TALWebRTCIceCandidateEvent = procedure(Sender: TObject; const aIceCandidate: TALWebRTCIceCandidate) of object;
  TALWebRTCIceCandidatesRemovedEvent = procedure(Sender: TObject; const aIceCandidates: TALWebRTCIceCandidates) of object;
  TALWebRTCIceConnectionChangeEvent = procedure(Sender: TObject; const aNewState: TALWebRTCIceConnectionState) of object;
  TALWebRTCErrorEvent = procedure(Sender: TObject; const aCode: integer; const aDescription: String) of object;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALWebRTCPeerConnectionParameters = record
    videoCallEnabled: boolean; // Enable video in a call.
    videoWidth: integer;
    videoHeight: integer;
    videoFps: integer; // Camera fps.
    videoMaxBitrate: integer; // Video encoder maximum bitrate in kbps.
    videoCodec: String; // Default video codec. VP8 / VP9 / H264 Baseline / H264 High
    videoCodecHwAcceleration: boolean; // Use hardware accelerated video codec (if available).
    audioStartBitrate: integer; // Audio codec bitrate in kbps.
    audioCodec: String; // Default audio codec. OPUS / ISAC
    noAudioProcessing: boolean; // Disable audio processing pipeline.
    aecDump: boolean; // Enable diagnostic audio recordings.
    disableBuiltInAEC: boolean; // Disable hardware AEC.
    disableBuiltInNS: boolean; // Disable hardware NS.
    dataChannelEnabled: boolean;  // Enable datachannel.
    dataChannelOrdered: boolean; // Order messages.
    dataChannelMaxRetransmitTimeMs: integer; // Max delay to retransmit.
    dataChannelMaxRetransmits: integer; // Max attempts to retransmit.
    dataChannelProtocol: String; // Subprotocol.
    dataChannelNegotiated: boolean; // Negotiated.
    dataChannelId: integer; // data channel id.
    class function Create(const aVideoCallEnabled: boolean = true;
                          const aVideoWidth: integer = 1920;
                          const aVideoHeight: integer = 1080;
                          const aVideoFps: integer = 0;
                          const aVideoMaxBitrate: integer = 0;
                          const aVideoCodec: String = 'VP8';
                          const aVideoCodecHwAcceleration: boolean = true;
                          const aAudioStartBitrate: integer = 0;
                          const aAudioCodec: String = 'opus';
                          const aNoAudioProcessing: boolean = false;
                          const aAecDump: boolean = false;
                          const aDisableBuiltInAEC: boolean = false;
                          const aDisableBuiltInNS: boolean = false;
                          const aDataChannelEnabled: boolean = false;
                          const aDataChannelOrdered: boolean = true;
                          const aDataChannelMaxRetransmitTimeMs: integer = -1;
                          const aDataChannelMaxRetransmits: integer = -1;
                          const aDataChannelProtocol: String = '';
                          const aDataChannelNegotiated: boolean = false;
                          const aDataChannelId: integer = -1): TALWebRTCPeerConnectionParameters; static;
  end;


  {$REGION ' IOS'}
  {$IF defined(ios)}

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  //We need a background thread because of this:
  //https://stackoverflow.com/questions/53828360/about-deadlock-with-tthread-synchronize?noredirect=1#comment94506034_53828360
  //especially setRemoteDescription that fire renderFrame
  TALiOSWebRTC = class(TThread)
  private
    const kARDMediaStreamId = 'ARDAMS';
    const kARDAudioTrackId = 'ARDAMSa0';
    const kARDVideoTrackId = 'ARDAMSv0';
    const kFramerateLimit: float64 = 30.0;
    const kARDVideoTrackKind = 'video';
  private
    type

      {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
      TPeerConnectionDelegate = class(TOCLocal, RTCPeerConnectionDelegate)
      strict private
        [Weak] fiOSWebRTC: TALiOSWebRTC;
      public
        constructor Create(const aiOSWebRTC: TALiOSWebRTC);
        [MethodName('peerConnection:didChangeSignalingState:')]
        procedure peerConnectionDidChangeSignalingState(peerConnection: RTCPeerConnection; didChangeSignalingState: RTCSignalingState); cdecl;
        [MethodName('peerConnection:didAddStream:')]
        procedure peerConnectionDidAddStream(peerConnection: RTCPeerConnection; didAddStream: RTCMediaStream); cdecl;
        [MethodName('peerConnection:didRemoveStream:')]
        procedure peerConnectionDidRemoveStream(peerConnection: RTCPeerConnection; didRemoveStream: RTCMediaStream); cdecl;
        procedure peerConnectionShouldNegotiate(peerConnection: RTCPeerConnection); cdecl;
        [MethodName('peerConnection:didChangeIceConnectionState:')]
        procedure peerConnectionDidChangeIceConnectionState(peerConnection: RTCPeerConnection; didChangeIceConnectionState: RTCIceConnectionState); cdecl;
        [MethodName('peerConnection:didChangeIceGatheringState:')]
        procedure peerConnectionDidChangeIceGatheringState(peerConnection: RTCPeerConnection; didChangeIceGatheringState: RTCIceGatheringState); cdecl;
        [MethodName('peerConnection:didGenerateIceCandidate:')]
        procedure peerConnectionDidGenerateIceCandidate(peerConnection: RTCPeerConnection; didGenerateIceCandidate: RTCIceCandidate); cdecl;
        [MethodName('peerConnection:didRemoveIceCandidates:')]
        procedure peerConnectionDidRemoveIceCandidates(peerConnection: RTCPeerConnection; didRemoveIceCandidates: NSArray); cdecl;
        [MethodName('peerConnection:didOpenDataChannel:')]
        procedure peerConnectionDidOpenDataChannel(peerConnection: RTCPeerConnection; didOpenDataChannel: RTCDataChannel); cdecl;
        [MethodName('peerConnection:didStartReceivingOnTransceiver:')]
        procedure peerConnectionDidStartReceivingOnTransceiver(peerConnection: RTCPeerConnection; didStartReceivingOnTransceiver: RTCRtpTransceiver); cdecl;
        [MethodName('peerConnection:didAddReceiver:streams:')]
        procedure peerConnectionDidAddReceiverStreams(peerConnection: RTCPeerConnection; didAddReceiver: RTCRtpReceiver; streams: NSArray); cdecl;
        [MethodName('peerConnection:didRemoveReceiver:')]
        procedure peerConnectionDidRemoveReceiver(peerConnection: RTCPeerConnection; didRemoveReceiver: RTCRtpReceiver); cdecl;
      end;

      {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
      TLocalVideoTrackRenderer = class(TOCLocal, RTCVideoRenderer)
      strict private
        [Weak] fiOSWebRTC: TALiOSWebRTC;
      public
        constructor Create(const aiOSWebRTC: TALiOSWebRTC);
        procedure setSize(size: CGSize); cdecl;
        procedure renderFrame(frame: RTCVideoFrame); cdecl;
      end;

      {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
      TRemoteVideoTrackRenderer = class(TOCLocal, RTCVideoRenderer)
      strict private
        [Weak] fiOSWebRTC: TALiOSWebRTC;
      public
        constructor Create(const aiOSWebRTC: TALiOSWebRTC);
        procedure setSize(size: CGSize); cdecl;
        procedure renderFrame(frame: RTCVideoFrame); cdecl;
      end;

  Private
    [Weak] fWebRTC: TALWebRTC;
    FSignal: TEvent;
    fQueue: TQueue<TProc>;
    fLocalVideoTextureRefLuma: CVOpenGLESTextureRef;
    fLocalVideoTextureRefChroma: CVOpenGLESTextureRef;
    fRemoteVideoTextureRefLuma: CVOpenGLESTextureRef;
    fRemoteVideoTextureRefChroma: CVOpenGLESTextureRef;
    fvideoTextureCacheRef: CVOpenGLESTextureCacheRef;
    fIceServers: TALWebRTCIceServers;
    fPeerConnectionParameters: TALWebRTCPeerConnectionParameters;
    fPeerConnectionFactory: RTCPeerConnectionFactory;
    fPeerConnection: RTCPeerConnection;
    fPeerConnectionDelegate: TPeerConnectionDelegate;
    fCameraVideoCapturer: RTCCameraVideoCapturer;
    fVideoSource: RTCVideoSource;
    fLocalAudioTrack: RTCAudioTrack;
    fLocalVideoTrack: RTCVideoTrack;
    fLocalVideoTrackRenderer: TLocalVideoTrackRenderer;
    fRemoteVideoTrack: RTCVideoTrack;
    fRemoteVideoTrackRenderer: TRemoteVideoTrackRenderer;
    fLocalSdp: RTCSessionDescription;
    fQueuedRemoteCandidates: TList<TALWebRTCIceCandidate>;
    fIsInitiator: boolean;
    procedure CreateSessionDescriptionCompletionHandler(sdp: RTCSessionDescription; error: NSError);
    procedure SetSessionDescriptionCompletionHandler(error: NSError);
    function createSdpMediaConstraints: RTCMediaConstraints;
    procedure drainCandidates;
    function findDeviceForPosition(const aPosition: AVCaptureDevicePosition): AVCaptureDevice;
    function selectFormatForDevice(const aDevice: AVCaptureDevice; const aPreferredOutputPixelFormat: FourCharCode): AVCaptureDeviceFormat;
    function selectFpsForFormat(const aFormat: AVCaptureDeviceFormat): NSInteger;
  Protected
    procedure Execute; override;
    procedure Enqueue(const aProc: Tproc);
    procedure TerminatedSet; override;
  public
    constructor Create(const aWebRTC: TALWebRTC; const aIceServers: TALWebRTCIceServers; const aPeerConnectionParameters: TALWebRTCPeerConnectionParameters); virtual;
    destructor Destroy; override;
    function Start: boolean;
    procedure Stop;
    procedure createOffer;
    procedure createAnswer;
    procedure setRemoteDescription(const aSdpType: TALWebRTCSDPType; const aSdpDescription: String);
    procedure addRemoteIceCandidate({$IF not defined(CPUARM)}const aTmpIceCandidate: TALWebRTCIceCandidate{$ELSE}const aIceCandidate: TALWebRTCIceCandidate{$ENDIF});
    procedure removeRemoteIceCandidates(const aIceCandidates: Tarray<TALWebRTCIceCandidate>);
    Procedure changeCaptureFormat(const aWidth: integer; const aHeight: integer; const aFramerate: integer);
    Procedure setAudioEnabled(const enable: boolean);
    Procedure setVideoEnabled(const enable: boolean);
    function setVideoMaxBitrate(const maxBitrateKbps: integer): boolean;
  end;

  {$ENDIF}
  {$ENDREGION}

  {~~~~~~~~~~~~~~~~~~~~~~~~}
  TALWebRTC = class(TObject)
  private

    const ERROR_CREATE_SDP = 1;
    const ERROR_SET_SDP = 2;
    const ERROR_AUDIO_RECORD_INIT = 3;
    const ERROR_AUDIO_RECORD_START = 4;
    const ERROR_AUDIO_RECORD = 5;
    const ERROR_AUDIO_TRACK_INIT = 6;
    const ERROR_AUDIO_TRACK_START = 7;
    const ERROR_AUDIO_TRACK = 8;

  private

    {$REGION ' ANDROID'}
    {$IF defined(android)}

    type

      {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
      TAndroidWebRTCListener = class(TJavaLocal, JALWebRTC_Listener)
      private
        [Weak] fWebRTC: TALWebRTC;
      public
        constructor Create(const aWebRTC: TALWebRTC);
        procedure onLocalFrameAvailable(textureId: integer; width: integer; height: integer; rotation: integer); cdecl;
        procedure onRemoteFrameAvailable(textureId: integer; width: integer; height: integer; rotation: integer); cdecl;
        procedure onLocalDescription(sdp: JSessionDescription); cdecl;
        procedure onIceCandidate(candidate: JIceCandidate); cdecl;
        procedure onIceCandidatesRemoved(candidates: TJavaObjectArray<JIceCandidate>); cdecl;
        procedure onIceConnectionChange(newState: JPeerConnection_IceConnectionState); cdecl;
        procedure onError(code: Integer; description: JString); cdecl;
      end;

    {$ENDIF}
    {$ENDREGION}

  private

    {$REGION ' ANDROID'}
    {$IF defined(android)}
    fAndroidWebRTCListener: TAndroidWebRTCListener;
    fAndroidWebRTC: JALWebRTC;
    fLocalBitmap: TTexture;
    fRemoteBitmap: TTexture;
    function getLocalBitmap: TTexture;
    function getRemoteBitmap: TTexture;
    {$ENDIF}
    {$ENDREGION}

    {$REGION ' IOS'}
    {$IF defined(ios)}
    fiOSWebRTC: TALiOSWebRTC;
    fLocalBitmap: TALBiPlanarTexture;
    fRemoteBitmap: TALPlanarTexture;
    function getLocalBitmap: TALBiPlanarTexture;
    function getRemoteBitmap: TALPlanarTexture;
    {$ENDIF}
    {$ENDREGION}

  private

    FlocalBitmapRotation: integer;
    FRemoteBitmapRotation: integer;
    fOnLocalFrameAvailableEvent: TNotifyEvent;
    fOnRemoteFrameAvailableEvent: TNotifyEvent;
    fonLocalDescriptionEvent: TALWebRTCLocalDescriptionEvent;
    fonIceCandidateEvent: TALWebRTCIceCandidateEvent;
    fonIceCandidatesRemovedEvent: TALWebRTCIceCandidatesRemovedEvent;
    fonIceConnectionChangeEvent: TALWebRTCIceConnectionChangeEvent;
    fonErrorEvent: TALWebRTCErrorEvent;

  public

    {$REGION ' ANDROID'}
    {$IF defined(android)}
    property LocalBitmap: TTexture read getLocalBitmap;
    property RemoteBitmap: TTexture read getRemoteBitmap;
    {$ENDIF}
    {$ENDREGION}

    {$REGION ' IOS'}
    {$IF defined(ios)}
    property LocalBitmap: TALBiPlanarTexture read getLocalBitmap;
    property RemoteBitmap: TALPlanarTexture read getRemoteBitmap;
    {$ENDIF}
    {$ENDREGION}

  public
    constructor Create(const aIceServers: TALWebRTCIceServers; const aPeerConnectionParameters: TALWebRTCPeerConnectionParameters); virtual;
    destructor Destroy; override;
    function Start: boolean;
    procedure Stop;
    procedure createOffer;
    procedure createAnswer;
    procedure setRemoteDescription(const aSdpType: TALWebRTCSDPType; const aSdpDescription: String);
    procedure addRemoteIceCandidate(const aIceCandidate: TALWebRTCIceCandidate);
    procedure removeRemoteIceCandidates(const aIceCandidates: Tarray<TALWebRTCIceCandidate>);
    Procedure changeCaptureFormat(const aWidth: integer; const aHeight: integer; const aFramerate: integer);
    Procedure setAudioEnabled(const enable: boolean);
    Procedure setVideoEnabled(const enable: boolean);
    function setVideoMaxBitrate(const maxBitrateKbps: integer): boolean;
    property localBitmapRotation: integer read flocalBitmapRotation;
    property RemoteBitmapRotation: integer read fRemoteBitmapRotation;
    property OnLocalFrameAvailable: TNotifyEvent read fOnLocalFrameAvailableEvent write fOnLocalFrameAvailableEvent;
    property OnRemoteFrameAvailable: TNotifyEvent read fOnRemoteFrameAvailableEvent write fOnRemoteFrameAvailableEvent;
    property onLocalDescription: TALWebRTCLocalDescriptionEvent read fonLocalDescriptionEvent write fonLocalDescriptionEvent;
    property onIceCandidate: TALWebRTCIceCandidateEvent read fonIceCandidateEvent write fonIceCandidateEvent;
    property onIceCandidatesRemoved: TALWebRTCIceCandidatesRemovedEvent read fonIceCandidatesRemovedEvent write fonIceCandidatesRemovedEvent;
    property onIceConnectionChange: TALWebRTCIceConnectionChangeEvent read fonIceConnectionChangeEvent write fonIceConnectionChangeEvent;
    property onError: TALWebRTCErrorEvent read fonErrorEvent write fonErrorEvent;
  end;

implementation

uses System.messaging,
     FMX.platform,
     {$IF defined(android)}
     Androidapi.JNI.GraphicsContentViewText,
     Androidapi.jni,
     Androidapi.jni.App,
     Androidapi.Helpers,
     FMX.Context.GLES.Android,
     ALFMXTypes3D,
     {$ELSEIF defined(IOS)}
     system.math,
     Macapi.Helpers,
     iOSapi.OpenGLES,
     iOSapi.CoreMedia,
     iOSapi.Helpers,
     FMX.Consts,
     FMX.Types3D,
     FMX.Context.GLES.iOS,
     FMX.Context.GLES,
     {$ENDIF}
     AlString,
     alcommon;

{**********************************************************}
class function TALWebRTCIceServer.Create(const aUri: String;
                                         const aUsername: String;
                                         const aPassword: String): TALWebRTCIceServer;
begin
  with result do begin
    Uri := aUri;
    Username := aUsername;
    Password := aPassword;
  end;
end;

{****************************************************************}
class function TALWebRTCIceCandidate.Create(const aSdpMid: String;
                                            const aSdpMLineIndex: Integer;
                                            const aSdp: String): TALWebRTCIceCandidate;
begin
  with result do begin
    SdpMid := aSdpMid;
    SdpMLineIndex := aSdpMLineIndex;
    Sdp := aSdp;
  end;
end;

{**********************************************************************************************}
class function TALWebRTCPeerConnectionParameters.Create(const aVideoCallEnabled: boolean = true;
                                                        const aVideoWidth: integer = 1920;
                                                        const aVideoHeight: integer = 1080;
                                                        const aVideoFps: integer = 0;
                                                        const aVideoMaxBitrate: integer = 0;
                                                        const aVideoCodec: String = 'VP8';
                                                        const aVideoCodecHwAcceleration: boolean = true;
                                                        const aAudioStartBitrate: integer = 0;
                                                        const aAudioCodec: String = 'opus';
                                                        const aNoAudioProcessing: boolean = false;
                                                        const aAecDump: boolean = false;
                                                        const aDisableBuiltInAEC: boolean = false;
                                                        const aDisableBuiltInNS: boolean = false;
                                                        const aDataChannelEnabled: boolean = false;
                                                        const aDataChannelOrdered: boolean = true;
                                                        const aDataChannelMaxRetransmitTimeMs: integer = -1;
                                                        const aDataChannelMaxRetransmits: integer = -1;
                                                        const aDataChannelProtocol: String = '';
                                                        const aDataChannelNegotiated: boolean = false;
                                                        const aDataChannelId: integer = -1): TALWebRTCPeerConnectionParameters;
begin
  with result do begin
    VideoCallEnabled := aVideoCallEnabled;
    VideoWidth := aVideoWidth;
    VideoHeight := aVideoHeight;
    VideoFps := aVideoFps;
    VideoMaxBitrate := aVideoMaxBitrate;
    VideoCodec := aVideoCodec;
    VideoCodecHwAcceleration := aVideoCodecHwAcceleration;
    AudioStartBitrate := aAudioStartBitrate;
    AudioCodec := aAudioCodec;
    NoAudioProcessing := aNoAudioProcessing;
    AecDump := aAecDump;
    DisableBuiltInAEC := aDisableBuiltInAEC;
    DisableBuiltInNS := aDisableBuiltInNS;
    DataChannelEnabled := aDataChannelEnabled;
    DataChannelOrdered := aDataChannelOrdered;
    DataChannelMaxRetransmitTimeMs := aDataChannelMaxRetransmitTimeMs;
    DataChannelMaxRetransmits := aDataChannelMaxRetransmits;
    DataChannelProtocol := aDataChannelProtocol;
    DataChannelNegotiated := aDataChannelNegotiated;
    DataChannelId := aDataChannelId;
  end;
end;

{***************************************************************************************************************************************}
constructor TALWebRTC.Create(const aIceServers: TALWebRTCIceServers; const aPeerConnectionParameters: TALWebRTCPeerConnectionParameters);

{$REGION ' ANDROID'}
{$IF defined(android)}
var aJListIceServers: JList;
    aIceServerBuilder: JPeerConnection_IceServer_Builder;
    aJPeerConnectionParameters: JALWebRTC_PeerConnectionParameters;
    i: integer;
{$ENDIF}
{$ENDREGION}

{$REGION ' IOS'}
{$IF defined(ios)}

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _InitTexture(const aTexture: TTexture);
  var Tex: GLuint;
  begin
    aTexture.Style := aTexture.Style - [TTextureStyle.MipMaps];

    {$IF CompilerVersion >= 32} // tokyo
    if TALCustomContextIOSAccess.valid then
    {$ELSE}
    TALCustomContextIOSAccess.CreateSharedContext;
    if TALCustomContextIOSAccess.IsContextAvailable then
    {$ENDIF}

    begin
      glActiveTexture(GL_TEXTURE0);
      glGenTextures(1, @Tex);
      glBindTexture(GL_TEXTURE_2D, Tex);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
      case aTexture.MagFilter of
        TTextureFilter.Nearest: glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
        TTextureFilter.Linear: glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
      end;
      //if TTextureStyle.MipMaps in Texture.Style then begin
      //  case Texture.MinFilter of
      //    TTextureFilter.Nearest: glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST_MIPMAP_NEAREST);
      //    TTextureFilter.Linear: glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
      //  end;
      //end
      //else begin
        case aTexture.MinFilter of
          TTextureFilter.Nearest: glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
          TTextureFilter.Linear: glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
        end;
      //end;
      //glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, Texture.Width, Texture.Height, 0, GL_RGBA, GL_UNSIGNED_BYTE, nil);
      glBindTexture(GL_TEXTURE_2D, 0);
      ITextureAccess(aTexture).Handle := Tex;
      if (TALCustomContextIOSAccess.GLHasAnyErrors()) then
        RaiseContextExceptionFmt(@SCannotCreateTexture, [TALCustomContextIOSAccess.ClassName]);
    end;
  end;

{$ENDIF}
{$ENDREGION}

begin

  inherited Create;
  //-----
  fOnLocalFrameAvailableEvent := nil;
  fOnRemoteFrameAvailableEvent := nil;
  fonLocalDescriptionEvent := nil;
  fonIceCandidateEvent := nil;
  fonIceCandidatesRemovedEvent := nil;
  fonIceConnectionChangeEvent := nil;
  fonErrorEvent := nil;
  //-----
  FlocalBitmapRotation := 0;
  FRemoteBitmapRotation := 0;

  {$REGION ' ANDROID'}
  {$IF defined(android)}


    fLocalbitmap := TALTexture.Create;
    TALTexture(fLocalbitmap).isExternalOES := true;
    fLocalbitmap.Style := fLocalbitmap.Style - [TTextureStyle.MipMaps];
    //-----
    fRemotebitmap := TALTexture.Create;
    TALTexture(fRemotebitmap).isExternalOES := true;
    fRemotebitmap.Style := fRemotebitmap.Style - [TTextureStyle.MipMaps];
    //-----
    fAndroidWebRTC := nil;
    //-----
    aJListIceServers := TJlist.wrap(TJArrayList.JavaClass.init);
    for I := low(aIceServers) to High(aIceServers) do begin
      aIceServerBuilder := TJPeerConnection_IceServer.javaclass.builder(StringToJstring(aIceServers[i].uri));
      if aIceServers[i].username <> '' then aIceServerBuilder := aIceServerBuilder.setUsername(StringToJstring(aIceServers[i].username));
      if aIceServers[i].password <> '' then aIceServerBuilder := aIceServerBuilder.setPassword(StringToJstring(aIceServers[i].password));
      aJListIceServers.add(aIceServerBuilder.createIceServer);
      aIceServerBuilder := nil;
    end;
    //-----
    aJPeerConnectionParameters := TJALWebRTC_PeerConnectionParameters.JavaClass.init;
    aJPeerConnectionParameters.VideoCallEnabled := aPeerConnectionParameters.VideoCallEnabled;
    aJPeerConnectionParameters.VideoWidth := aPeerConnectionParameters.VideoWidth;
    aJPeerConnectionParameters.VideoHeight := aPeerConnectionParameters.VideoHeight;
    aJPeerConnectionParameters.VideoFps := aPeerConnectionParameters.VideoFps;
    aJPeerConnectionParameters.VideoMaxBitrate := aPeerConnectionParameters.VideoMaxBitrate;
    aJPeerConnectionParameters.VideoCodec := StringToJstring(aPeerConnectionParameters.VideoCodec);
    aJPeerConnectionParameters.VideoCodecHwAcceleration := aPeerConnectionParameters.VideoCodecHwAcceleration;
    aJPeerConnectionParameters.AudioStartBitrate := aPeerConnectionParameters.AudioStartBitrate;
    aJPeerConnectionParameters.AudioCodec := StringToJstring(aPeerConnectionParameters.AudioCodec);
    aJPeerConnectionParameters.NoAudioProcessing := aPeerConnectionParameters.NoAudioProcessing;
    aJPeerConnectionParameters.AecDump := aPeerConnectionParameters.AecDump;
    aJPeerConnectionParameters.DisableBuiltInAEC := aPeerConnectionParameters.DisableBuiltInAEC;
    aJPeerConnectionParameters.DisableBuiltInNS := aPeerConnectionParameters.DisableBuiltInNS;
    aJPeerConnectionParameters.DataChannelEnabled := aPeerConnectionParameters.DataChannelEnabled;
    aJPeerConnectionParameters.DataChannelOrdered := aPeerConnectionParameters.DataChannelOrdered;
    aJPeerConnectionParameters.DataChannelMaxRetransmitTimeMs := aPeerConnectionParameters.DataChannelMaxRetransmitTimeMs;
    aJPeerConnectionParameters.DataChannelMaxRetransmits := aPeerConnectionParameters.DataChannelMaxRetransmits;
    aJPeerConnectionParameters.DataChannelProtocol := StringToJstring(aPeerConnectionParameters.DataChannelProtocol);
    aJPeerConnectionParameters.DataChannelNegotiated := aPeerConnectionParameters.DataChannelNegotiated;
    aJPeerConnectionParameters.DataChannelId := aPeerConnectionParameters.DataChannelId;
    //-----
    fAndroidWebRTC := TJALWebRTC.Wrap(TJALWebRTC.JavaClass.init(TAndroidHelper.Context.getApplicationContext,
                                                                int64(TCustomAndroidContext(TContext3D.CurrentContext).SharedContext),
                                                                aJListIceServers,
                                                                aJPeerConnectionParameters));
    fAndroidWebRTCListener := TAndroidWebRTCListener.Create(Self);
    fAndroidWebRTC.setListener(fAndroidWebRTCListener);
    //-----
    aJPeerConnectionParameters := nil;
    aJListIceServers := nil;

  {$ENDIF}
  {$ENDREGION}

  {$REGION ' IOS'}
  {$IF defined(ios)}

    fLocalbitmap := TALBiPlanarTexture.Create(true{aVolatile});
    //-----
    fRemotebitmap := TALPlanarTexture.Create(true{aVolatile});
    _InitTexture(fRemotebitmap);
    _InitTexture(fRemotebitmap.SecondTexture);
    _InitTexture(fRemotebitmap.ThirdTexture);
    //-----
    fiOSWebRTC := TALiOSWebRTC.Create(self, aIceServers, aPeerConnectionParameters);

  {$ENDIF}
  {$ENDREGION}

end;

{***************************}
destructor TALWebRTC.Destroy;
begin

  {$REGION ' ANDROID'}
  {$IF defined(android)}

    Stop;
    //-----
    ALFreeAndNil(fLocalBitmap);
    ALFreeAndNil(fRemoteBitmap);

  {$ENDIF}
  {$ENDREGION}

  {$REGION ' IOS'}
  {$IF defined(ios)}

    Stop;
    //-----
    ITextureAccess(fLocalBitmap).Handle := 0;
    ITextureAccess(fLocalBitmap.secondTexture).Handle := 0;
    alfreeAndNil(fLocalbitmap);
    //-----
    alfreeAndNil(fRemotebitmap);

  {$ENDIF}
  {$ENDREGION}

  inherited Destroy;

end;


{********************************}
function TALWebRTC.Start: boolean;

{$REGION ' ANDROID'}
{$IF defined(android)}
var aNativeWin: JWindow;
{$ENDIF}
{$ENDREGION}

begin

  {$REGION ' ANDROID'}
  {$IF defined(android)}

    result := fAndroidWebRTC.start;
    if result then begin
      aNativeWin := TAndroidHelper.Activity.getWindow;
      if aNativeWin <> nil then aNativeWin.addFlags(TJWindowManager_LayoutParams.javaclass.FLAG_KEEP_SCREEN_ON);
    end

  {$ENDIF}
  {$ENDREGION}

  {$REGION ' IOS'}
  {$IF defined(ios)}

    result := fiOSWebRTC.start;
    if result then TiOSHelper.SharedApplication.setIdleTimerDisabled(true);

  {$ENDIF}
  {$ENDREGION}

  {$REGION ' MSWINDOWS / _MACOS'}
  {$IF defined(MSWINDOWS) or defined(_MACOS)}

    result := false;

  {$ENDIF}
  {$ENDREGION}

end;

{***********************}
procedure TALWebRTC.Stop;

{$REGION ' ANDROID'}
{$IF defined(android)}
var aNativeWin: JWindow;
{$ENDIF}
{$ENDREGION}

begin

  {$REGION ' ANDROID'}
  {$IF defined(android)}

    if (fAndroidWebRTC <> nil) then begin
      fAndroidWebRTC.setListener(nil);
      fAndroidWebRTC.Stop;
      fAndroidWebRTC := Nil;
    end;
    AlFreeAndNil(fAndroidWebRTCListener);
    aNativeWin := TAndroidHelper.Activity.getWindow;
    if aNativeWin <> nil then aNativeWin.clearFlags(TJWindowManager_LayoutParams.javaclass.FLAG_KEEP_SCREEN_ON);

  {$ENDIF}
  {$ENDREGION}

  {$REGION ' IOS'}
  {$IF defined(ios)}

    if fiOSWebRTC <> nil then begin
      fiOSWebRTC.stop; // will simply call terminate
      fiOSWebRTC := nil; // as fWebRTC have freeOnTerminate = True, it's will free up the memory it's use by himself
    end;
    TiOSHelper.SharedApplication.setIdleTimerDisabled(false);

  {$ENDIF}
  {$ENDREGION}

end;

{******************************}
procedure TALWebRTC.createOffer;
begin

  {$REGION ' ANDROID'}
  {$IF defined(android)}

    fAndroidWebRTC.createOffer;

  {$ENDIF}
  {$ENDREGION}

  {$REGION ' IOS'}
  {$IF defined(ios)}

    fiOSWebRTC.createOffer;

  {$ENDIF}
  {$ENDREGION}

end;

{*******************************}
procedure TALWebRTC.createAnswer;
begin

  {$REGION ' ANDROID'}
  {$IF defined(android)}

    fAndroidWebRTC.createAnswer;

  {$ENDIF}
  {$ENDREGION}

  {$REGION ' IOS'}
  {$IF defined(ios)}

    fiOSWebRTC.createAnswer;

  {$ENDIF}
  {$ENDREGION}

end;

{********************************************************************************************************}
procedure TALWebRTC.setRemoteDescription(const aSdpType: TALWebRTCSDPType; const aSdpDescription: String);
begin

  {$REGION ' ANDROID'}
  {$IF defined(android)}

    case aSdpType of
      TALWebRTCSDPType.OFFER:    fAndroidWebRTC.setRemoteDescription(TJSessionDescription_Type.JavaClass.OFFER,    StringToJstring(aSdpDescription));
      TALWebRTCSDPType.ANSWER:   fAndroidWebRTC.setRemoteDescription(TJSessionDescription_Type.JavaClass.ANSWER,   StringToJstring(aSdpDescription));
      TALWebRTCSDPType.PRANSWER: fAndroidWebRTC.setRemoteDescription(TJSessionDescription_Type.JavaClass.PRANSWER, StringToJstring(aSdpDescription));
    end;

  {$ENDIF}
  {$ENDREGION}

  {$REGION ' IOS'}
  {$IF defined(ios)}

    fiOSWebRTC.setRemoteDescription(aSdpType, aSdpDescription);

  {$ENDIF}
  {$ENDREGION}

end;

{************************************************************************************}
procedure TALWebRTC.addRemoteIceCandidate(const aIceCandidate: TALWebRTCIceCandidate);
begin

  {$REGION ' ANDROID'}
  {$IF defined(android)}

    fAndroidWebRTC.addRemoteIceCandidate(StringToJstring(aIceCandidate.SdpMid),
                                         aIceCandidate.sdpMLineIndex,
                                         StringToJstring(aIceCandidate.Sdp));

  {$ENDIF}
  {$ENDREGION}

  {$REGION ' IOS'}
  {$IF defined(ios)}

    fiOSWebRTC.addRemoteIceCandidate(aIceCandidate);

  {$ENDIF}
  {$ENDREGION}

end;

{*************************************************************************************************}
procedure TALWebRTC.removeRemoteIceCandidates(const aIceCandidates: Tarray<TALWebRTCIceCandidate>);

{$REGION ' ANDROID'}
{$IF defined(android)}
var aJIceCandidates: TJavaObjectArray<JIceCandidate>;
    i: integer;
{$ENDIF}
{$ENDREGION}

begin

  {$REGION ' ANDROID'}
  {$IF defined(android)}

    aJIceCandidates := TJavaObjectArray<JIceCandidate>.Create(length(aIceCandidates));
    try
      for I := 0 to length(aIceCandidates) - 1 do
        aJIceCandidates.Items[I] := TJIceCandidate.JavaClass.init(
                                      StringToJstring(aIceCandidates[i].SdpMid),
                                      aIceCandidates[i].sdpMLineIndex,
                                      StringToJstring(aIceCandidates[i].Sdp));
      fAndroidWebRtc.removeRemoteIceCandidates(aJIceCandidates);
    finally
      ALFreeAndNil(aJIceCandidates);
    end;

  {$ENDIF}
  {$ENDREGION}

  {$REGION ' IOS'}
  {$IF defined(ios)}

    fiOSWebRTC.removeRemoteIceCandidates(aIceCandidates);

  {$ENDIF}
  {$ENDREGION}

end;

{****************************************************************************************************************}
Procedure TALWebRTC.changeCaptureFormat(const aWidth: integer; const aHeight: integer; const aFramerate: integer);
begin

  {$REGION ' ANDROID'}
  {$IF defined(android)}

    fAndroidWebRTC.changeCaptureFormat(aWidth, aHeight, aframerate);

  {$ENDIF}
  {$ENDREGION}

  {$REGION ' IOS'}
  {$IF defined(ios)}

    fiOSWebRTC.changeCaptureFormat(aWidth, aHeight, aFramerate);

  {$ENDIF}
  {$ENDREGION}

end;

{*********************************************************}
Procedure TALWebRTC.setAudioEnabled(const enable: boolean);
begin

  {$REGION ' ANDROID'}
  {$IF defined(android)}

    fAndroidWebRTC.setAudioEnabled(enable);

  {$ENDIF}
  {$ENDREGION}

  {$REGION ' IOS'}
  {$IF defined(ios)}

    fiOSWebRTC.setAudioEnabled(enable);

  {$ENDIF}
  {$ENDREGION}

end;

{*********************************************************}
Procedure TALWebRTC.setVideoEnabled(const enable: boolean);
begin

  {$REGION ' ANDROID'}
  {$IF defined(android)}

    fAndroidWebRTC.setVideoEnabled(enable);


  {$ENDIF}
  {$ENDREGION}

  {$REGION ' IOS'}
  {$IF defined(ios)}

    fiOSWebRTC.setVideoEnabled(enable);

  {$ENDIF}
  {$ENDREGION}

end;

{****************************************************************************}
function TALWebRTC.setVideoMaxBitrate(const maxBitrateKbps: integer): boolean;
begin

  {$REGION ' ANDROID'}
  {$IF defined(android)}

    result := fAndroidWebRTC.setVideoMaxBitrate(maxBitrateKbps);


  {$ENDIF}
  {$ENDREGION}

  {$REGION ' IOS'}
  {$IF defined(ios)}

    result := fiOSWebRTC.setVideoMaxBitrate(maxBitrateKbps);

  {$ENDIF}
  {$ENDREGION}


  {$REGION ' MSWINDOWS / _MACOS'}
  {$IF defined(MSWINDOWS) or defined(_MACOS)}

    result := false;

  {$ENDIF}
  {$ENDREGION}

end;

{$REGION ' ANDROID'}
{$IF defined(android)}

{****************************************************************************}
constructor TALWebRTC.TAndroidWebRTCListener.Create(const aWebRTC: TALWebRTC);
begin
  inherited Create;
  fWebRTC := aWebRTC;
end;

{***************************************************************************************************************************************}
procedure TALWebRTC.TAndroidWebRTCListener.onLocalFrameAvailable(textureId: integer; width: integer; height: integer; rotation: integer);
begin

  {$IFDEF DEBUG}
  //allog('TALWebRTC.TAndroidWebRTCListener.onLocalFrameAvailable','textureId: ' + alinttostrU(textureId) +
  //                                                               ' - width: ' + alinttostrU(width) +
  //                                                               ' - height: ' + alinttostrU(height) +
  //                                                               ' - rotation: ' + alinttostrU(rotation) +
  //                                                               ' - ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.VERBOSE);
  {$ENDIF}

  TALTextureAccessPrivate(fWebRTC.fLocalBitmap).FHandle := textureId;
  TALTextureAccessPrivate(fWebRTC.fLocalBitmap).FWidth := width;
  TALTextureAccessPrivate(fWebRTC.fLocalBitmap).FHeight := height;
  fWebRTC.FlocalBitmapRotation := rotation;
  if assigned(fWebRTC.fOnLocalFrameAvailableEvent) then
    fWebRTC.fOnLocalFrameAvailableEvent(fWebRTC);

end;

{****************************************************************************************************************************************}
procedure TALWebRTC.TAndroidWebRTCListener.onRemoteFrameAvailable(textureId: integer; width: integer; height: integer; rotation: integer);
begin

  {$IFDEF DEBUG}
  //allog('TALWebRTC.TAndroidWebRTCListener.onRemoteFrameAvailable','textureId: ' + alinttostrU(textureId) +
  //                                                                ' - width: ' + alinttostrU(width) +
  //                                                                ' - height: ' + alinttostrU(height) +
  //                                                                ' - rotation: ' + alinttostrU(rotation) +
  //                                                                ' - ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.VERBOSE);
  {$ENDIF}

  TALTextureAccessPrivate(fWebRTC.fRemoteBitmap).FHandle := textureId;
  TALTextureAccessPrivate(fWebRTC.fRemoteBitmap).FWidth := width;
  TALTextureAccessPrivate(fWebRTC.fRemoteBitmap).FHeight := height;
  fWebRTC.FRemoteBitmapRotation := rotation;
  if assigned(fWebRTC.fOnRemoteFrameAvailableEvent) then
    fWebRTC.fOnRemoteFrameAvailableEvent(fWebRTC);

end;

{**************************************************************************************}
procedure TALWebRTC.TAndroidWebRTCListener.onLocalDescription(sdp: JSessionDescription);
var aSDPType: TALWebRTCSDPType;
begin

  {$IFDEF DEBUG}
  allog('TALWebRTC.TAndroidWebRTCListener.onLocalDescription','sdp.description: ' + JstringToString(sdp.description) +
                                                              ' - ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.VERBOSE);
  {$ENDIF}

  if assigned(fWebRTC.fonLocalDescriptionEvent) then begin
    if sdp.&type.equals(TJSessionDescription_Type.JavaClass.OFFER) then aSDPType := TALWebRTCSDPType.OFFER
    else if sdp.&type.equals(TJSessionDescription_Type.JavaClass.ANSWER) then aSDPType := TALWebRTCSDPType.ANSWER
    else if sdp.&type.equals(TJSessionDescription_Type.JavaClass.PRANSWER) then aSDPType := TALWebRTCSDPType.PRANSWER
    else raise Exception.createFmt('Unknown Description Type (%s)', [JstringToString(sdp.&type.toString)]);
    fWebRTC.fonLocalDescriptionEvent(fWebRTC, aSDPType, JstringToString(sdp.description));
  end;

end;

{**********************************************************************************}
procedure TALWebRTC.TAndroidWebRTCListener.onIceCandidate(candidate: JIceCandidate);
begin

  {$IFDEF DEBUG}
  allog('TALWebRTC.TAndroidWebRTCListener.onIceCandidate','candidate.sdpMLineIndex: ' + ALinttostrU(candidate.sdpMLineIndex) +
                                                          ' - candidate.sdpMid: ' + JstringToString(candidate.sdpMid) +
                                                          ' - candidate.serverUrl: ' + JstringToString(candidate.serverUrl) +
                                                          ' - candidate.sdp: ' + JstringToString(candidate.sdp) +
                                                          ' - ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.VERBOSE);
  {$ENDIF}

  if assigned(fWebRTC.fonIceCandidateEvent) then
    fWebRTC.fonIceCandidateEvent(fWebRTC,
                                 TALWebRTCIceCandidate.Create(
                                   JstringToString(candidate.sdpMid),
                                   candidate.sdpMLineIndex,
                                   JstringToString(candidate.sdp)));

end;

{*************************************************************************************************************}
procedure TALWebRTC.TAndroidWebRTCListener.onIceCandidatesRemoved(candidates: TJavaObjectArray<JIceCandidate>);
var aIceCandidates: TALWebRTCIceCandidates;
    i: integer;
begin

  {$IFDEF DEBUG}
  allog('TALWebRTC.TAndroidWebRTCListener.onIceCandidatesRemoved','ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.VERBOSE);
  {$ENDIF}

  if assigned(fWebRTC.fonIceCandidatesRemovedEvent) then begin
    setlength(aIceCandidates, candidates.Length);
    for I := 0 to candidates.Length - 1 do begin
      aIceCandidates[i] := TALWebRTCIceCandidate.Create(
                             JstringToString(candidates.Items[i].sdpMid),
                             candidates.Items[i].sdpMLineIndex,
                             JstringToString(candidates.Items[i].sdp));
    end;
    fWebRTC.fonIceCandidatesRemovedEvent(fWebRTC, aIceCandidates);
  end;

end;

{*************************************************************************************************************}
procedure TALWebRTC.TAndroidWebRTCListener.onIceConnectionChange(newState: JPeerConnection_IceConnectionState);
var aState: TALWebRTCIceConnectionState;
begin

  {$IFDEF DEBUG}
  allog('TALWebRTC.TAndroidWebRTCListener.onIceConnectionChange','newState: ' + JstringToString(newState.toString) +
                                                                 ' - ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.VERBOSE);
  {$ENDIF}

  if assigned(fWebRTC.fonIceConnectionChangeEvent) then begin
    if newState.equals(tJPeerConnection_IceConnectionState.JavaClass.CONNECTED) then aState := TALWebRTCIceConnectionState.CONNECTED
    else if newState.equals(tJPeerConnection_IceConnectionState.JavaClass.DISCONNECTED) then aState := TALWebRTCIceConnectionState.DISCONNECTED
    else if newState.equals(tJPeerConnection_IceConnectionState.JavaClass.FAILED) then aState := TALWebRTCIceConnectionState.FAILED
    else if newState.equals(tJPeerConnection_IceConnectionState.JavaClass.CLOSED) then aState := TALWebRTCIceConnectionState.CLOSED
    else if newState.equals(tJPeerConnection_IceConnectionState.JavaClass.COMPLETED) then aState := TALWebRTCIceConnectionState.COMPLETED
    else if newState.equals(tJPeerConnection_IceConnectionState.JavaClass.CHECKING) then aState := TALWebRTCIceConnectionState.CHECKING
    else if newState.equals(tJPeerConnection_IceConnectionState.JavaClass.NEW) then aState := TALWebRTCIceConnectionState.NEW
    else raise Exception.createFmt('Unknown Ice Connection State (%s)', [JstringToString(newState.toString)]);
    fWebRTC.fonIceConnectionChangeEvent(fWebRTC,aState);
  end;

end;

{**************************************************************************************}
procedure TALWebRTC.TAndroidWebRTCListener.onError(code: Integer; description: JString);
begin

  {$IFDEF DEBUG}
  allog('TALWebRTC.TAndroidWebRTCListener.onError','code: ' + ALinttoStrU(code) +
                                                   ' - description: ' + JstringToString(description) +
                                                   ' - ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.error);
  {$ENDIF}

  if assigned(fWebRTC.fOnErrorEvent) then
    fWebRTC.fOnErrorEvent(fWebRTC, code, JstringToString(description));

end;

{******************************************}
function TALWebRTC.getLocalBitmap: TTexture;
begin
  if fLocalBitmap.Handle = 0 then result := nil
  else result := fLocalBitmap;
end;

{*******************************************}
function TALWebRTC.getRemoteBitmap: TTexture;
begin
  if fRemoteBitmap.Handle = 0 then result := nil
  else result := fRemoteBitmap;
end;

{$ENDIF}
{$ENDREGION}

{$REGION ' IOS'}
{$IF defined(ios)}

{********************************************************************************************************************************************************************}
constructor TALiOSWebRTC.Create(const aWebRTC: TALWebRTC; const aIceServers: TALWebRTCIceServers; const aPeerConnectionParameters: TALWebRTCPeerConnectionParameters);
begin

  {$IFDEF DEBUG}
  allog('TALiOSWebRTC.Create', 'TALiOSWebRTC.Create - ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.verbose);
  {$ENDIF}

  FreeOnTerminate := True;
  fWebRTC := aWebRTC;
  //-----
  FSignal := TEvent.Create(nil, false{ManualReset}, false, '');
  fQueue := TQueue<TProc>.create;
  //-----
  fLocalVideoTextureRefLuma := 0;
  fLocalVideoTextureRefChroma := 0;
  fRemoteVideoTextureRefLuma := 0;
  fRemoteVideoTextureRefChroma := 0;
  if CVOpenGLESTextureCacheCreate(kCFAllocatorDefault, // allocator: The CFAllocatorRef to use for allocating the texture cache. This parameter can be NULL.
                                  nil, // cacheAttributes: A CFDictionaryRef containing the attributes of the texture cache itself. This parameter can be NULL.
                                  (TCustomContextIOS.SharedContext as ILocalObject).GetObjectID, // eaglContext: The OpenGLES 2.0 context into which the texture objects will be created. OpenGLES 1.x contexts are not supported.
                                  nil, // textureAttributes: A CFDictionaryRef containing the attributes to be used for creating the CVOpenGLESTextureRef objects. This parameter can be NULL.
                                  @fvideoTextureCacheRef) <> kCVReturnSuccess then raise Exception.Create('CVOpenGLESTextureCacheCreate failed!'); // cacheOut: A pointer to a CVOpenGLESTextureCacheRef where the newly created texture cache will be placed.
  //-----
  fIceServers := aIceServers;
  fPeerConnectionParameters := aPeerConnectionParameters;
  fPeerConnectionFactory := nil;
  fPeerConnection := nil;
  fPeerConnectionDelegate := nil;
  fCameraVideoCapturer := nil;
  fVideoSource := nil;
  fLocalAudioTrack := nil;
  fLocalVideoTrack := nil;
  fLocalVideoTrackRenderer := nil;
  fRemoteVideoTrack := nil;
  fRemoteVideoTrackRenderer := nil;
  fLocalSdp := nil;
  fQueuedRemoteCandidates := nil;
  fIsInitiator := False;
  //-----
  inherited Create(False); // see http://www.gerixsoft.com/blog/delphi/fixing-symbol-resume-deprecated-warning-delphi-2010

end;

{******************************}
destructor TALiOSWebRTC.Destroy;
begin

  {$IFDEF DEBUG}
  allog('TALiOSWebRTC.Destroy', 'TALiOSWebRTC.Destroy - ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.verbose);
  {$ENDIF}

  if not terminated then begin
    Terminate; // << will fire fSignal.SetEvent in TerminatedSet
    WaitFor;
  end;
  //-----
  ALfreeandNil(FSignal);
  ALFreeAndNil(fQueue);
  //-----
  TThread.Synchronize(nil,
  Procedure
  Begin
    if fLocalVideoTextureRefLuma <> 0 then CFRelease(pointer(fLocalVideoTextureRefLuma));
    if fLocalVideoTextureRefChroma <> 0 then CFRelease(pointer(fLocalVideoTextureRefChroma));
    if fRemoteVideoTextureRefLuma <> 0 then CFRelease(pointer(fRemoteVideoTextureRefLuma));
    if fRemoteVideoTextureRefChroma <> 0 then CFRelease(pointer(fRemoteVideoTextureRefChroma));
    //-----
    CVOpenGLESTextureCacheFlush(fvideoTextureCacheRef, 0);  // The texture cache automatically flushes currently unused resources when you call the
                                                            // CVOpenGLESTextureCacheCreateTextureFromImage function, but can you can also flush the
                                                            // cache explicitly by calling this function. The EAGLContext associated with the cache
                                                            // may be used to delete or unbind textures.
    CFrelease(pointer(fvideoTextureCacheRef));
  end);
  //-----
  inherited;

end;

{*****************************}
procedure TALiOSWebRTC.Execute;
var aProc: TProc;
begin

  //loop still not terminated
  while not Terminated do begin
    Try

      Tmonitor.Enter(fQueue);
      try
        if (fQueue.Count > 0) then aProc := fQueue.Dequeue()
        else aProc := nil;
      finally
        Tmonitor.exit(fQueue);
      end;
      if assigned(aProc) then begin
        aProc();
        aProc := nil;
      end
      else fSignal.WaitFor;

    Except
      on E: Exception do begin
        {$IFDEF DEBUG}
        allog('TALiOSWebRTC.Execute.Error','code: 0' +
                                           ' - description: ' + e.Message +
                                           ' - ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.error);
        {$ENDIF}
        TThread.Synchronize(nil,
          procedure
          begin
            if Terminated then exit;
            if assigned(fWebRTC.fOnErrorEvent) then
              fWebRTC.fOnErrorEvent(self, 0, e.Message);
          end);
      end;
    End;
  end;

  //release all retained object
  //-----
  if fpeerConnection <> nil then begin
    fpeerConnection.setdelegate(nil); // << remove the fPeerConnectionDelegate
    fpeerConnection.close; // << Terminate all media and close the transport.
  end;
  //-----
  if fCameraVideoCapturer <> nil then begin
    fCameraVideoCapturer.stopCapture;
    fCameraVideoCapturer.release; // << was created - taken from TRTCCameraVideoCapturer.OCClass.alloc.initWithDelegate(fVideoSource)
    fCameraVideoCapturer := nil;
  end;
  //-----
  if fLocalAudioTrack <> nil then begin
    fLocalAudioTrack.release; // << was retained - taken from fPeerConnectionFactory.audioTrackWithSource(aAudioSource, StrToNSStr(kARDAudioTrackId))
    fLocalAudioTrack := nil;
  end;
  //-----
  if fLocalVideoTrackRenderer <> nil then begin
    if fLocalVideoTrack <> nil then fLocalVideoTrack.removeRenderer(fLocalVideoTrackRenderer.GetObjectID);
    ALFreeAndNil(fLocalVideoTrackRenderer); // was created
  end;
  //-----
  if fLocalVideoTrack <> nil then begin
    fLocalVideoTrack.release; // was retained - taken from fpeerConnectionFactory.videoTrackWithSource(fVideoSource, StrToNSStr(kARDVideoTrackId))
    fLocalVideoTrack := nil;
  end;
  //-----
  if fVideoSource <> nil then begin
    fVideoSource.release; // was retained - taken from fPeerConnectionFactory.videoSource
    fVideoSource := nil;
  end;
  //-----
  if fRemoteVideoTrackRenderer <> nil then begin
    if fRemoteVideoTrack <> nil then fRemoteVideoTrack.removeRenderer(fRemoteVideoTrackRenderer.GetObjectID);
    ALFreeAndNil(fRemoteVideoTrackRenderer); // < was created
  end;
  //-----
  if fRemoteVideoTrack <> nil then begin
    fRemoteVideoTrack.release; // << was retained - taken from fpeerConnection.transceivers[x].receiver.track
    fRemoteVideoTrack := nil;
  end;
  //-----
  if fpeerConnection <> nil then begin
    fpeerConnection.release; // << was retained - taken from fPeerConnectionFactory.peerConnectionWithConfiguration(aConfiguration, aMediaConstraints, fPeerConnectionDelegate.GetObjectID)
    fpeerConnection := nil;
  end;
  //-----
  ALFreeAndNil(fPeerConnectionDelegate); // << was created
  //-----
  if fLocalSdp <> nil then begin
    fLocalSdp.release; // << was retained - taken from CreateSessionDescriptionCompletionHandler(sdp: RTCSessionDescription; error: NSError)
    fLocalSdp := nil;
  end;
  //-----
  if fPeerConnectionFactory <> nil then begin
    fPeerConnectionFactory.release; // << was created - taken from TRTCPeerConnectionFactory.OCClass.alloc.initWithEncoderFactory(aEncoderFactory, aDecoderFactory)
    fPeerConnectionFactory := nil;
  end;
  //-----
  ALFreeAndNil(fQueuedRemoteCandidates);

end;

{***********************************}
procedure TALiOSWebRTC.TerminatedSet;
begin
  fSignal.SetEvent;
  inherited;
end;

{*************************************************}
procedure TALiOSWebRTC.Enqueue(const aProc: Tproc);
begin
  Tmonitor.Enter(fQueue);
  try
    fQueue.Enqueue(aProc);
  finally
    Tmonitor.Exit(fQueue);
  end;
  fSignal.SetEvent;
end;

{***********************************}
function TALiOSWebRTC.start: boolean;
begin

  Enqueue(
    Procedure
    var aDecoderFactory: RTCDefaultVideoDecoderFactory;
        aEncoderFactory: RTCDefaultVideoEncoderFactory;
        aVideoCodecInfo: RTCVideoCodecInfo;
        aH264ProfileLevelId: RTCH264ProfileLevelId;
        aSupportedCodecs: NSArray;
        aVideoCodecName: String;
        aConfiguration: RTCConfiguration;
        aMediaConstraints: RTCMediaConstraints;
        aIceServers: array of Pointer;
        aAudioSource: RTCAudioSource;
        aCaptureDevice: AVCaptureDevice;
        aCaptureDeviceFormat: AVCaptureDeviceFormat;
        aCertificate: RTCCertificate;
        aCertificateParams: NSMutableDictionary;
        aOptionalConstraints: NSMutableDictionary;
        aTransceiver: RTCRtpTransceiver;
        i: integer;
    begin

      //init fPeerConnectionFactory
      aDecoderFactory := TRTCDefaultVideoDecoderFactory.Wrap(TRTCDefaultVideoDecoderFactory.Wrap(TRTCDefaultVideoDecoderFactory.OCClass.alloc).init);
      aEncoderFactory := TRTCDefaultVideoEncoderFactory.Wrap(TRTCDefaultVideoEncoderFactory.Wrap(TRTCDefaultVideoEncoderFactory.OCClass.alloc).init);
      try

        aVideoCodecInfo := nil;
        aSupportedCodecs := TRTCDefaultVideoEncoderFactory.OCClass.supportedCodecs;
        for I := aSupportedCodecs.count - 1 downto 0 do begin
          aVideoCodecInfo := TRTCVideoCodecInfo.Wrap(TRTCDefaultVideoEncoderFactory.OCClass.supportedCodecs.objectAtIndex(i));
          aVideoCodecName := NSStrToStr(aVideoCodecInfo.name);
          if ALSameTextU(aVideoCodecName, 'H264') then begin
            aH264ProfileLevelId := TRTCH264ProfileLevelId.wrap(
                                     TRTCH264ProfileLevelId.wrap(
                                       TRTCH264ProfileLevelId.OCClass.alloc).
                                         initWithHexString(
                                           TNSString.Wrap(aVideoCodecInfo.parameters.objectForKey((StrToNsStr('profile-level-id') as IlocalObject).GetObjectID))));
            try
              if (aH264ProfileLevelId.profile = RTCH264ProfileConstrainedHigh) or
                 (aH264ProfileLevelId.profile = RTCH264ProfileHigh) then aVideoCodecName := 'H264 High'
              else if (aH264ProfileLevelId.profile = RTCH264ProfileConstrainedBaseline) or
                      (aH264ProfileLevelId.profile = RTCH264ProfileBaseline) then aVideoCodecName := 'H264 Baseline';
            finally
              aH264ProfileLevelId.release;
            end;
          end;
          if ALSameTextU(aVideoCodecName, fPeerConnectionParameters.videoCodec) then break;
        end;
        if aVideoCodecInfo <> nil then aEncoderFactory.setPreferredCodec(aVideoCodecInfo);
        //-----
        fPeerConnectionFactory := TRTCPeerConnectionFactory.Wrap(
                                    TRTCPeerConnectionFactory.Wrap(
                                      TRTCPeerConnectionFactory.OCClass.alloc).
                                        initWithEncoderFactory(
                                          (aEncoderFactory as ILocalObject).GetObjectID,
                                          (aDecoderFactory as ILocalObject).GetObjectID));

      finally
        aDecoderFactory.release;
        aEncoderFactory.release;
      end;

      //-----
      fQueuedRemoteCandidates := TList<TALWebRTCIceCandidate>.create;

      //-----
      setlength(aIceServers, length(fIceServers));
      for I := low(aIceServers) to High(aIceServers) do begin
        if (fIceServers[i].username = '') and
           (fIceServers[i].password = '') then aIceServers[i] := TRTCIceServer.Wrap(
                                                                   TRTCIceServer.OCClass.alloc).
                                                                     initWithURLStrings(
                                                                       TNSArray.Wrap(TNSArray.OCClass.arrayWithObject((StrToNSStr(fIceServers[i].uri) as ILocalObject).GetObjectID))) // urlStrings: NSArray)
        else aIceServers[i] := TRTCIceServer.Wrap(
                                 TRTCIceServer.OCClass.alloc).
                                    initWithURLStringsUsernameCredential(
                                      TNSArray.Wrap(TNSArray.OCClass.arrayWithObject((StrToNSStr(fIceServers[i].uri) as ILocalObject).GetObjectID)), // urlStrings: NSArray;
                                      StrToNSStr(fIceServers[i].username), // username: NSString;
                                      StrToNSStr(fIceServers[i].password)); // credential: NSString)
      end;

      try

        aConfiguration := TRTCConfiguration.Wrap(TRTCConfiguration.Wrap(TRTCConfiguration.OCClass.alloc).init);
        try

          aConfiguration.setIceServers(TNSArray.Wrap(TNSArray.OCClass.arrayWithObjects(@aIceServers[0], Length(aIceServers))));
          //-----
          aCertificateParams := TNSMutableDictionary.Create;
          try
            aCertificateParams.setValue((StrToNSStr('RSASSA-PKCS1-v1_5') as ILocalObject).GetObjectID, StrToNSStr('name'));
            aCertificateParams.setValue(TNSNumber.OCClass.numberWithInt(100000), StrToNSStr('expires'));
            aCertificate := TRTCCertificate.oCClass.generateCertificateWithParams(aCertificateParams); // << we don't need to call release (https://stackoverflow.com/questions/52314334/ios-objective-c-object-when-to-use-release-and-when-to-not-use-it)
            aConfiguration.setCertificate(aCertificate);
          finally
            aCertificateParams.release;
          end;
          //-----
          aConfiguration.setsdpSemantics(RTCSdpSemanticsUnifiedPlan);
          //-----
          aOptionalConstraints := TNSMutableDictionary.Create;
          try
            aOptionalConstraints.setValue((StrToNSStr('true') as ILocalObject).GetObjectID, StrToNSStr('DtlsSrtpKeyAgreement'));
            aMediaConstraints := TRTCMediaConstraints.Wrap(
                                   TRTCMediaConstraints.Wrap(
                                     TRTCMediaConstraints.OCClass.alloc).
                                       initWithMandatoryConstraints(nil, //mandatory: NSDictionary;
                                                                    aOptionalConstraints)); //optionalConstraints: NSDictionary
            try

              fPeerConnectionDelegate := TPeerConnectionDelegate.Create(self);
              fpeerConnection := fPeerConnectionFactory.peerConnectionWithConfiguration(aConfiguration, // configuration: RTCConfiguration;
                                                                                        aMediaConstraints, // constraints: RTCMediaConstraints;
                                                                                        fPeerConnectionDelegate.GetObjectID); //delegate: Pointer);
              fpeerConnection.retain;

            finally
              aMediaConstraints.release;
            end;
          finally
            aOptionalConstraints.release;
          end;

        finally
          aConfiguration.release;
        end;

      finally
        for I := Low(aIceServers) to High(aIceServers) do
          TRTCIceServer.Wrap(aIceServers[i]).release;
      end;

      //-----
      aMediaConstraints := TRTCMediaConstraints.Wrap(
                             TRTCMediaConstraints.Wrap(
                               TRTCMediaConstraints.OCClass.alloc).
                                 initWithMandatoryConstraints(nil, //mandatory: NSDictionary;
                                                              nil)); //optionalConstraints: NSDictionary
      try

        aAudioSource := fPeerConnectionFactory.audioSourceWithConstraints(aMediaConstraints); // << we don't need to call release (https://stackoverflow.com/questions/52314334/ios-objective-c-object-when-to-use-release-and-when-to-not-use-it)
        fLocalAudioTrack := fPeerConnectionFactory.audioTrackWithSource(aAudioSource, StrToNSStr(kARDAudioTrackId)); // << we don't need to call release (https://stackoverflow.com/questions/52314334/ios-objective-c-object-when-to-use-release-and-when-to-not-use-it)
        fLocalAudioTrack.retain;
        fpeerConnection.addTrack(fLocalAudioTrack, TNSArray.Wrap(TNSArray.OCClass.arrayWithObject((StrToNSStr(kARDMediaStreamId) as ILocalObject).GetObjectID)));

        if (fPeerConnectionParameters.videoCallEnabled) then begin

          fVideoSource := fPeerConnectionFactory.videoSource;
          fVideoSource.retain;
          fCameraVideoCapturer := TRTCCameraVideoCapturer.Wrap(
                                    TRTCCameraVideoCapturer.Wrap(
                                      TRTCCameraVideoCapturer.OCClass.alloc).
                                        initWithDelegate(
                                          (fVideoSource as ILocalObject).GetObjectID));

          fLocalVideoTrack := fpeerConnectionFactory.videoTrackWithSource(fVideoSource, StrToNSStr(kARDVideoTrackId));
          fLocalVideoTrack.retain; // << we need to call retain as we didn't create it ourself (https://stackoverflow.com/questions/52314334/ios-objective-c-object-when-to-use-release-and-when-to-not-use-it)
          fpeerConnection.addTrack(fLocalVideoTrack, TNSArray.Wrap(TNSArray.OCClass.arrayWithObject((StrToNSStr(kARDMediaStreamId) as ILocalObject).GetObjectID)));

          fLocalVideoTrackRenderer := TLocalVideoTrackRenderer.create(self);
          fLocalVideoTrack.addRenderer(fLocalVideoTrackRenderer.GetObjectID);

          aCaptureDevice := findDeviceForPosition(AVCaptureDevicePositionFront);
          aCaptureDeviceFormat := selectFormatForDevice(aCaptureDevice, fCameraVideoCapturer.PreferredOutputPixelFormat);
          if aCaptureDeviceFormat = nil then raise Exception.Create('No valid formats for device');
          fCameraVideoCapturer.startCaptureWithDeviceFormatFps(aCaptureDevice, // device: AVCaptureDevice;
                                                               aCaptureDeviceFormat, // format: AVCaptureDeviceFormat;
                                                               selectFpsForFormat(aCaptureDeviceFormat)); // fps: NSInteger)

          // We can set up rendering for the remote track right away since the transceiver already has an
          // RTCRtpReceiver with a track. The track will automatically get unmuted and produce frames
          // once RTP is received.
          aTransceiver := nil;
          for I := 0 to fpeerConnection.transceivers.count - 1 do begin
            aTransceiver := tRTCRtpTransceiver.Wrap(fpeerConnection.transceivers.objectAtIndex(i));
            if aTransceiver.mediaType <> RTCRtpMediaTypeVideo then aTransceiver := nil
            else break;
          end;
          if aTransceiver = nil then raise Exception.Create('No remote video track founded');
          fRemoteVideoTrack := TRTCVideoTrack.Wrap((aTransceiver.receiver.track as ILocalObject).GetObjectID);
          fRemoteVideoTrack.retain;
          fRemoteVideoTrackRenderer := TRemoteVideoTrackRenderer.create(self);
          fRemoteVideoTrack.addRenderer(fRemoteVideoTrackRenderer.GetObjectID);

        end;

      finally
        aMediaConstraints.release;
      end;

  end);

  //-----
  result := true;

end;

{**************************}
procedure TALiOSWebRTC.Stop;
begin
  Terminate;
end;

{*********************************}
procedure TALiOSWebRTC.createOffer;
begin

  Enqueue(
    Procedure
    var aMediaConstraints: RTCMediaConstraints;
    begin

      //-----
      fIsInitiator := true;

      //-----
      aMediaConstraints := createSdpMediaConstraints;
      try
        fpeerConnection.offerForConstraints(aMediaConstraints, CreateSessionDescriptionCompletionHandler);
      finally
        aMediaConstraints.release;
      end;

    end);

end;

{**********************************}
procedure TALiOSWebRTC.createAnswer;
begin

  Enqueue(
    Procedure
    var aMediaConstraints: RTCMediaConstraints;
    begin

      //-----
      fIsInitiator := false;

      //-----
      aMediaConstraints := createSdpMediaConstraints;
      try
        fpeerConnection.answerForConstraints(aMediaConstraints, CreateSessionDescriptionCompletionHandler);
      finally
        aMediaConstraints.release;
      end;

    end);

end;

{***********************************************************************************************************}
procedure TALiOSWebRTC.setRemoteDescription(const aSdpType: TALWebRTCSDPType; const aSdpDescription: String);
begin

  Enqueue(
    Procedure
    var aSdp: RTCSessionDescription;
    begin
      case aSdpType of
        TALWebRTCSDPType.OFFER:    aSdp := TRTCSessionDescription.Wrap(TRTCSessionDescription.Wrap(TRTCSessionDescription.OCClass.alloc).initWithType(RTCSdpTypeOFFER,    StrToNsStr(aSdpDescription)));
        TALWebRTCSDPType.ANSWER:   aSdp := TRTCSessionDescription.Wrap(TRTCSessionDescription.Wrap(TRTCSessionDescription.OCClass.alloc).initWithType(RTCSdpTypeANSWER,   StrToNsStr(aSdpDescription)));
        TALWebRTCSDPType.PRANSWER: aSdp := TRTCSessionDescription.Wrap(TRTCSessionDescription.Wrap(TRTCSessionDescription.OCClass.alloc).initWithType(RTCSdpTypePRANSWER, StrToNsStr(aSdpDescription)));
      end;
      fPeerConnection.setRemoteDescription(aSdp, SetSessionDescriptionCompletionHandler);
    end);

end;

{****************************************************************************************************************************************************************************}
procedure TALiOSWebRTC.addRemoteIceCandidate({$IF not defined(CPUARM)}const aTmpIceCandidate: TALWebRTCIceCandidate{$ELSE}const aIceCandidate: TALWebRTCIceCandidate{$ENDIF});
{$IF not defined(CPUARM)}
var aIceCandidate: TALWebRTCIceCandidate;
{$ENDIF}
begin

  {$IF not defined(CPUARM)}
  aIceCandidate := aTmpIceCandidate ; // << because else on iOS simulator: [DCC Error] ALWebRTC.pas(1651): E2555 Cannot capture symbol 'aIceCandidate'
  {$ENDIF}

  Enqueue(
    Procedure
    var aRTCIceCandidate: RTCIceCandidate;
    begin

      if fQueuedRemoteCandidates <> nil then fQueuedRemoteCandidates.Add(aIceCandidate)
      else begin
        aRTCIceCandidate := TRTCIceCandidate.wrap(
                              TRTCIceCandidate.wrap(
                                TRTCIceCandidate.OCClass.alloc).
                                  initWithSdp(
                                    StrToNsStr(aIceCandidate.Sdp),// sdp: NSString;
                                    aIceCandidate.SdpMLineIndex, // sdpMLineIndex: Integer;
                                    StrToNsStr(aIceCandidate.SdpMid))); // sdpMid: NSString)
        try
          fPeerConnection.addIceCandidate(aRTCIceCandidate);
        finally
          aRTCIceCandidate.release;
        end;
      end;

    end);

end;

{****************************************************************************************************}
procedure TALiOSWebRTC.removeRemoteIceCandidates(const aIceCandidates: Tarray<TALWebRTCIceCandidate>);
begin

  Enqueue(
    Procedure
    var aRTCIceCandidates: array of Pointer;
        i: integer;
    begin

      // Drain the queued remote candidates if there is any so that
      // they are processed in the proper order.
      drainCandidates();

      //-----
      setlength(aRTCIceCandidates, length(aIceCandidates));
      for I := low(aRTCIceCandidates) to High(aRTCIceCandidates) do
        aRTCIceCandidates[i] := TRTCIceCandidate.wrap(
                                  TRTCIceCandidate.OCClass.alloc).
                                    initWithSdp(
                                      StrToNsStr(aIceCandidates[i].Sdp),// sdp: NSString;
                                      aIceCandidates[i].SdpMLineIndex, // sdpMLineIndex: Integer;
                                      StrToNsStr(aIceCandidates[i].SdpMid)); // sdpMid: NSString)
      try
        fPeerConnection.removeIceCandidates(TNSArray.Wrap(TNSArray.OCClass.arrayWithObjects(@aRTCIceCandidates[0], Length(aRTCIceCandidates))));
      finally
        for I := Low(aRTCIceCandidates) to High(aRTCIceCandidates) do
          TRTCIceCandidate.Wrap(aRTCIceCandidates[i]).release;
      end;

    end);

end;

{*******************************************************************************************************************}
Procedure TALiOSWebRTC.changeCaptureFormat(const aWidth: integer; const aHeight: integer; const aFramerate: integer);
begin

  Enqueue(
    Procedure
    begin

      fVideoSource.adaptOutputFormatToWidth(aWidth, aHeight, aFramerate);

    end);

end;

{************************************************************}
Procedure TALiOSWebRTC.setAudioEnabled(const enable: boolean);
begin

  Enqueue(
    Procedure
    begin

      if (fLocalAudioTrack <> nil) then fLocalAudioTrack.setIsEnabled(enable);

    end);

end;

{************************************************************}
Procedure TALiOSWebRTC.setVideoEnabled(const enable: boolean);
begin

  Enqueue(
    Procedure
    begin

      if (fLocalVideoTrack <> nil) then fLocalVideoTrack.setIsEnabled(enable);
      if (fRemoteVideoTrack <> nil) then fRemoteVideoTrack.setIsEnabled(enable);

    end);

end;

{*******************************************************************************}
function TALiOSWebRTC.setVideoMaxBitrate(const maxBitrateKbps: integer): boolean;
begin

  Enqueue(
    Procedure
    var aSender: RTCRtpSender;
        aParametersToModify: RTCRtpParameters;
        i,j: integer;
    begin

      for i := 0 to fPeerConnection.senders.count - 1 do begin
        aSender := TRTCRtpSender.Wrap(fPeerConnection.senders.objectAtIndex(i));
        if (aSender.track <> nil) and (NSStrToStr(aSender.track.kind) = kARDVideoTrackKind) then begin
          aParametersToModify := aSender.parameters;
          for J := 0 to aParametersToModify.encodings.count - 1 do begin
            TRTCRtpEncodingParameters.wrap(
              aParametersToModify.encodings.objectAtIndex(j))
                .setMaxBitrateBps(TNSNumber.Wrap(TNSNumber.OCClass.numberWithInt(maxBitrateKbps * 1000)));
          end;
        end;
      end;

    end);

    result := true;

end;

{***********************************************************************************************************}
procedure TALiOSWebRTC.CreateSessionDescriptionCompletionHandler(sdp: RTCSessionDescription; error: NSError);
var aErrorStr: String;
begin

  {$IFDEF DEBUG}
  if (error <> nil) then allog('TALWebRTC.CreateSessionDescriptionCompletionHandler', 'Failed to create session description. Error: ' + NSStrToStr(error.localizedDescription) +
                                                                                      ' - ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.error)
  else allog('TALWebRTC.CreateSessionDescriptionCompletionHandler', 'Session description successfully created' +
                                                                    ' - ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.verbose);
  {$ENDIF}

  //check if error
  if error <> nil then begin
    aErrorStr := NSStrToStr(error.localizedDescription);
    TThread.Synchronize(nil,
      procedure
      begin
        if Terminated then exit;
        if assigned(fWebRTC.fOnErrorEvent) then
          fWebRTC.fOnErrorEvent(self, fWebRTC.ERROR_CREATE_SDP, aErrorStr);
      end);
    exit;
  end;

  //check fLocalSdp is null
  if (fLocalSdp <> nil) then begin
    TThread.Synchronize(nil,
      procedure
      begin
        if Terminated then exit;
        if assigned(fWebRTC.fOnErrorEvent) then
          fWebRTC.fOnErrorEvent(self, fWebRTC.ERROR_CREATE_SDP, 'Multiple SDP create.');
      end);
    exit;
   end;

  //init fLocalSdp
  fLocalSdp := sdp;
  fLocalSdp.retain;

  //The RTCPeerConnection.setLocalDescription() method changes the local description associated with
  //the connection. This description specifies the properties of the local end of the connection,
  //including the media format. The method takes a single parameter—the session description—and it
  //returns a Promise which is fulfilled once the description has been changed, asynchronously.
  //If setLocalDescription() is called while a connection is already in place, it means renegotiation
  //is underway (possibly to adapt to changing network conditions). Because descriptions will be
  //exchanged until the two peers agree on a configuration, the description submitted by calling
  //setLocalDescription() does not immediately take effect. Instead, the current connection configuration
  //remains in place until negotiation is complete. Only then does the agreed-upon configuration take effect.
  fPeerConnection.setLocalDescription(fLocalSdp, SetSessionDescriptionCompletionHandler);

end;

{****************************************************************************}
procedure TALiOSWebRTC.SetSessionDescriptionCompletionHandler(error: NSError);
var aSDPType: TALWebRTCSDPType;
    aErrorStr: String;
    aSDPStr: String;
begin

  {$IFDEF DEBUG}
  if (error <> nil) then allog('TALWebRTC.SetSessionDescriptionCompletionHandler', 'Failed to set session description. Error: ' + NSStrToStr(error.localizedDescription) +
                                                                                   ' - ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.error);
  {$ENDIF}

  //check if error
  if error <> nil then begin
    aErrorStr := NSStrToStr(error.localizedDescription);
    TThread.Synchronize(nil,
      procedure
      begin
        if Terminated then exit;
        if assigned(fWebRTC.fOnErrorEvent) then
          fWebRTC.fOnErrorEvent(self, fWebRTC.ERROR_SET_SDP, aErrorStr);
      end);
    exit;
  end;

  //process the sdp
  if fIsInitiator then begin

    // For offering peer connection we first create offer and set
    // local SDP, then after receiving answer set remote SDP.
    if (fPeerConnection.RemoteDescription = nil) then begin

      // We've just set our local SDP so time to send it.
      {$IFDEF DEBUG}
      allog('TALWebRTC.SetSessionDescriptionCompletionHandler', 'Local SDP set succesfully' +
                                                                ' - sdp.description: ' + NSStrToStr(fLocalSdp.sdp) +
                                                                ' - ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.verbose);
      {$ENDIF}

      if fLocalSdp.&type = RTCSdpTypeOffer then aSDPType := TALWebRTCSDPType.OFFER
      else if fLocalSdp.&type = RTCSdpTypeANSWER then aSDPType := TALWebRTCSDPType.ANSWER
      else if fLocalSdp.&type = RTCSdpTypePRANSWER then aSDPType := TALWebRTCSDPType.PRANSWER
      else raise Exception.createFmt('Unknown Description Type (%d)', [fLocalSdp.&type]);

      aSDPStr := NSStrToStr(fLocalSdp.sdp);

      fLocalSdp.release;
      fLocalSdp := nil;

      TThread.Synchronize(nil,
        procedure
        begin
          if Terminated then exit;
          if assigned(fWebRTC.fonLocalDescriptionEvent) then
            fWebRTC.fonLocalDescriptionEvent(self, aSDPType, aSDPStr);
        end);

    end
    else begin

      // We've just set remote description, so drain remote
      // and send local ICE candidates.
      {$IFDEF DEBUG}
      allog('TALWebRTC.SetSessionDescriptionCompletionHandler', 'Remote SDP set succesfully' +
                                                                ' - ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.verbose);
      {$ENDIF}

      Enqueue(
        procedure
        begin
          drainCandidates();
        end);

    end;

  end
  else begin

    // For answering peer connection we set remote SDP and then
    // create answer and set local SDP.
    if (fPeerConnection.LocalDescription <> nil) then begin

      // We've just set our local SDP so time to send it, drain
      // remote and send local ICE candidates.
      {$IFDEF DEBUG}
      allog('TALWebRTC.SetSessionDescriptionCompletionHandler', 'Local SDP set succesfully' +
                                                                ' - sdp.description: ' + NsStrToStr(fLocalSdp.sdp) +
                                                                ' - ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.verbose);
      {$ENDIF}

      if fLocalSdp.&type = RTCSdpTypeOffer then aSDPType := TALWebRTCSDPType.OFFER
      else if fLocalSdp.&type = RTCSdpTypeANSWER then aSDPType := TALWebRTCSDPType.ANSWER
      else if fLocalSdp.&type = RTCSdpTypePRANSWER then aSDPType := TALWebRTCSDPType.PRANSWER
      else raise Exception.createFmt('Unknown Description Type (%d)', [fLocalSdp.&type]);

      aSDPStr := NsStrToStr(fLocalSdp.sdp);

      fLocalSdp.release;
      fLocalSdp := nil;

      TThread.Synchronize(nil,
        procedure
        begin
          if Terminated then exit;
          if assigned(fWebRTC.fonLocalDescriptionEvent) then
            fWebRTC.fonLocalDescriptionEvent(self, aSDPType, aSDPStr);
        end);

      Enqueue(
        procedure
        begin
          drainCandidates();
        end);

    end
    else begin

      // We've just set remote SDP - do nothing for now -
      // answer will be created soon.
      {$IFDEF DEBUG}
      allog('TALWebRTC.SetSessionDescriptionCompletionHandler', 'Remote SDP set succesfully' +
                                                                ' - ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.verbose);
      {$ENDIF}

    end;

  end;

end;

{*******************************************************************}
function TALiOSWebRTC.createSdpMediaConstraints: RTCMediaConstraints;
var aMandatoryConstraints: NSMutableDictionary;
begin

  aMandatoryConstraints := TNSMutableDictionary.Create;
  try
    aMandatoryConstraints.setValue((StrToNSStr('true') as ILocalObject).GetObjectID, StrToNSStr('OfferToReceiveAudio'));
    aMandatoryConstraints.setValue((StrToNSStr(ALIfThenU(fPeerConnectionParameters.videoCallEnabled, 'true', 'false')) as ILocalObject).GetObjectID, StrToNSStr('OfferToReceiveVideo'));
    result := TRTCMediaConstraints.Wrap(
                TRTCMediaConstraints.Wrap(
                  TRTCMediaConstraints.OCClass.alloc).
                    initWithMandatoryConstraints(aMandatoryConstraints, //mandatory: NSDictionary;
                                                 nil)); //optionalConstraints: NSDictionary
  finally
    aMandatoryConstraints.release;
  end;

end;

{*************************************}
procedure TALiOSWebRTC.drainCandidates;
var aALWebRTCIceCandidate: TALWebRTCIceCandidate;
var aRTCIceCandidate: RTCIceCandidate;
begin

  if (fQueuedRemoteCandidates <> nil) then begin
    for aALWebRTCIceCandidate in fQueuedRemoteCandidates do begin
      aRTCIceCandidate := TRTCIceCandidate.wrap(
                            TRTCIceCandidate.wrap(
                              TRTCIceCandidate.OCClass.alloc).
                                initWithSdp(
                                  StrToNsStr(aALWebRTCIceCandidate.Sdp),// sdp: NSString;
                                  aALWebRTCIceCandidate.SdpMLineIndex, // sdpMLineIndex: Integer;
                                  StrToNsStr(aALWebRTCIceCandidate.SdpMid))); // sdpMid: NSString)
      try
        fPeerConnection.addIceCandidate(aRTCIceCandidate);
      finally
        aRTCIceCandidate.release;
      end;
    end;
    AlFreeAndNil(fQueuedRemoteCandidates);
  end;

end;

{*****************************************************************************************************}
function TALiOSWebRTC.findDeviceForPosition(const aPosition: AVCaptureDevicePosition): AVCaptureDevice;
var aCaptureDevices: NSArray;
    I: integer;
begin
  result := nil;
  aCaptureDevices := TRTCCameraVideoCapturer.OCClass.captureDevices;
  for I := 0 to aCaptureDevices.count - 1 do begin
    result := TAVCaptureDevice.Wrap(aCaptureDevices.objectAtIndex(I));
    if result.position = aPosition then exit;
  end;
end;

{**************************************************************************************************************************************************}
function TALiOSWebRTC.selectFormatForDevice(const aDevice: AVCaptureDevice; const aPreferredOutputPixelFormat: FourCharCode): AVCaptureDeviceFormat;
var aSupportedFormats: NSArray;
    aSupportedFormat: AVCaptureDeviceFormat;
    aTargetWidth: integer;
    aTargetHeight: integer;
    aCurrentDiff: integer;
    aDimension: CMVideoDimensions;
    aPixelFormat: FourCharCode;
    aDiff: integer;
    I: integer;
begin
  aSupportedFormats := TRTCCameraVideoCapturer.OCClass.supportedFormatsForDevice(aDevice);
  aTargetWidth := fPeerConnectionParameters.videoWidth;
  aTargetHeight := fPeerConnectionParameters.videoHeight;
  result := nil;
  aCurrentDiff := MaxInt;
  for I := 0 to aSupportedFormats.count - 1 do begin
    aSupportedFormat := TAVCaptureDeviceFormat.Wrap(aSupportedFormats.objectAtIndex(i));
    aDimension := CMVideoFormatDescriptionGetDimensions(aSupportedFormat.formatDescription);
    aPixelFormat := CMFormatDescriptionGetMediaSubType(aSupportedFormat.formatDescription);
    aDiff := abs(aTargetWidth - aDimension.width) + abs(aTargetHeight - aDimension.height);
    if (aDiff < aCurrentDiff) then begin
      result := aSupportedFormat;
      aCurrentDiff := aDiff;
    end
    else if (aDiff = aCurrentDiff) and (aPixelFormat = aPreferredOutputPixelFormat) then result := aSupportedFormat;
  end;
end;

{****************************************************************************************}
function TALiOSWebRTC.selectFpsForFormat(const aFormat: AVCaptureDeviceFormat): NSInteger;
var aMaxSupportedFramerate: Float64;
    i: integer;
begin
  aMaxSupportedFramerate := 0;
  for I := 0 to aFormat.videoSupportedFrameRateRanges.count - 1 do
    aMaxSupportedFramerate := max(aMaxSupportedFramerate, TAVFrameRateRange.Wrap(aFormat.videoSupportedFrameRateRanges.ObjectatIndex(i)).maxFrameRate);
  result := trunc(min(aMaxSupportedFramerate, kFramerateLimit));
end;

{**************************************************************************************}
constructor TALiOSWebRTC.TPeerConnectionDelegate.Create(const aiOSWebRTC: TALiOSWebRTC);
begin
  inherited Create;
  fiOSWebRTC := aiOSWebRTC;
end;

{******************************************************************************************************************************************************************}
procedure TALiOSWebRTC.TPeerConnectionDelegate.peerConnectionDidChangeSignalingState(peerConnection: RTCPeerConnection; didChangeSignalingState: RTCSignalingState);
begin
  {$IFDEF DEBUG}
  allog('TALWebRTC.TPeerConnectionDelegate.peerConnectionDidChangeSignalingState', 'ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.verbose);
  {$ENDIF}
end;

{*****************************************************************************************************************************************}
procedure TALiOSWebRTC.TPeerConnectionDelegate.peerConnectionDidAddStream(peerConnection: RTCPeerConnection; didAddStream: RTCMediaStream);
begin
  {$IFDEF DEBUG}
  allog('TALWebRTC.TPeerConnectionDelegate.peerConnectionDidAddStream', 'ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.verbose);
  {$ENDIF}
end;

{***********************************************************************************************************************************************}
procedure TALiOSWebRTC.TPeerConnectionDelegate.peerConnectionDidRemoveStream(peerConnection: RTCPeerConnection; didRemoveStream: RTCMediaStream);
begin
  {$IFDEF DEBUG}
  allog('TALWebRTC.TPeerConnectionDelegate.peerConnectionDidRemoveStream', 'ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.verbose);
  {$ENDIF}
end;

{**************************************************************************************************************}
procedure TALiOSWebRTC.TPeerConnectionDelegate.peerConnectionShouldNegotiate(peerConnection: RTCPeerConnection);
begin

  {$IFDEF DEBUG}
  allog('TALWebRTC.TPeerConnectionDelegate.peerConnectionShouldNegotiate', 'ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.verbose);
  {$ENDIF}

  // No need to do anything; AppRTC follows a pre-agreed-upon
  // signaling/negotiation protocol.

end;

{******************************************************************************************************************************************************************************}
procedure TALiOSWebRTC.TPeerConnectionDelegate.peerConnectionDidChangeIceConnectionState(peerConnection: RTCPeerConnection; didChangeIceConnectionState: RTCIceConnectionState);
var aState: TALWebRTCIceConnectionState;
begin

  {$IFDEF DEBUG}
  allog('TALWebRTC.TPeerConnectionDelegate.peerConnectionDidChangeIceConnectionState', 'ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.verbose);
  {$ENDIF}

  if didChangeIceConnectionState = RTCIceConnectionStateCONNECTED then aState := TALWebRTCIceConnectionState.CONNECTED
  else if didChangeIceConnectionState = RTCIceConnectionStateDISCONNECTED then aState := TALWebRTCIceConnectionState.DISCONNECTED
  else if didChangeIceConnectionState = RTCIceConnectionStateFAILED then aState := TALWebRTCIceConnectionState.FAILED
  else if didChangeIceConnectionState = RTCIceConnectionStateCLOSED then aState := TALWebRTCIceConnectionState.CLOSED
  else if didChangeIceConnectionState = RTCIceConnectionStateCOMPLETED then aState := TALWebRTCIceConnectionState.COMPLETED
  else if didChangeIceConnectionState = RTCIceConnectionStateCHECKING then aState := TALWebRTCIceConnectionState.CHECKING
  else if didChangeIceConnectionState = RTCIceConnectionStateNEW then aState := TALWebRTCIceConnectionState.NEW
  else if didChangeIceConnectionState = RTCIceConnectionStateCount then exit
  else raise Exception.createfmt('Unknown Ice Connection State (%d)', [didChangeIceConnectionState]);

  TThread.synchronize(nil,
    procedure
    begin
      if fiOSWebRTC.Terminated then exit;
      if assigned(fiOSWebRTC.fWebRTC.fonIceConnectionChangeEvent) then
        fiOSWebRTC.fWebRTC.fonIceConnectionChangeEvent(fiOSWebRTC.fWebRTC,aState);
    end);

end;

{***************************************************************************************************************************************************************************}
procedure TALiOSWebRTC.TPeerConnectionDelegate.peerConnectionDidChangeIceGatheringState(peerConnection: RTCPeerConnection; didChangeIceGatheringState: RTCIceGatheringState);
begin
  {$IFDEF DEBUG}
  allog('TALWebRTC.TPeerConnectionDelegate.peerConnectionDidChangeIceGatheringState', 'ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.verbose);
  {$ENDIF}
end;

{****************************************************************************************************************************************************************}
procedure TALiOSWebRTC.TPeerConnectionDelegate.peerConnectionDidGenerateIceCandidate(peerConnection: RTCPeerConnection; didGenerateIceCandidate: RTCIceCandidate);
var aALWebRTCIceCandidate: TALWebRTCIceCandidate;
begin

  {$IFDEF DEBUG}
  allog('TALWebRTC.TPeerConnectionDelegate.peerConnectionDidGenerateIceCandidate', 'ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.verbose);
  {$ENDIF}

  aALWebRTCIceCandidate := TALWebRTCIceCandidate.Create(
                             NSStrToStr(didGenerateIceCandidate.sdpMid),
                             didGenerateIceCandidate.sdpMLineIndex,
                             NSStrToStr(didGenerateIceCandidate.sdp));

  TThread.synchronize(nil,
    procedure
    begin
      if fiOSWebRTC.Terminated then exit;
      if assigned(fiOSWebRTC.fWebRTC.fonIceCandidateEvent) then
        fiOSWebRTC.fWebRTC.fonIceCandidateEvent(fiOSWebRTC.fWebRTC, aALWebRTCIceCandidate);
    end);

end;

{******************************************************************************************************************************************************}
procedure TALiOSWebRTC.TPeerConnectionDelegate.peerConnectionDidRemoveIceCandidates(peerConnection: RTCPeerConnection; didRemoveIceCandidates: NSArray);
var aALWebRTCIceCandidates: TALWebRTCIceCandidates;
    aRTCIceCandidate: RTCIceCandidate;
    i: integer;
begin

  {$IFDEF DEBUG}
  allog('TALWebRTC.TPeerConnectionDelegate.peerConnectionDidRemoveIceCandidates', 'ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.verbose);
  {$ENDIF}

  setlength(aALWebRTCIceCandidates, didRemoveIceCandidates.count);
  for I := 0 to didRemoveIceCandidates.count - 1 do begin
    aRTCIceCandidate := TRTCIceCandidate.Wrap(didRemoveIceCandidates.objectAtIndex(I));
    aALWebRTCIceCandidates[i] := TALWebRTCIceCandidate.Create(
                                   NSStrToStr(aRTCIceCandidate.sdpMid),
                                   aRTCIceCandidate.sdpMLineIndex,
                                   NSStrToStr(aRTCIceCandidate.sdp));
  end;

  TThread.synchronize(nil,
    procedure
    begin
      if fiOSWebRTC.Terminated then exit;
      if assigned(fiOSWebRTC.fWebRTC.fonIceCandidatesRemovedEvent) then
        fiOSWebRTC.fWebRTC.fonIceCandidatesRemovedEvent(fiOSWebRTC.fWebRTC, aALWebRTCIceCandidates);
    end);

end;

{*****************************************************************************************************************************************************}
procedure TALiOSWebRTC.TPeerConnectionDelegate.peerConnectionDidOpenDataChannel(peerConnection: RTCPeerConnection; didOpenDataChannel: RTCDataChannel);
begin
  {$IFDEF DEBUG}
  allog('TALWebRTC.TPeerConnectionDelegate.peerConnectionDidOpenDataChannel', 'ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.verbose);
  {$ENDIF}
end;

{********************************************************************************************************************************************************************************}
procedure TALiOSWebRTC.TPeerConnectionDelegate.peerConnectionDidStartReceivingOnTransceiver(peerConnection: RTCPeerConnection; didStartReceivingOnTransceiver: RTCRtpTransceiver);
begin
  {$IFDEF DEBUG}
  allog('TALWebRTC.TPeerConnectionDelegate.peerConnectionDidStartReceivingOnTransceiver', 'ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.verbose);
  {$ENDIF}
end;

{**********************************************************************************************************************************************************************}
procedure TALiOSWebRTC.TPeerConnectionDelegate.peerConnectionDidAddReceiverStreams(peerConnection: RTCPeerConnection; didAddReceiver: RTCRtpReceiver; streams: NSArray);
begin
  {$IFDEF DEBUG}
  allog('TALWebRTC.TPeerConnectionDelegate.peerConnectionDidAddReceiverStreams', 'ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.verbose);
  {$ENDIF}
end;

{***************************************************************************************************************************************************}
procedure TALiOSWebRTC.TPeerConnectionDelegate.peerConnectionDidRemoveReceiver(peerConnection: RTCPeerConnection; didRemoveReceiver: RTCRtpReceiver);
begin
  {$IFDEF DEBUG}
  allog('TALWebRTC.TPeerConnectionDelegate.peerConnectionDidRemoveReceiver', 'ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.verbose);
  {$ENDIF}
end;

{***************************************************************************************}
constructor TALiOSWebRTC.TLocalVideoTrackRenderer.Create(const aiOSWebRTC: TALiOSWebRTC);
begin
  inherited Create;
  fiOSWebRTC := aiOSWebRTC;
end;

{********************************************************************}
procedure TALiOSWebRTC.TLocalVideoTrackRenderer.setSize(size: CGSize);
begin
  {$IFDEF DEBUG}
  allog('TALWebRTC.TLocalVideoTrackRenderer.setSize', 'ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.verbose);
  {$ENDIF}
end;

{********************************************************************************}
procedure TALiOSWebRTC.TLocalVideoTrackRenderer.renderFrame(frame: RTCVideoFrame);
begin

  {$IFDEF DEBUG}
  //allog('TALWebRTC.TLocalVideoTrackRenderer.renderFrame', 'ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.verbose);
  {$ENDIF}

  TThread.synchronize(nil,
  procedure
  var aPixelBuffer: CVPixelBufferRef;
      aTextureRefLuma: CVOpenGLESTextureRef;
      aTextureRefChroma: CVOpenGLESTextureRef;
      aLumaWidth, aLumaHeight: integer;
      aChromaWidth, aChromaHeight: integer;
      aReturnValue: CVReturn;
  begin

    //-----
    if fiOSWebRTC.Terminated then exit;

    //init aPixelBuffer
    aPixelBuffer := TRTCCVPixelBuffer.Wrap(Frame.buffer).pixelBuffer;
    if aPixelBuffer = 0 then begin // could be nil if nothing should be displayed
      {$IFDEF DEBUG}
      ALLog('TALWebRTC.TLocalVideoTrackRenderer.renderFrame', 'pixelBuffer:nil', TalLogType.warn);
      {$ENDIF}
      exit; // could be nil if nothing should be displayed
    end;

    //check the CVPixelBufferGetPixelFormatType
    if CVPixelBufferGetPixelFormatType(aPixelBuffer) <> kCVPixelFormatType_420YpCbCr8BiPlanarVideoRange then
      raise Exception.CreateFmt('TALWebRTC.TLocalVideoTrackRenderer.renderFrame: Unknown pixel format type (%d)', [CVPixelBufferGetPixelFormatType(aPixelBuffer)]);


    /////////////
    // Y-plane //
    /////////////

    //-----
    aLumaWidth := CVPixelBufferGetWidth(aPixelBuffer); // Returns the width of the pixel buffer.
    aLumaHeight := CVPixelBufferGetHeight(aPixelBuffer); // Returns the height of the pixel buffer.

    //-----
    // This function either creates a new or returns a cached CVOpenGLESTexture texture object mapped to the
    // CVImageBuffer and associated parameters. This operation creates a live binding between the image buffer
    // and the underlying texture object. The EAGLContext associated with the cache may be modified to create,
    // delete, or bind textures. When used as a source texture or GL_COLOR_ATTACHMENT, the image buffer must be
    // unlocked before rendering. The source or render buffer texture should not be re-used until the rendering
    // has completed. This can be guaranteed by calling glFlush()
    //
    // The texture cache automatically flushes currently unused resources when you call the
    // CVOpenGLESTextureCacheCreateTextureFromImage function
    aReturnValue := CVOpenGLESTextureCacheCreateTextureFromImage(kCFAllocatorDefault, // allocator: The CFAllocator to use for allocating the texture object. This parameter can be NULL.
                                                                 fiOSWebRTC.fvideoTextureCacheRef, // textureCache: The texture cache object that will manage the texture.
                                                                 aPixelBuffer, // sourceImage: The CVImageBuffer that you want to create a texture from.
                                                                 nil,  // textureAttributes: A CFDictionary containing the attributes to be used for creating the CVOpenGLESTexture objects. This parameter can be NULL.
                                                                 GL_TEXTURE_2D, // target: The target texture. GL_TEXTURE_2D and GL_RENDERBUFFER are the only targets currently supported.
                                                                 GL_RED_EXT,  // internalFormat: The number of color components in the texture. Examples are GL_RGBA, GL_LUMINANCE, GL_RGBA8_OES, GL_RED, and GL_RG.
                                                                 aLumaWidth, // width: The width of the texture image.
                                                                 aLumaHeight, // height The height of the texture image.
                                                                 GL_RED_EXT,  // format: The format of the pixel data. Examples are GL_RGBA and GL_LUMINANCE.
                                                                 GL_UNSIGNED_BYTE, // type: The data type of the pixel data. One example is GL_UNSIGNED_BYTE.
                                                                 0,  // planeIndex: The plane of the CVImageBuffer to map bind. Ignored for non-planar CVImageBuffers.
                                                                 @aTextureRefLuma); // textureOut: A pointer to a CVOpenGLESTexture where the newly created texture object will be placed.
    if aReturnValue <> kCVReturnSuccess then begin
      {$IFDEF DEBUG}
      ALLog('TALWebRTC.renderFrame', alFormatU('CVOpenGLESTextureCacheCreateTextureFromImage (Luma) failed: %d', [aReturnValue]), TalLogType.Error);
      {$ENDIF}
      exit;
    end;


    //////////////
    // UV-plane //
    //////////////

    //-----
    aChromaWidth := CVPixelBufferGetWidthOfPlane(aPixelBuffer, 1); // Returns the width of the pixel buffer.
    aChromaHeight := CVPixelBufferGetHeightOfPlane(aPixelBuffer, 1); // Returns the height of the pixel buffer.

    //-----
    // This function either creates a new or returns a cached CVOpenGLESTexture texture object mapped to the
    // CVImageBuffer and associated parameters. This operation creates a live binding between the image buffer
    // and the underlying texture object. The EAGLContext associated with the cache may be modified to create,
    // delete, or bind textures. When used as a source texture or GL_COLOR_ATTACHMENT, the image buffer must be
    // unlocked before rendering. The source or render buffer texture should not be re-used until the rendering
    // has completed. This can be guaranteed by calling glFlush()
    //
    // The texture cache automatically flushes currently unused resources when you call the
    // CVOpenGLESTextureCacheCreateTextureFromImage function
    aReturnValue := CVOpenGLESTextureCacheCreateTextureFromImage(kCFAllocatorDefault, // allocator: The CFAllocator to use for allocating the texture object. This parameter can be NULL.
                                                                 fiOSWebRTC.fvideoTextureCacheRef, // textureCache: The texture cache object that will manage the texture.
                                                                 aPixelBuffer, // sourceImage: The CVImageBuffer that you want to create a texture from.
                                                                 nil,  // textureAttributes: A CFDictionary containing the attributes to be used for creating the CVOpenGLESTexture objects. This parameter can be NULL.
                                                                 GL_TEXTURE_2D, // target: The target texture. GL_TEXTURE_2D and GL_RENDERBUFFER are the only targets currently supported.
                                                                 GL_RG_EXT,  // internalFormat: The number of color components in the texture. Examples are GL_RGBA, GL_LUMINANCE, GL_RGBA8_OES, GL_RED, and GL_RG.
                                                                 aChromaWidth, // width: The width of the texture image.
                                                                 aChromaHeight, // height The height of the texture image.
                                                                 GL_RG_EXT,  // format: The format of the pixel data. Examples are GL_RGBA and GL_LUMINANCE.
                                                                 GL_UNSIGNED_BYTE, // type: The data type of the pixel data. One example is GL_UNSIGNED_BYTE.
                                                                 1,  // planeIndex: The plane of the CVImageBuffer to map bind. Ignored for non-planar CVImageBuffers.
                                                                 @aTextureRefChroma); // textureOut: A pointer to a CVOpenGLESTexture where the newly created texture object will be placed.
    if aReturnValue <> kCVReturnSuccess then begin
      {$IFDEF DEBUG}
      ALLog('TALWebRTC.renderFrame', alFormatU('CVOpenGLESTextureCacheCreateTextureFromImage (Chroma) failed: %d', [aReturnValue]), TalLogType.Error);
      {$ENDIF}
      cfRElease(pointer(aTextureRefLuma));
      exit;
    end;


    /////////////
    // Y-plane //
    /////////////

    //-----
    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, CVOpenGLESTextureGetName(aTextureRefLuma));
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    case fiOSWebRTC.fWebRTC.FLocalBitmap.MagFilter of
      TTextureFilter.Nearest: glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
      TTextureFilter.Linear: glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    end;
    case fiOSWebRTC.fWebRTC.FLocalBitmap.MinFilter of
      TTextureFilter.Nearest: glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
      TTextureFilter.Linear: glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    end;
    glBindTexture(GL_TEXTURE_2D, 0);

    //-----
    {$IF CompilerVersion > 32} // tokyo
      {$MESSAGE WARN 'Check if this is still true and adjust the IFDEF'}
    {$ENDIF}
    TALTextureAccessPrivate(fiOSWebRTC.fWebRTC.FLocalBitmap).FWidth := aLumaWidth;
    TALTextureAccessPrivate(fiOSWebRTC.fWebRTC.FLocalBitmap).FHeight := aLumaHeight; // we can't use setsize because it's fill finalise the texture
                                                                                     // but with/height are used only in
                                                                                     // procedure TCanvasHelper.TexRect(const DestCorners, SrcCorners: TCornersF; const Texture: TTexture; const Color1, Color2, Color3, Color4: TAlphaColor);
                                                                                     // begin
                                                                                     //   ...
                                                                                     //   if (Texture = nil) or (Texture.Width < 1) or (Texture.Height < 1) then Exit
                                                                                     //   ...
                                                                                     //   InvTexSize := PointF(1 / Texture.Width, 1 / Texture.Height);
                                                                                     //   ...
                                                                                     // end
                                                                                     // so i don't need to finalize the texture !!
    ITextureAccess(fiOSWebRTC.fWebRTC.FLocalBitmap).Handle := CVOpenGLESTextureGetName(aTextureRefLuma);

    //-----
    if fiOSWebRTC.fLocalVideoTextureRefLuma <> 0 then cfRElease(pointer(fiOSWebRTC.fLocalVideoTextureRefLuma));
    fiOSWebRTC.fLocalVideoTextureRefLuma := aTextureRefLuma;


    //////////////
    // UV-plane //
    //////////////

    //-----
    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, CVOpenGLESTextureGetName(aTextureRefChroma));
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    case fiOSWebRTC.fWebRTC.FLocalBitmap.SecondTexture.MagFilter of
      TTextureFilter.Nearest: glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
      TTextureFilter.Linear: glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    end;
    case fiOSWebRTC.fWebRTC.FLocalBitmap.SecondTexture.MinFilter of
      TTextureFilter.Nearest: glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
      TTextureFilter.Linear: glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    end;
    glBindTexture(GL_TEXTURE_2D, 0);

    //-----
    ITextureAccess(fiOSWebRTC.fWebRTC.FLocalBitmap.SecondTexture).Handle := CVOpenGLESTextureGetName(aTextureRefChroma);

    //-----
    if fiOSWebRTC.fLocalVideoTextureRefChroma <> 0 then cfRElease(pointer(fiOSWebRTC.fLocalVideoTextureRefChroma));
    fiOSWebRTC.fLocalVideoTextureRefChroma := aTextureRefChroma;


    ////////////////////////////////
    // OnLocalFrameAvailableEvent //
    ////////////////////////////////

    fiOSWebRTC.fWebRTC.FlocalBitmapRotation := Frame.rotation;
    if assigned(fiOSWebRTC.fWebRTC.fOnLocalFrameAvailableEvent) then
      fiOSWebRTC.fWebRTC.fOnLocalFrameAvailableEvent(self);

  end);

end;

{****************************************************************************************}
constructor TALiOSWebRTC.TRemoteVideoTrackRenderer.Create(const aiOSWebRTC: TALiOSWebRTC);
begin
  inherited Create;
  fiOSWebRTC := aiOSWebRTC;
end;

{*********************************************************************}
procedure TALiOSWebRTC.TRemoteVideoTrackRenderer.setSize(size: CGSize);
begin

  {$IFDEF DEBUG}
  allog('TALWebRTC.TRemoteVideoTrackRenderer.setSize', 'ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.verbose);
  {$ENDIF}

end;

{*********************************************************************************}
procedure TALiOSWebRTC.TRemoteVideoTrackRenderer.renderFrame(frame: RTCVideoFrame);
begin

  {$IFDEF DEBUG}
  //allog('TALWebRTC.TRemoteVideoTrackRenderer.renderFrame', 'ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.verbose);
  {$ENDIF}

  //-----
  TThread.Synchronize(nil,
  procedure
  var aRTCI420Buffer: RTCI420Buffer;
  begin

    //-----
    if fiOSWebRTC.Terminated then exit;

    //-----
    aRTCI420Buffer := TRTCI420Buffer.Wrap(frame.buffer);

    //-----
    glBindTexture(GL_TEXTURE_2D, fiOSWebRTC.fWebRTC.fRemoteBitmap.Handle);
    glTexImage2D(GL_TEXTURE_2D,
                 0,
                 GL_RED_EXT,
                 aRTCI420Buffer.width,
                 aRTCI420Buffer.height,
                 0,
                 GL_RED_EXT,
                 GL_UNSIGNED_BYTE,
                 aRTCI420Buffer.dataY);
    TALTextureAccessPrivate(fiOSWebRTC.fWebRTC.fRemoteBitmap).FWidth := aRTCI420Buffer.width;
    TALTextureAccessPrivate(fiOSWebRTC.fWebRTC.fRemoteBitmap).FHeight := aRTCI420Buffer.height; // we can't use setsize because it's fill finalise the texture
                                                                                                // but with/height are used only in
                                                                                                // procedure TCanvasHelper.TexRect(const DestCorners, SrcCorners: TCornersF; const Texture: TTexture; const Color1, Color2, Color3, Color4: TAlphaColor);
                                                                                                // begin
                                                                                                //   ...
                                                                                                //   if (Texture = nil) or (Texture.Width < 1) or (Texture.Height < 1) then Exit
                                                                                                //   ...
                                                                                                //   InvTexSize := PointF(1 / Texture.Width, 1 / Texture.Height);
                                                                                                //   ...
                                                                                                // end
                                                                                                // so i don't need to finalize the texture !!

    //-----
    glBindTexture(GL_TEXTURE_2D, fiOSWebRTC.fWebRTC.fRemoteBitmap.SecondTexture.Handle);
    glTexImage2D(GL_TEXTURE_2D,
                 0,
                 GL_RED_EXT,
                 aRTCI420Buffer.ChromaWidth,
                 aRTCI420Buffer.ChromaHeight,
                 0,
                 GL_RED_EXT,
                 GL_UNSIGNED_BYTE,
                 aRTCI420Buffer.dataU);

    //-----
    glBindTexture(GL_TEXTURE_2D, fiOSWebRTC.fWebRTC.fRemoteBitmap.ThirdTexture.Handle);
    glTexImage2D(GL_TEXTURE_2D,
                 0,
                 GL_RED_EXT,
                 aRTCI420Buffer.ChromaWidth,
                 aRTCI420Buffer.ChromaHeight,
                 0,
                 GL_RED_EXT,
                 GL_UNSIGNED_BYTE,
                 aRTCI420Buffer.dataV);

    //-----
    fiOSWebRTC.fWebRTC.FRemoteBitmapRotation := Frame.Rotation;
    if assigned(fiOSWebRTC.fWebRTC.fOnRemoteFrameAvailableEvent) then
      fiOSWebRTC.fWebRTC.fOnRemoteFrameAvailableEvent(self);

  end);

end;

{****************************************************}
function TALWebRTC.getLocalBitmap: TALBiPlanarTexture;
begin
  if (fLocalBitmap.Handle = 0) or (fLocalBitmap.width=0) or (fLocalBitmap.Height=0) then result := nil
  else result := fLocalBitmap;
end;

{***************************************************}
function TALWebRTC.getRemoteBitmap: TALPlanarTexture;
begin
  if (fRemoteBitmap.Handle = 0) or (fRemoteBitmap.width=0) or (fRemoteBitmap.Height=0) then result := nil
  else result := fRemoteBitmap;
end;

{$ENDIF}
{$ENDREGION}

Type

  {*******************************************}
  //if i don't don't this i have internal error
  _TProcOfObjectWrapper = class(Tobject)
  public
    class procedure ApplicationEventHandler(const Sender: TObject; const M: TMessage);
  end;

{******************************************************************************************************}
class procedure _TProcOfObjectWrapper.ApplicationEventHandler(const Sender: TObject; const M: TMessage);

{$REGION ' IOS'}
{$IF defined(ios)}
Var aFieldTrials: NSDictionary;
{$ENDIF}
{$ENDREGION}

begin

  {$REGION ' ANDROID'}
  {$IF defined(android)}

  //didFinishLaunchingWithOptions
  if (M is TApplicationEventMessage) then begin
    if ((M as TApplicationEventMessage).Value.Event = TApplicationEvent.FinishedLaunching) then begin

      TJALWebRTC.Javaclass.initializeLibrary(TAndroidHelper.Context.getApplicationContext);

    end

    //applicationWillTerminate
    else if ((M as TApplicationEventMessage).Value.Event = TApplicationEvent.WillTerminate) then begin

      TJALWebRTC.Javaclass.finalizeLibrary();

    end;
  end;

  {$ENDIF}
  {$ENDREGION}


  {$REGION ' IOS'}
  {$IF defined(ios)}

  //didFinishLaunchingWithOptions
  if (M is TApplicationEventMessage) then begin
    if ((M as TApplicationEventMessage).Value.Event = TApplicationEvent.FinishedLaunching) then begin

      aFieldTrials := TNSDictionary.Wrap(TNSDictionary.Wrap(TNSDictionary.OCClass.alloc).init);
      try
        RTCInitFieldTrialDictionary((aFieldTrials as ILocalObject).GetObjectID);
      finally
        aFieldTrials.release;
      end;

      RTCInitializeSSL();
      //RTCSetupInternalTracer();

      {$IFDEF DEBUG}
      // In debug builds the default level is LS_INFO and in non-debug builds it is
      // disabled. Continue to log to console in non-debug builds, but only
      // warnings and errors.
      RTCSetMinDebugLogLevel(RTCLoggingSeverityWarning);
      {$ENDIF}

    end

    //applicationWillTerminate
    else if ((M as TApplicationEventMessage).Value.Event = TApplicationEvent.WillTerminate) then begin

      //RTCShutdownInternalTracer();
      RTCCleanupSSL();

    end;
  end;

  {$ENDIF}
  {$ENDREGION}


end;

initialization
  TMessageManager.DefaultManager.SubscribeToMessage(TApplicationEventMessage, _TProcOfObjectWrapper.ApplicationEventHandler);

finalization
  TMessageManager.DefaultManager.Unsubscribe(TApplicationEventMessage, _TProcOfObjectWrapper.ApplicationEventHandler);

end.
