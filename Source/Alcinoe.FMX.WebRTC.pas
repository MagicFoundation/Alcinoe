unit Alcinoe.FMX.WebRTC;

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

{$I Alcinoe.inc}

uses
  System.sysUtils,
  {$IF defined(android)}
  Androidapi.JNI.JavaTypes,
  Androidapi.JNIBridge,
  FMX.Types3D,
  Alcinoe.AndroidApi.WebRTC,
  Alcinoe.FMX.Types3D,
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
  Alcinoe.iOSApi.WebRTC,
  Alcinoe.FMX.Types3D,
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
                          const aVideoWidth: integer = 1280;
                          const aVideoHeight: integer = 720;
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
    fCameraVideoCapturerStopped: Boolean;
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
    procedure resumeVideoCapturer;
    procedure pauseVideoCapturer;
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
    fLocalBitmap: TALTexture;
    fRemoteBitmap: TALTexture;
    function getLocalBitmap: TALTexture;
    function getRemoteBitmap: TALTexture;
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

    {$REGION ' MSWINDOWS / ALMacOS'}
    {$IF defined(MSWINDOWS) or defined(ALMacOS)}
    function getLocalBitmap: Tbitmap;
    function getRemoteBitmap: Tbitmap;
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
    property LocalBitmap: TALTexture read getLocalBitmap;
    property RemoteBitmap: TALTexture read getRemoteBitmap;
    {$ENDIF}
    {$ENDREGION}

    {$REGION ' IOS'}
    {$IF defined(ios)}
    property LocalBitmap: TALBiPlanarTexture read getLocalBitmap;
    property RemoteBitmap: TALPlanarTexture read getRemoteBitmap;
    {$ENDIF}
    {$ENDREGION}

    {$REGION ' MSWINDOWS / ALMacOS'}
    {$IF defined(MSWINDOWS) or defined(ALMacOS)}
    property LocalBitmap: Tbitmap read getLocalBitmap;
    property RemoteBitmap: Tbitmap read getRemoteBitmap;
    {$ENDIF}
    {$ENDREGION}


  public
    constructor Create(const aIceServers: TALWebRTCIceServers; const aPeerConnectionParameters: TALWebRTCPeerConnectionParameters); virtual;
    destructor Destroy; override;
    function Start: boolean;
    procedure Stop;
    procedure resumeVideoCapturer;
    procedure pauseVideoCapturer;
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

uses
  System.messaging,
  FMX.platform,
  {$IF defined(android)}
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.jni,
  Androidapi.jni.App,
  Androidapi.JNI.OpenGL,
  Androidapi.Helpers,
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
  Alcinoe.StringUtils,
  Alcinoe.Common;

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
                                                        const aVideoWidth: integer = 1280;
                                                        const aVideoHeight: integer = 720;
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
var LJListIceServers: JList;
    LIceServerBuilder: JPeerConnection_IceServer_Builder;
    LJPeerConnectionParameters: JALWebRTC_PeerConnectionParameters;
    I: integer;
{$ENDIF}
{$ENDREGION}

{$REGION ' IOS'}
{$IF defined(ios)}

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  {$IFNDEF ALCompilerVersionSupported}
    {$MESSAGE WARN 'Check if FMX.Context.GLES.TCustomContextOpenGL.DoInitializeTexture still has the same implementation and adjust the IFDEF'}
  {$ENDIF}
  procedure _InitTexture(const aTexture: TTexture);
  var Tex: GLuint;
  begin
    aTexture.Style := aTexture.Style - [TTextureStyle.MipMaps];
    if TALCustomContextIOSAccess.valid then
    begin
      glActiveTexture(GL_TEXTURE0);
      glGenTextures(1, @Tex);
      glBindTexture(GL_TEXTURE_2D, Tex);
      {$IFDEF IOS}
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
      {$ELSE}
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
      {$ENDIF}
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
      TGlesDiagnostic.RaiseIfHasError(@SCannotCreateTexture, [TALCustomContextIOSAccess.ClassName]);
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
    fLocalbitmap.Material := TalTexture.DefExternalOESMaterial;
    fLocalbitmap.Style := fLocalbitmap.Style - [TTextureStyle.MipMaps];
    //-----
    fRemotebitmap := TALTexture.Create;
    fRemotebitmap.Material := TalTexture.DefExternalOESMaterial;
    fRemotebitmap.Style := fRemotebitmap.Style - [TTextureStyle.MipMaps];
    //-----
    fAndroidWebRTC := nil;
    //-----
    LJListIceServers := TJlist.wrap(TJArrayList.JavaClass.init);
    for I := low(aIceServers) to High(aIceServers) do begin
      LIceServerBuilder := TJPeerConnection_IceServer.javaclass.builder(StringToJstring(aIceServers[i].uri));
      if aIceServers[i].username <> '' then LIceServerBuilder := LIceServerBuilder.setUsername(StringToJstring(aIceServers[i].username));
      if aIceServers[i].password <> '' then LIceServerBuilder := LIceServerBuilder.setPassword(StringToJstring(aIceServers[i].password));
      LJListIceServers.add(LIceServerBuilder.createIceServer);
      LIceServerBuilder := nil;
    end;
    //-----
    LJPeerConnectionParameters := TJALWebRTC_PeerConnectionParameters.JavaClass.init;
    LJPeerConnectionParameters.VideoCallEnabled := aPeerConnectionParameters.VideoCallEnabled;
    LJPeerConnectionParameters.VideoWidth := aPeerConnectionParameters.VideoWidth;
    LJPeerConnectionParameters.VideoHeight := aPeerConnectionParameters.VideoHeight;
    LJPeerConnectionParameters.VideoFps := aPeerConnectionParameters.VideoFps;
    LJPeerConnectionParameters.VideoMaxBitrate := aPeerConnectionParameters.VideoMaxBitrate;
    LJPeerConnectionParameters.VideoCodec := StringToJstring(aPeerConnectionParameters.VideoCodec);
    LJPeerConnectionParameters.VideoCodecHwAcceleration := aPeerConnectionParameters.VideoCodecHwAcceleration;
    LJPeerConnectionParameters.AudioStartBitrate := aPeerConnectionParameters.AudioStartBitrate;
    LJPeerConnectionParameters.AudioCodec := StringToJstring(aPeerConnectionParameters.AudioCodec);
    LJPeerConnectionParameters.NoAudioProcessing := aPeerConnectionParameters.NoAudioProcessing;
    LJPeerConnectionParameters.AecDump := aPeerConnectionParameters.AecDump;
    LJPeerConnectionParameters.DisableBuiltInAEC := aPeerConnectionParameters.DisableBuiltInAEC;
    LJPeerConnectionParameters.DisableBuiltInNS := aPeerConnectionParameters.DisableBuiltInNS;
    LJPeerConnectionParameters.DataChannelEnabled := aPeerConnectionParameters.DataChannelEnabled;
    LJPeerConnectionParameters.DataChannelOrdered := aPeerConnectionParameters.DataChannelOrdered;
    LJPeerConnectionParameters.DataChannelMaxRetransmitTimeMs := aPeerConnectionParameters.DataChannelMaxRetransmitTimeMs;
    LJPeerConnectionParameters.DataChannelMaxRetransmits := aPeerConnectionParameters.DataChannelMaxRetransmits;
    LJPeerConnectionParameters.DataChannelProtocol := StringToJstring(aPeerConnectionParameters.DataChannelProtocol);
    LJPeerConnectionParameters.DataChannelNegotiated := aPeerConnectionParameters.DataChannelNegotiated;
    LJPeerConnectionParameters.DataChannelId := aPeerConnectionParameters.DataChannelId;
    //-----
    fAndroidWebRTC := TJALWebRTC.Wrap(TJALWebRTC.JavaClass.init(TAndroidHelper.Context.getApplicationContext,
                                                                TJEGL14.JavaClass.eglGetCurrentContext,
                                                                LJListIceServers,
                                                                LJPeerConnectionParameters));
    fAndroidWebRTCListener := TAndroidWebRTCListener.Create(Self);
    fAndroidWebRTC.setListener(fAndroidWebRTCListener);
    //-----
    LJPeerConnectionParameters := nil;
    LJListIceServers := nil;

  {$ENDIF}
  {$ENDREGION}

  {$REGION ' IOS'}
  {$IF defined(ios)}

    fLocalbitmap := TALBiPlanarTexture.Create;
    //-----
    fRemotebitmap := TALPlanarTexture.Create;
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
var LNativeWin: JWindow;
{$ENDIF}
{$ENDREGION}

begin

  {$IFDEF DEBUG}
  allog('TALWebRTC.Start', TalLogType.verbose);
  {$ENDIF}

  {$REGION ' ANDROID'}
  {$IF defined(android)}

    result := fAndroidWebRTC.start;
    if result then begin
      LNativeWin := TAndroidHelper.Activity.getWindow;
      if LNativeWin <> nil then LNativeWin.addFlags(TJWindowManager_LayoutParams.javaclass.FLAG_KEEP_SCREEN_ON);
    end

  {$ENDIF}
  {$ENDREGION}

  {$REGION ' IOS'}
  {$IF defined(ios)}

    result := fiOSWebRTC.start;
    if result then TiOSHelper.SharedApplication.setIdleTimerDisabled(true);

  {$ENDIF}
  {$ENDREGION}

  {$REGION ' MSWINDOWS / ALMacOS'}
  {$IF defined(MSWINDOWS) or defined(ALMacOS)}

    result := false;

  {$ENDIF}
  {$ENDREGION}

end;

{***********************}
procedure TALWebRTC.Stop;

{$REGION ' ANDROID'}
{$IF defined(android)}
var LNativeWin: JWindow;
{$ENDIF}
{$ENDREGION}

begin

  {$IFDEF DEBUG}
  allog('TALWebRTC.Stop', TalLogType.verbose);
  {$ENDIF}

  {$REGION ' ANDROID'}
  {$IF defined(android)}

    if (fAndroidWebRTC <> nil) then begin
      fAndroidWebRTC.setListener(nil);
      fAndroidWebRTC.Stop;
      fAndroidWebRTC := Nil;
    end;
    AlFreeAndNil(fAndroidWebRTCListener);
    LNativeWin := TAndroidHelper.Activity.getWindow;
    if LNativeWin <> nil then LNativeWin.clearFlags(TJWindowManager_LayoutParams.javaclass.FLAG_KEEP_SCREEN_ON);

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

{**************************************}
procedure TALWebRTC.resumeVideoCapturer;
begin

  {$REGION ' ANDROID'}
  {$IF defined(android)}

    fAndroidWebRTC.resumeVideoCapturer;

  {$ENDIF}
  {$ENDREGION}

  {$REGION ' IOS'}
  {$IF defined(ios)}

    fiOSWebRTC.resumeVideoCapturer;

  {$ENDIF}
  {$ENDREGION}

end;

{************************************}
procedure TALWebRTC.pauseVideoCapturer;
begin

  {$REGION ' ANDROID'}
  {$IF defined(android)}

    fAndroidWebRTC.pauseVideoCapturer;

  {$ENDIF}
  {$ENDREGION}

  {$REGION ' IOS'}
  {$IF defined(ios)}

    fiOSWebRTC.pauseVideoCapturer;

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
var LJIceCandidates: TJavaObjectArray<JIceCandidate>;
    I: integer;
{$ENDIF}
{$ENDREGION}

begin

  {$REGION ' ANDROID'}
  {$IF defined(android)}

    LJIceCandidates := TJavaObjectArray<JIceCandidate>.Create(length(aIceCandidates));
    try
      for I := 0 to length(aIceCandidates) - 1 do
        LJIceCandidates.Items[I] := TJIceCandidate.JavaClass.init(
                                      StringToJstring(aIceCandidates[i].SdpMid),
                                      aIceCandidates[i].sdpMLineIndex,
                                      StringToJstring(aIceCandidates[i].Sdp));
      fAndroidWebRtc.removeRemoteIceCandidates(LJIceCandidates);
    finally
      ALFreeAndNil(LJIceCandidates);
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

  {$REGION ' MSWINDOWS / ALMacOS'}
  {$IF defined(MSWINDOWS) or defined(ALMacOS)}

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
  //allog(
  //  'TALWebRTC.TAndroidWebRTCListener.onLocalFrameAvailable',
  //  'textureId: ' + ALIntToStrW(textureId) +
  //  ' - width: ' + ALIntToStrW(width) +
  //  ' - height: ' + ALIntToStrW(height) +
  //  ' - rotation: ' + ALIntToStrW(rotation),
  //  TalLogType.VERBOSE);
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
  //allog(
  //  'TALWebRTC.TAndroidWebRTCListener.onRemoteFrameAvailable',
  //  'textureId: ' + ALIntToStrW(textureId) +
  //  ' - width: ' + ALIntToStrW(width) +
  //  ' - height: ' + ALIntToStrW(height) +
  //  ' - rotation: ' + ALIntToStrW(rotation),
  //  TalLogType.VERBOSE);
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
var LSDPType: TALWebRTCSDPType;
begin

  {$IFDEF DEBUG}
  allog('TALWebRTC.TAndroidWebRTCListener.onLocalDescription','sdp.description: ' + JstringToString(sdp.description), TalLogType.VERBOSE);
  {$ENDIF}

  if assigned(fWebRTC.fonLocalDescriptionEvent) then begin
    if sdp.&type.equals(TJSessionDescription_Type.JavaClass.OFFER) then LSDPType := TALWebRTCSDPType.OFFER
    else if sdp.&type.equals(TJSessionDescription_Type.JavaClass.ANSWER) then LSDPType := TALWebRTCSDPType.ANSWER
    else if sdp.&type.equals(TJSessionDescription_Type.JavaClass.PRANSWER) then LSDPType := TALWebRTCSDPType.PRANSWER
    else raise Exception.createFmt('Unknown Description Type (%s)', [JstringToString(sdp.&type.toString)]);
    fWebRTC.fonLocalDescriptionEvent(fWebRTC, LSDPType, JstringToString(sdp.description));
  end;

end;

{**********************************************************************************}
procedure TALWebRTC.TAndroidWebRTCListener.onIceCandidate(candidate: JIceCandidate);
begin

  {$IFDEF DEBUG}
  allog('TALWebRTC.TAndroidWebRTCListener.onIceCandidate','candidate.sdpMLineIndex: ' + ALIntToStrW(candidate.sdpMLineIndex) +
                                                          ' - candidate.sdpMid: ' + JstringToString(candidate.sdpMid) +
                                                          ' - candidate.serverUrl: ' + JstringToString(candidate.serverUrl) +
                                                          ' - candidate.sdp: ' + JstringToString(candidate.sdp), TalLogType.VERBOSE);
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
var LIceCandidates: TALWebRTCIceCandidates;
    I: integer;
begin

  {$IFDEF DEBUG}
  allog('TALWebRTC.TAndroidWebRTCListener.onIceCandidatesRemoved', TalLogType.VERBOSE);
  {$ENDIF}

  if assigned(fWebRTC.fonIceCandidatesRemovedEvent) then begin
    setlength(LIceCandidates, candidates.Length);
    for I := 0 to candidates.Length - 1 do begin
      LIceCandidates[i] := TALWebRTCIceCandidate.Create(
                             JstringToString(candidates.Items[i].sdpMid),
                             candidates.Items[i].sdpMLineIndex,
                             JstringToString(candidates.Items[i].sdp));
    end;
    fWebRTC.fonIceCandidatesRemovedEvent(fWebRTC, LIceCandidates);
  end;

end;

{*************************************************************************************************************}
procedure TALWebRTC.TAndroidWebRTCListener.onIceConnectionChange(newState: JPeerConnection_IceConnectionState);
var LState: TALWebRTCIceConnectionState;
begin

  {$IFDEF DEBUG}
  allog('TALWebRTC.TAndroidWebRTCListener.onIceConnectionChange','newState: ' + JstringToString(newState.toString), TalLogType.VERBOSE);
  {$ENDIF}

  if assigned(fWebRTC.fonIceConnectionChangeEvent) then begin
    if newState.equals(tJPeerConnection_IceConnectionState.JavaClass.CONNECTED) then LState := TALWebRTCIceConnectionState.CONNECTED
    else if newState.equals(tJPeerConnection_IceConnectionState.JavaClass.DISCONNECTED) then LState := TALWebRTCIceConnectionState.DISCONNECTED
    else if newState.equals(tJPeerConnection_IceConnectionState.JavaClass.FAILED) then LState := TALWebRTCIceConnectionState.FAILED
    else if newState.equals(tJPeerConnection_IceConnectionState.JavaClass.CLOSED) then LState := TALWebRTCIceConnectionState.CLOSED
    else if newState.equals(tJPeerConnection_IceConnectionState.JavaClass.COMPLETED) then LState := TALWebRTCIceConnectionState.COMPLETED
    else if newState.equals(tJPeerConnection_IceConnectionState.JavaClass.CHECKING) then LState := TALWebRTCIceConnectionState.CHECKING
    else if newState.equals(tJPeerConnection_IceConnectionState.JavaClass.NEW) then LState := TALWebRTCIceConnectionState.NEW
    else raise Exception.createFmt('Unknown Ice Connection State (%s)', [JstringToString(newState.toString)]);
    fWebRTC.fonIceConnectionChangeEvent(fWebRTC,LState);
  end;

end;

{**************************************************************************************}
procedure TALWebRTC.TAndroidWebRTCListener.onError(code: Integer; description: JString);
begin

  {$IFDEF DEBUG}
  allog('TALWebRTC.TAndroidWebRTCListener.onError','code: ' + ALIntToStrW(code) + ' - description: ' + JstringToString(description), TalLogType.error);
  {$ENDIF}

  if assigned(fWebRTC.fOnErrorEvent) then
    fWebRTC.fOnErrorEvent(fWebRTC, code, JstringToString(description));

end;

{********************************************}
function TALWebRTC.getLocalBitmap: TALTexture;
begin
  if fLocalBitmap.Handle = 0 then result := nil
  else result := fLocalBitmap;
end;

{*********************************************}
function TALWebRTC.getRemoteBitmap: TALTexture;
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
Var LWebRTCConfig: RTCAudioSessionConfiguration;
begin

  {$IFDEF DEBUG}
  allog('TALiOSWebRTC.Create', TalLogType.verbose);
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
  fCameraVideoCapturerStopped := true;
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
  LwebRTCConfig := TRTCAudioSessionConfiguration.Wrap(TRTCAudioSessionConfiguration.OCClass.webRTCConfiguration);
  LwebRTCConfig.setCategoryOptions(LwebRTCConfig.CategoryOptions or AVAudioSessionCategoryOptionDefaultToSpeaker);
  TRTCAudioSessionConfiguration.OCClass.setWebRTCConfiguration(LwebRTCConfig);
  //-----
  inherited Create(False); // see http://www.gerixsoft.com/blog/delphi/fixing-symbol-resume-deprecated-warning-delphi-2010

end;

{******************************}
destructor TALiOSWebRTC.Destroy;
begin

  {$IFDEF DEBUG}
  allog('TALiOSWebRTC.Destroy', TalLogType.verbose);
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
var LProc: TProc;
begin

  //loop still not terminated
  while not Terminated do begin
    Try

      Tmonitor.Enter(fQueue);
      try
        if (fQueue.Count > 0) then LProc := fQueue.Dequeue()
        else LProc := nil;
      finally
        Tmonitor.exit(fQueue);
      end;
      if assigned(LProc) then begin
        LProc();
        LProc := nil;
      end
      else fSignal.WaitFor;

    Except
      on E: Exception do begin
        {$IFDEF DEBUG}
        allog('TALiOSWebRTC.Execute.Error','code: 0 - description: ' + e.Message , TalLogType.error);
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

  {$IFDEF DEBUG}
  allog('TALiOSWebRTC.Execute', 'Terminating', TalLogType.verbose);
  {$ENDIF}

  //release all retained object
  //-----
  if fpeerConnection <> nil then begin
    fpeerConnection.setdelegate(nil); // << remove the fPeerConnectionDelegate
    fpeerConnection.close; // << Terminate all media and close the transport.
  end;
  //-----
  if fCameraVideoCapturer <> nil then begin
    fCameraVideoCapturer.stopCapture;
    fCameraVideoCapturerStopped := true;
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
    var LDecoderFactory: RTCDefaultVideoDecoderFactory;
        LEncoderFactory: RTCDefaultVideoEncoderFactory;
        LVideoCodecInfo: RTCVideoCodecInfo;
        LH264ProfileLevelId: RTCH264ProfileLevelId;
        LSupportedCodecs: NSArray;
        LVideoCodecName: String;
        LConfiguration: RTCConfiguration;
        LMediaConstraints: RTCMediaConstraints;
        LIceServers: array of Pointer;
        LAudioSource: RTCAudioSource;
        LCaptureDevice: AVCaptureDevice;
        LCaptureDeviceFormat: AVCaptureDeviceFormat;
        LCertificate: RTCCertificate;
        LCertificateParams: NSMutableDictionary;
        LOptionalConstraints: NSMutableDictionary;
        LTransceiver: RTCRtpTransceiver;
        I: integer;
    begin

      //exit if already started
      if fPeerConnectionFactory <> nil then exit;

      //init fPeerConnectionFactory
      LDecoderFactory := TRTCDefaultVideoDecoderFactory.Wrap(TRTCDefaultVideoDecoderFactory.Wrap(TRTCDefaultVideoDecoderFactory.OCClass.alloc).init);
      LEncoderFactory := TRTCDefaultVideoEncoderFactory.Wrap(TRTCDefaultVideoEncoderFactory.Wrap(TRTCDefaultVideoEncoderFactory.OCClass.alloc).init);
      try

        LVideoCodecInfo := nil;
        LSupportedCodecs := TRTCDefaultVideoEncoderFactory.OCClass.supportedCodecs;
        for I := LSupportedCodecs.count - 1 downto 0 do begin
          LVideoCodecInfo := TRTCVideoCodecInfo.Wrap(TRTCDefaultVideoEncoderFactory.OCClass.supportedCodecs.objectAtIndex(I));
          LVideoCodecName := NSStrToStr(LVideoCodecInfo.name);
          if ALSameTextW(LVideoCodecName, 'H264') then begin
            LH264ProfileLevelId := TRTCH264ProfileLevelId.wrap(
                                     TRTCH264ProfileLevelId.wrap(
                                       TRTCH264ProfileLevelId.OCClass.alloc).
                                         initWithHexString(
                                           TNSString.Wrap(LVideoCodecInfo.parameters.objectForKey((StrToNsStr('profile-level-id') as IlocalObject).GetObjectID))));
            try
              if (LH264ProfileLevelId.profile = RTCH264ProfileConstrainedHigh) or
                 (LH264ProfileLevelId.profile = RTCH264ProfileHigh) then LVideoCodecName := 'H264 High'
              else if (LH264ProfileLevelId.profile = RTCH264ProfileConstrainedBaseline) or
                      (LH264ProfileLevelId.profile = RTCH264ProfileBaseline) then LVideoCodecName := 'H264 Baseline';
            finally
              LH264ProfileLevelId.release;
            end;
          end;
          if ALSameTextW(LVideoCodecName, fPeerConnectionParameters.videoCodec) then break;
        end;
        if LVideoCodecInfo <> nil then LEncoderFactory.setPreferredCodec(LVideoCodecInfo);
        //-----
        fPeerConnectionFactory := TRTCPeerConnectionFactory.Wrap(
                                    TRTCPeerConnectionFactory.Wrap(
                                      TRTCPeerConnectionFactory.OCClass.alloc).
                                        initWithEncoderFactory(
                                          (LEncoderFactory as ILocalObject).GetObjectID,
                                          (LDecoderFactory as ILocalObject).GetObjectID));

      finally
        LDecoderFactory.release;
        LEncoderFactory.release;
      end;

      //-----
      fQueuedRemoteCandidates := TList<TALWebRTCIceCandidate>.create;

      //-----
      setlength(LIceServers, length(fIceServers));
      for I := low(LIceServers) to High(LIceServers) do begin
        if (fIceServers[I].username = '') and
           (fIceServers[I].password = '') then LIceServers[I] := TRTCIceServer.Wrap(
                                                                   TRTCIceServer.OCClass.alloc).
                                                                     initWithURLStrings(
                                                                       TNSArray.Wrap(TNSArray.OCClass.arrayWithObject((StrToNSStr(fIceServers[I].uri) as ILocalObject).GetObjectID))) // urlStrings: NSArray)
        else LIceServers[I] := TRTCIceServer.Wrap(
                                 TRTCIceServer.OCClass.alloc).
                                    initWithURLStringsUsernameCredential(
                                      TNSArray.Wrap(TNSArray.OCClass.arrayWithObject((StrToNSStr(fIceServers[I].uri) as ILocalObject).GetObjectID)), // urlStrings: NSArray;
                                      StrToNSStr(fIceServers[I].username), // username: NSString;
                                      StrToNSStr(fIceServers[I].password)); // credential: NSString)
      end;

      try

        LConfiguration := TRTCConfiguration.Wrap(TRTCConfiguration.Wrap(TRTCConfiguration.OCClass.alloc).init);
        try

          LConfiguration.setIceServers(TNSArray.Wrap(TNSArray.OCClass.arrayWithObjects(@LIceServers[0], Length(LIceServers))));
          //-----
          LCertificateParams := TNSMutableDictionary.Create;
          try
            LCertificateParams.setValue((StrToNSStr('RSASSA-PKCS1-v1_5') as ILocalObject).GetObjectID, StrToNSStr('name'));
            LCertificateParams.setValue(TNSNumber.OCClass.numberWithInt(100000), StrToNSStr('expires'));
            LCertificate := TRTCCertificate.oCClass.generateCertificateWithParams(LCertificateParams); // << we don't need to call release (https://stackoverflow.com/questions/52314334/ios-objective-c-object-when-to-use-release-and-when-to-not-use-it)
            LConfiguration.setCertificate(LCertificate);
          finally
            LCertificateParams.release;
          end;
          //-----
          LConfiguration.setsdpSemantics(RTCSdpSemanticsUnifiedPlan);
          //-----
          LOptionalConstraints := TNSMutableDictionary.Create;
          try
            LOptionalConstraints.setValue((StrToNSStr('true') as ILocalObject).GetObjectID, StrToNSStr('DtlsSrtpKeyAgreement'));
            LMediaConstraints := TRTCMediaConstraints.Wrap(
                                   TRTCMediaConstraints.Wrap(
                                     TRTCMediaConstraints.OCClass.alloc).
                                       initWithMandatoryConstraints(nil, //mandatory: NSDictionary;
                                                                    LOptionalConstraints)); //optionalConstraints: NSDictionary
            try

              fPeerConnectionDelegate := TPeerConnectionDelegate.Create(self);
              fpeerConnection := fPeerConnectionFactory.peerConnectionWithConfiguration(LConfiguration, // configuration: RTCConfiguration;
                                                                                        LMediaConstraints, // constraints: RTCMediaConstraints;
                                                                                        fPeerConnectionDelegate.GetObjectID); //delegate: Pointer);
              fpeerConnection.retain;

            finally
              LMediaConstraints.release;
            end;
          finally
            LOptionalConstraints.release;
          end;

        finally
          LConfiguration.release;
        end;

      finally
        for I := Low(LIceServers) to High(LIceServers) do
          TRTCIceServer.Wrap(LIceServers[I]).release;
      end;

      //-----
      LMediaConstraints := TRTCMediaConstraints.Wrap(
                             TRTCMediaConstraints.Wrap(
                               TRTCMediaConstraints.OCClass.alloc).
                                 initWithMandatoryConstraints(nil, //mandatory: NSDictionary;
                                                              nil)); //optionalConstraints: NSDictionary
      try

        LAudioSource := fPeerConnectionFactory.audioSourceWithConstraints(LMediaConstraints); // << we don't need to call release (https://stackoverflow.com/questions/52314334/ios-objective-c-object-when-to-use-release-and-when-to-not-use-it)
        fLocalAudioTrack := fPeerConnectionFactory.audioTrackWithSource(LAudioSource, StrToNSStr(kARDAudioTrackId)); // << we don't need to call release (https://stackoverflow.com/questions/52314334/ios-objective-c-object-when-to-use-release-and-when-to-not-use-it)
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

          LCaptureDevice := findDeviceForPosition(AVCaptureDevicePositionFront);
          LCaptureDeviceFormat := selectFormatForDevice(LCaptureDevice, fCameraVideoCapturer.PreferredOutputPixelFormat);
          if LCaptureDeviceFormat = nil then raise Exception.Create('No valid formats for device');
          fCameraVideoCapturer.startCaptureWithDeviceFormatFps(LCaptureDevice, // device: AVCaptureDevice;
                                                               LCaptureDeviceFormat, // format: AVCaptureDeviceFormat;
                                                               selectFpsForFormat(LCaptureDeviceFormat)); // fps: NSInteger)
          fCameraVideoCapturerStopped := False;

          // We can set up rendering for the remote track right away since the transceiver already has an
          // RTCRtpReceiver with a track. The track will automatically get unmuted and produce frames
          // once RTP is received.
          LTransceiver := nil;
          for I := 0 to fpeerConnection.transceivers.count - 1 do begin
            LTransceiver := tRTCRtpTransceiver.Wrap(fpeerConnection.transceivers.objectAtIndex(I));
            if LTransceiver.mediaType <> RTCRtpMediaTypeVideo then LTransceiver := nil
            else break;
          end;
          if LTransceiver = nil then raise Exception.Create('No remote video track founded');
          fRemoteVideoTrack := TRTCVideoTrack.Wrap((LTransceiver.receiver.track as ILocalObject).GetObjectID);
          fRemoteVideoTrack.retain;
          fRemoteVideoTrackRenderer := TRemoteVideoTrackRenderer.create(self);
          fRemoteVideoTrack.addRenderer(fRemoteVideoTrackRenderer.GetObjectID);

        end;

      finally
        LMediaConstraints.release;
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

{*****************************************}
procedure TALiOSWebRTC.resumeVideoCapturer;
begin

  Enqueue(
    Procedure
    var LCaptureDevice: AVCaptureDevice;
        LCaptureDeviceFormat: AVCaptureDeviceFormat;
    begin
      if not fCameraVideoCapturerStopped then exit;
      LCaptureDevice := findDeviceForPosition(AVCaptureDevicePositionFront);
      LCaptureDeviceFormat := selectFormatForDevice(LCaptureDevice, fCameraVideoCapturer.PreferredOutputPixelFormat);
      if LCaptureDeviceFormat = nil then raise Exception.Create('No valid formats for device');
      fCameraVideoCapturer.startCaptureWithDeviceFormatFps(LCaptureDevice, // device: AVCaptureDevice;
                                                           LCaptureDeviceFormat, // format: AVCaptureDeviceFormat;
                                                           selectFpsForFormat(LCaptureDeviceFormat)); // fps: NSInteger)
      fCameraVideoCapturerStopped := False;
    end);

end;

{****************************************}
procedure TALiOSWebRTC.pauseVideoCapturer;
begin

  Enqueue(
    Procedure
    begin
      if fCameraVideoCapturerStopped then exit;
      fCameraVideoCapturer.stopCapture;
      fCameraVideoCapturerStopped := true;
    end);

end;

{*********************************}
procedure TALiOSWebRTC.createOffer;
begin

  Enqueue(
    Procedure
    var LMediaConstraints: RTCMediaConstraints;
    begin

      //-----
      fIsInitiator := true;

      //-----
      LMediaConstraints := createSdpMediaConstraints;
      try
        fpeerConnection.offerForConstraints(LMediaConstraints, CreateSessionDescriptionCompletionHandler);
      finally
        LMediaConstraints.release;
      end;

    end);

end;

{**********************************}
procedure TALiOSWebRTC.createAnswer;
begin

  Enqueue(
    Procedure
    var LMediaConstraints: RTCMediaConstraints;
    begin

      //-----
      fIsInitiator := false;

      //-----
      LMediaConstraints := createSdpMediaConstraints;
      try
        fpeerConnection.answerForConstraints(LMediaConstraints, CreateSessionDescriptionCompletionHandler);
      finally
        LMediaConstraints.release;
      end;

    end);

end;

{***********************************************************************************************************}
procedure TALiOSWebRTC.setRemoteDescription(const aSdpType: TALWebRTCSDPType; const aSdpDescription: String);
begin

  Enqueue(
    Procedure
    var LSdp: RTCSessionDescription;
    begin
      case aSdpType of
        TALWebRTCSDPType.OFFER:    LSdp := TRTCSessionDescription.Wrap(TRTCSessionDescription.Wrap(TRTCSessionDescription.OCClass.alloc).initWithType(RTCSdpTypeOFFER,    StrToNsStr(aSdpDescription)));
        TALWebRTCSDPType.ANSWER:   LSdp := TRTCSessionDescription.Wrap(TRTCSessionDescription.Wrap(TRTCSessionDescription.OCClass.alloc).initWithType(RTCSdpTypeANSWER,   StrToNsStr(aSdpDescription)));
        TALWebRTCSDPType.PRANSWER: LSdp := TRTCSessionDescription.Wrap(TRTCSessionDescription.Wrap(TRTCSessionDescription.OCClass.alloc).initWithType(RTCSdpTypePRANSWER, StrToNsStr(aSdpDescription)));
      end;
      fPeerConnection.setRemoteDescription(LSdp, SetSessionDescriptionCompletionHandler);
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
    var LRTCIceCandidate: RTCIceCandidate;
    begin

      if fQueuedRemoteCandidates <> nil then fQueuedRemoteCandidates.Add(aIceCandidate)
      else begin
        LRTCIceCandidate := TRTCIceCandidate.wrap(
                              TRTCIceCandidate.wrap(
                                TRTCIceCandidate.OCClass.alloc).
                                  initWithSdp(
                                    StrToNsStr(aIceCandidate.Sdp),// sdp: NSString;
                                    aIceCandidate.SdpMLineIndex, // sdpMLineIndex: Integer;
                                    StrToNsStr(aIceCandidate.SdpMid))); // sdpMid: NSString)
        try
          fPeerConnection.addIceCandidate(LRTCIceCandidate);
        finally
          LRTCIceCandidate.release;
        end;
      end;

    end);

end;

{****************************************************************************************************}
procedure TALiOSWebRTC.removeRemoteIceCandidates(const aIceCandidates: Tarray<TALWebRTCIceCandidate>);
begin

  Enqueue(
    Procedure
    var LRTCIceCandidates: array of Pointer;
        I: integer;
    begin

      // Drain the queued remote candidates if there is any so that
      // they are processed in the proper order.
      drainCandidates();

      //-----
      setlength(LRTCIceCandidates, length(aIceCandidates));
      for I := low(LRTCIceCandidates) to High(LRTCIceCandidates) do
        LRTCIceCandidates[I] := TRTCIceCandidate.wrap(
                                  TRTCIceCandidate.OCClass.alloc).
                                    initWithSdp(
                                      StrToNsStr(aIceCandidates[I].Sdp),// sdp: NSString;
                                      aIceCandidates[I].SdpMLineIndex, // sdpMLineIndex: Integer;
                                      StrToNsStr(aIceCandidates[I].SdpMid)); // sdpMid: NSString)
      try
        fPeerConnection.removeIceCandidates(TNSArray.Wrap(TNSArray.OCClass.arrayWithObjects(@LRTCIceCandidates[0], Length(LRTCIceCandidates))));
      finally
        for I := Low(LRTCIceCandidates) to High(LRTCIceCandidates) do
          TRTCIceCandidate.Wrap(LRTCIceCandidates[I]).release;
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
    var LSender: RTCRtpSender;
        LParametersToModify: RTCRtpParameters;
        I,J: integer;
    begin

      for I := 0 to fPeerConnection.senders.count - 1 do begin
        LSender := TRTCRtpSender.Wrap(fPeerConnection.senders.objectAtIndex(I));
        if (LSender.track <> nil) and (NSStrToStr(LSender.track.kind) = kARDVideoTrackKind) then begin
          LParametersToModify := LSender.parameters;
          for J := 0 to LParametersToModify.encodings.count - 1 do begin
            TRTCRtpEncodingParameters.wrap(
              LParametersToModify.encodings.objectAtIndex(J))
                .setMaxBitrateBps(TNSNumber.Wrap(TNSNumber.OCClass.numberWithInt(maxBitrateKbps * 1000)));
          end;
        end;
      end;

    end);

    result := true;

end;

{***********************************************************************************************************}
procedure TALiOSWebRTC.CreateSessionDescriptionCompletionHandler(sdp: RTCSessionDescription; error: NSError);
var LErrorStr: String;
begin

  {$IFDEF DEBUG}
  if (error <> nil) then allog('TALWebRTC.CreateSessionDescriptionCompletionHandler', 'Failed to create session description. Error: ' + NSStrToStr(error.localizedDescription), TalLogType.error)
  else allog('TALWebRTC.CreateSessionDescriptionCompletionHandler', 'Session description successfully created', TalLogType.verbose);
  {$ENDIF}

  //check if error
  if error <> nil then begin
    LErrorStr := NSStrToStr(error.localizedDescription);
    TThread.Synchronize(nil,
      procedure
      begin
        if Terminated then exit;
        if assigned(fWebRTC.fOnErrorEvent) then
          fWebRTC.fOnErrorEvent(self, fWebRTC.ERROR_CREATE_SDP, LErrorStr);
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
  //including the media format. The method takes a single parameter�the session description�and it
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
var LSDPType: TALWebRTCSDPType;
    LErrorStr: String;
    LSDPStr: String;
begin

  {$IFDEF DEBUG}
  if (error <> nil) then allog('TALWebRTC.SetSessionDescriptionCompletionHandler', 'Failed to set session description. Error: ' + NSStrToStr(error.localizedDescription), TalLogType.error);
  {$ENDIF}

  //check if error
  if error <> nil then begin
    LErrorStr := NSStrToStr(error.localizedDescription);
    TThread.Synchronize(nil,
      procedure
      begin
        if Terminated then exit;
        if assigned(fWebRTC.fOnErrorEvent) then
          fWebRTC.fOnErrorEvent(self, fWebRTC.ERROR_SET_SDP, LErrorStr);
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
      allog('TALWebRTC.SetSessionDescriptionCompletionHandler', 'Local SDP set succesfully - sdp.description: ' + NSStrToStr(fLocalSdp.sdp), TalLogType.verbose);
      {$ENDIF}

      if fLocalSdp.&type = RTCSdpTypeOffer then LSDPType := TALWebRTCSDPType.OFFER
      else if fLocalSdp.&type = RTCSdpTypeANSWER then LSDPType := TALWebRTCSDPType.ANSWER
      else if fLocalSdp.&type = RTCSdpTypePRANSWER then LSDPType := TALWebRTCSDPType.PRANSWER
      else raise Exception.createFmt('Unknown Description Type (%d)', [fLocalSdp.&type]);

      LSDPStr := NSStrToStr(fLocalSdp.sdp);

      fLocalSdp.release;
      fLocalSdp := nil;

      TThread.Synchronize(nil,
        procedure
        begin
          if Terminated then exit;
          if assigned(fWebRTC.fonLocalDescriptionEvent) then
            fWebRTC.fonLocalDescriptionEvent(self, LSDPType, LSDPStr);
        end);

    end
    else begin

      // We've just set remote description, so drain remote
      // and send local ICE candidates.
      {$IFDEF DEBUG}
      allog('TALWebRTC.SetSessionDescriptionCompletionHandler', 'Remote SDP set succesfully', TalLogType.verbose);
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
      allog('TALWebRTC.SetSessionDescriptionCompletionHandler', 'Local SDP set succesfully - sdp.description: ' + NsStrToStr(fLocalSdp.sdp), TalLogType.verbose);
      {$ENDIF}

      if fLocalSdp.&type = RTCSdpTypeOffer then LSDPType := TALWebRTCSDPType.OFFER
      else if fLocalSdp.&type = RTCSdpTypeANSWER then LSDPType := TALWebRTCSDPType.ANSWER
      else if fLocalSdp.&type = RTCSdpTypePRANSWER then LSDPType := TALWebRTCSDPType.PRANSWER
      else raise Exception.createFmt('Unknown Description Type (%d)', [fLocalSdp.&type]);

      LSDPStr := NsStrToStr(fLocalSdp.sdp);

      fLocalSdp.release;
      fLocalSdp := nil;

      TThread.Synchronize(nil,
        procedure
        begin
          if Terminated then exit;
          if assigned(fWebRTC.fonLocalDescriptionEvent) then
            fWebRTC.fonLocalDescriptionEvent(self, LSDPType, LSDPStr);
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
      allog('TALWebRTC.SetSessionDescriptionCompletionHandler', 'Remote SDP set succesfully', TalLogType.verbose);
      {$ENDIF}

    end;

  end;

end;

{*******************************************************************}
function TALiOSWebRTC.createSdpMediaConstraints: RTCMediaConstraints;
var LMandatoryConstraints: NSMutableDictionary;
begin

  LMandatoryConstraints := TNSMutableDictionary.Create;
  try
    LMandatoryConstraints.setValue((StrToNSStr('true') as ILocalObject).GetObjectID, StrToNSStr('OfferToReceiveAudio'));
    LMandatoryConstraints.setValue((StrToNSStr(ALIfThenW(fPeerConnectionParameters.videoCallEnabled, 'true', 'false')) as ILocalObject).GetObjectID, StrToNSStr('OfferToReceiveVideo'));
    result := TRTCMediaConstraints.Wrap(
                TRTCMediaConstraints.Wrap(
                  TRTCMediaConstraints.OCClass.alloc).
                    initWithMandatoryConstraints(LMandatoryConstraints, //mandatory: NSDictionary;
                                                 nil)); //optionalConstraints: NSDictionary
  finally
    LMandatoryConstraints.release;
  end;

end;

{*************************************}
procedure TALiOSWebRTC.drainCandidates;
var LWebRTCIceCandidate: TALWebRTCIceCandidate;
    LRTCIceCandidate: RTCIceCandidate;
begin

  if (fQueuedRemoteCandidates <> nil) then begin
    for LWebRTCIceCandidate in fQueuedRemoteCandidates do begin
      LRTCIceCandidate := TRTCIceCandidate.wrap(
                            TRTCIceCandidate.wrap(
                              TRTCIceCandidate.OCClass.alloc).
                                initWithSdp(
                                  StrToNsStr(LWebRTCIceCandidate.Sdp),// sdp: NSString;
                                  LWebRTCIceCandidate.SdpMLineIndex, // sdpMLineIndex: Integer;
                                  StrToNsStr(LWebRTCIceCandidate.SdpMid))); // sdpMid: NSString)
      try
        fPeerConnection.addIceCandidate(LRTCIceCandidate);
      finally
        LRTCIceCandidate.release;
      end;
    end;
    AlFreeAndNil(fQueuedRemoteCandidates);
  end;

end;

{*****************************************************************************************************}
function TALiOSWebRTC.findDeviceForPosition(const aPosition: AVCaptureDevicePosition): AVCaptureDevice;
var LCaptureDevices: NSArray;
    I: integer;
begin
  result := nil;
  LCaptureDevices := TRTCCameraVideoCapturer.OCClass.captureDevices;
  for I := 0 to LCaptureDevices.count - 1 do begin
    result := TAVCaptureDevice.Wrap(LCaptureDevices.objectAtIndex(I));
    if result.position = aPosition then exit;
  end;
end;

{**************************************************************************************************************************************************}
function TALiOSWebRTC.selectFormatForDevice(const aDevice: AVCaptureDevice; const aPreferredOutputPixelFormat: FourCharCode): AVCaptureDeviceFormat;
var LSupportedFormats: NSArray;
    LSupportedFormat: AVCaptureDeviceFormat;
    LTargetWidth: integer;
    LTargetHeight: integer;
    LCurrentDiff: integer;
    LDimension: CMVideoDimensions;
    LPixelFormat: FourCharCode;
    LDiff: integer;
    I: integer;
begin
  LSupportedFormats := TRTCCameraVideoCapturer.OCClass.supportedFormatsForDevice(aDevice);
  LTargetWidth := fPeerConnectionParameters.videoWidth;
  LTargetHeight := fPeerConnectionParameters.videoHeight;
  result := nil;
  LCurrentDiff := MaxInt;
  for I := 0 to LSupportedFormats.count - 1 do begin
    LSupportedFormat := TAVCaptureDeviceFormat.Wrap(LSupportedFormats.objectAtIndex(i));
    LDimension := CMVideoFormatDescriptionGetDimensions(LSupportedFormat.formatDescription);
    LPixelFormat := CMFormatDescriptionGetMediaSubType(LSupportedFormat.formatDescription);
    LDiff := abs(LTargetWidth - LDimension.width) + abs(LTargetHeight - LDimension.height);
    if (LDiff < LCurrentDiff) then begin
      result := LSupportedFormat;
      LCurrentDiff := LDiff;
    end
    else if (LDiff = LCurrentDiff) and (LPixelFormat = aPreferredOutputPixelFormat) then result := LSupportedFormat;
  end;
end;

{****************************************************************************************}
function TALiOSWebRTC.selectFpsForFormat(const aFormat: AVCaptureDeviceFormat): NSInteger;
var LMaxSupportedFramerate: Float64;
    I: integer;
begin
  LMaxSupportedFramerate := 0;
  for I := 0 to aFormat.videoSupportedFrameRateRanges.count - 1 do
    LMaxSupportedFramerate := max(LMaxSupportedFramerate, TAVFrameRateRange.Wrap(aFormat.videoSupportedFrameRateRanges.ObjectatIndex(I)).maxFrameRate);
  result := trunc(min(LMaxSupportedFramerate, kFramerateLimit));
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
  allog('TALWebRTC.TPeerConnectionDelegate.peerConnectionDidChangeSignalingState', TalLogType.verbose);
  {$ENDIF}
end;

{*****************************************************************************************************************************************}
procedure TALiOSWebRTC.TPeerConnectionDelegate.peerConnectionDidAddStream(peerConnection: RTCPeerConnection; didAddStream: RTCMediaStream);
begin
  {$IFDEF DEBUG}
  allog('TALWebRTC.TPeerConnectionDelegate.peerConnectionDidAddStream', TalLogType.verbose);
  {$ENDIF}
end;

{***********************************************************************************************************************************************}
procedure TALiOSWebRTC.TPeerConnectionDelegate.peerConnectionDidRemoveStream(peerConnection: RTCPeerConnection; didRemoveStream: RTCMediaStream);
begin
  {$IFDEF DEBUG}
  allog('TALWebRTC.TPeerConnectionDelegate.peerConnectionDidRemoveStream', TalLogType.verbose);
  {$ENDIF}
end;

{**************************************************************************************************************}
procedure TALiOSWebRTC.TPeerConnectionDelegate.peerConnectionShouldNegotiate(peerConnection: RTCPeerConnection);
begin

  {$IFDEF DEBUG}
  allog('TALWebRTC.TPeerConnectionDelegate.peerConnectionShouldNegotiate', TalLogType.verbose);
  {$ENDIF}

  // No need to do anything; AppRTC follows a pre-agreed-upon
  // signaling/negotiation protocol.

end;

{******************************************************************************************************************************************************************************}
procedure TALiOSWebRTC.TPeerConnectionDelegate.peerConnectionDidChangeIceConnectionState(peerConnection: RTCPeerConnection; didChangeIceConnectionState: RTCIceConnectionState);
var LState: TALWebRTCIceConnectionState;
begin

  {$IFDEF DEBUG}
  allog('TALWebRTC.TPeerConnectionDelegate.peerConnectionDidChangeIceConnectionState', TalLogType.verbose);
  {$ENDIF}

  if didChangeIceConnectionState = RTCIceConnectionStateCONNECTED then LState := TALWebRTCIceConnectionState.CONNECTED
  else if didChangeIceConnectionState = RTCIceConnectionStateDISCONNECTED then LState := TALWebRTCIceConnectionState.DISCONNECTED
  else if didChangeIceConnectionState = RTCIceConnectionStateFAILED then LState := TALWebRTCIceConnectionState.FAILED
  else if didChangeIceConnectionState = RTCIceConnectionStateCLOSED then LState := TALWebRTCIceConnectionState.CLOSED
  else if didChangeIceConnectionState = RTCIceConnectionStateCOMPLETED then LState := TALWebRTCIceConnectionState.COMPLETED
  else if didChangeIceConnectionState = RTCIceConnectionStateCHECKING then LState := TALWebRTCIceConnectionState.CHECKING
  else if didChangeIceConnectionState = RTCIceConnectionStateNEW then LState := TALWebRTCIceConnectionState.NEW
  else if didChangeIceConnectionState = RTCIceConnectionStateCount then exit
  else raise Exception.createfmt('Unknown Ice Connection State (%d)', [didChangeIceConnectionState]);

  TThread.synchronize(nil,
    procedure
    begin
      if fiOSWebRTC.Terminated then exit;
      if assigned(fiOSWebRTC.fWebRTC.fonIceConnectionChangeEvent) then
        fiOSWebRTC.fWebRTC.fonIceConnectionChangeEvent(fiOSWebRTC.fWebRTC,LState);
    end);

end;

{***************************************************************************************************************************************************************************}
procedure TALiOSWebRTC.TPeerConnectionDelegate.peerConnectionDidChangeIceGatheringState(peerConnection: RTCPeerConnection; didChangeIceGatheringState: RTCIceGatheringState);
begin
  {$IFDEF DEBUG}
  allog('TALWebRTC.TPeerConnectionDelegate.peerConnectionDidChangeIceGatheringState', TalLogType.verbose);
  {$ENDIF}
end;

{****************************************************************************************************************************************************************}
procedure TALiOSWebRTC.TPeerConnectionDelegate.peerConnectionDidGenerateIceCandidate(peerConnection: RTCPeerConnection; didGenerateIceCandidate: RTCIceCandidate);
var LWebRTCIceCandidate: TALWebRTCIceCandidate;
begin

  {$IFDEF DEBUG}
  allog('TALWebRTC.TPeerConnectionDelegate.peerConnectionDidGenerateIceCandidate', TalLogType.verbose);
  {$ENDIF}

  LWebRTCIceCandidate := TALWebRTCIceCandidate.Create(
                             NSStrToStr(didGenerateIceCandidate.sdpMid),
                             didGenerateIceCandidate.sdpMLineIndex,
                             NSStrToStr(didGenerateIceCandidate.sdp));

  TThread.synchronize(nil,
    procedure
    begin
      if fiOSWebRTC.Terminated then exit;
      if assigned(fiOSWebRTC.fWebRTC.fonIceCandidateEvent) then
        fiOSWebRTC.fWebRTC.fonIceCandidateEvent(fiOSWebRTC.fWebRTC, LWebRTCIceCandidate);
    end);

end;

{******************************************************************************************************************************************************}
procedure TALiOSWebRTC.TPeerConnectionDelegate.peerConnectionDidRemoveIceCandidates(peerConnection: RTCPeerConnection; didRemoveIceCandidates: NSArray);
var LWebRTCIceCandidates: TALWebRTCIceCandidates;
    LRTCIceCandidate: RTCIceCandidate;
    i: integer;
begin

  {$IFDEF DEBUG}
  allog('TALWebRTC.TPeerConnectionDelegate.peerConnectionDidRemoveIceCandidates', TalLogType.verbose);
  {$ENDIF}

  setlength(LWebRTCIceCandidates, didRemoveIceCandidates.count);
  for I := 0 to didRemoveIceCandidates.count - 1 do begin
    LRTCIceCandidate := TRTCIceCandidate.Wrap(didRemoveIceCandidates.objectAtIndex(I));
    LWebRTCIceCandidates[i] := TALWebRTCIceCandidate.Create(
                                   NSStrToStr(LRTCIceCandidate.sdpMid),
                                   LRTCIceCandidate.sdpMLineIndex,
                                   NSStrToStr(LRTCIceCandidate.sdp));
  end;

  TThread.synchronize(nil,
    procedure
    begin
      if fiOSWebRTC.Terminated then exit;
      if assigned(fiOSWebRTC.fWebRTC.fonIceCandidatesRemovedEvent) then
        fiOSWebRTC.fWebRTC.fonIceCandidatesRemovedEvent(fiOSWebRTC.fWebRTC, LWebRTCIceCandidates);
    end);

end;

{*****************************************************************************************************************************************************}
procedure TALiOSWebRTC.TPeerConnectionDelegate.peerConnectionDidOpenDataChannel(peerConnection: RTCPeerConnection; didOpenDataChannel: RTCDataChannel);
begin
  {$IFDEF DEBUG}
  allog('TALWebRTC.TPeerConnectionDelegate.peerConnectionDidOpenDataChannel', TalLogType.verbose);
  {$ENDIF}
end;

{********************************************************************************************************************************************************************************}
procedure TALiOSWebRTC.TPeerConnectionDelegate.peerConnectionDidStartReceivingOnTransceiver(peerConnection: RTCPeerConnection; didStartReceivingOnTransceiver: RTCRtpTransceiver);
begin
  {$IFDEF DEBUG}
  allog('TALWebRTC.TPeerConnectionDelegate.peerConnectionDidStartReceivingOnTransceiver', TalLogType.verbose);
  {$ENDIF}
end;

{**********************************************************************************************************************************************************************}
procedure TALiOSWebRTC.TPeerConnectionDelegate.peerConnectionDidAddReceiverStreams(peerConnection: RTCPeerConnection; didAddReceiver: RTCRtpReceiver; streams: NSArray);
begin
  {$IFDEF DEBUG}
  allog('TALWebRTC.TPeerConnectionDelegate.peerConnectionDidAddReceiverStreams', TalLogType.verbose);
  {$ENDIF}
end;

{***************************************************************************************************************************************************}
procedure TALiOSWebRTC.TPeerConnectionDelegate.peerConnectionDidRemoveReceiver(peerConnection: RTCPeerConnection; didRemoveReceiver: RTCRtpReceiver);
begin
  {$IFDEF DEBUG}
  allog('TALWebRTC.TPeerConnectionDelegate.peerConnectionDidRemoveReceiver', TalLogType.verbose);
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
  allog('TALWebRTC.TLocalVideoTrackRenderer.setSize', TalLogType.verbose);
  {$ENDIF}
end;

{********************************************************************************}
procedure TALiOSWebRTC.TLocalVideoTrackRenderer.renderFrame(frame: RTCVideoFrame);
begin

  {$IFDEF DEBUG}
  //allog('TALWebRTC.TLocalVideoTrackRenderer.renderFrame', TalLogType.verbose);
  {$ENDIF}

  TThread.synchronize(nil,
  procedure
  var LPixelBuffer: CVPixelBufferRef;
      LTextureRefLuma: CVOpenGLESTextureRef;
      LTextureRefChroma: CVOpenGLESTextureRef;
      LLumaWidth, LLumaHeight: integer;
      LChromaWidth, LChromaHeight: integer;
      LReturnValue: CVReturn;
  begin

    //-----
    if fiOSWebRTC.Terminated then exit;

    //init aPixelBuffer
    LPixelBuffer := TRTCCVPixelBuffer.Wrap(Frame.buffer).pixelBuffer;
    if LPixelBuffer = 0 then begin // could be nil if nothing should be displayed
      {$IFDEF DEBUG}
      ALLog('TALWebRTC.TLocalVideoTrackRenderer.renderFrame', 'pixelBuffer:nil', TalLogType.warn);
      {$ENDIF}
      exit; // could be nil if nothing should be displayed
    end;

    //check the CVPixelBufferGetPixelFormatType
    if CVPixelBufferGetPixelFormatType(LPixelBuffer) <> kCVPixelFormatType_420YpCbCr8BiPlanarVideoRange then
      raise Exception.CreateFmt('TALWebRTC.TLocalVideoTrackRenderer.renderFrame: Unknown pixel format type (%d)', [CVPixelBufferGetPixelFormatType(LPixelBuffer)]);


    /////////////
    // Y-plane //
    /////////////

    //-----
    LLumaWidth := CVPixelBufferGetWidth(LPixelBuffer); // Returns the width of the pixel buffer.
    LLumaHeight := CVPixelBufferGetHeight(LPixelBuffer); // Returns the height of the pixel buffer.

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
    LReturnValue := CVOpenGLESTextureCacheCreateTextureFromImage(kCFAllocatorDefault, // allocator: The CFAllocator to use for allocating the texture object. This parameter can be NULL.
                                                                 fiOSWebRTC.fvideoTextureCacheRef, // textureCache: The texture cache object that will manage the texture.
                                                                 LPixelBuffer, // sourceImage: The CVImageBuffer that you want to create a texture from.
                                                                 nil,  // textureAttributes: A CFDictionary containing the attributes to be used for creating the CVOpenGLESTexture objects. This parameter can be NULL.
                                                                 GL_TEXTURE_2D, // target: The target texture. GL_TEXTURE_2D and GL_RENDERBUFFER are the only targets currently supported.
                                                                 GL_RED_EXT,  // internalFormat: The number of color components in the texture. Examples are GL_RGBA, GL_LUMINANCE, GL_RGBA8_OES, GL_RED, and GL_RG.
                                                                 LLumaWidth, // width: The width of the texture image.
                                                                 LLumaHeight, // height The height of the texture image.
                                                                 GL_RED_EXT,  // format: The format of the pixel data. Examples are GL_RGBA and GL_LUMINANCE.
                                                                 GL_UNSIGNED_BYTE, // type: The data type of the pixel data. One example is GL_UNSIGNED_BYTE.
                                                                 0,  // planeIndex: The plane of the CVImageBuffer to map bind. Ignored for non-planar CVImageBuffers.
                                                                 @LTextureRefLuma); // textureOut: A pointer to a CVOpenGLESTexture where the newly created texture object will be placed.
    if LReturnValue <> kCVReturnSuccess then begin
      {$IFDEF DEBUG}
      ALLog('TALWebRTC.renderFrame', ALFormatW('CVOpenGLESTextureCacheCreateTextureFromImage (Luma) failed: %d', [LReturnValue]), TalLogType.Error);
      {$ENDIF}
      exit;
    end;


    //////////////
    // UV-plane //
    //////////////

    //-----
    LChromaWidth := CVPixelBufferGetWidthOfPlane(LPixelBuffer, 1); // Returns the width of the pixel buffer.
    LChromaHeight := CVPixelBufferGetHeightOfPlane(LPixelBuffer, 1); // Returns the height of the pixel buffer.

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
    LReturnValue := CVOpenGLESTextureCacheCreateTextureFromImage(kCFAllocatorDefault, // allocator: The CFAllocator to use for allocating the texture object. This parameter can be NULL.
                                                                 fiOSWebRTC.fvideoTextureCacheRef, // textureCache: The texture cache object that will manage the texture.
                                                                 LPixelBuffer, // sourceImage: The CVImageBuffer that you want to create a texture from.
                                                                 nil,  // textureAttributes: A CFDictionary containing the attributes to be used for creating the CVOpenGLESTexture objects. This parameter can be NULL.
                                                                 GL_TEXTURE_2D, // target: The target texture. GL_TEXTURE_2D and GL_RENDERBUFFER are the only targets currently supported.
                                                                 GL_RG_EXT,  // internalFormat: The number of color components in the texture. Examples are GL_RGBA, GL_LUMINANCE, GL_RGBA8_OES, GL_RED, and GL_RG.
                                                                 LChromaWidth, // width: The width of the texture image.
                                                                 LChromaHeight, // height The height of the texture image.
                                                                 GL_RG_EXT,  // format: The format of the pixel data. Examples are GL_RGBA and GL_LUMINANCE.
                                                                 GL_UNSIGNED_BYTE, // type: The data type of the pixel data. One example is GL_UNSIGNED_BYTE.
                                                                 1,  // planeIndex: The plane of the CVImageBuffer to map bind. Ignored for non-planar CVImageBuffers.
                                                                 @LTextureRefChroma); // textureOut: A pointer to a CVOpenGLESTexture where the newly created texture object will be placed.
    if LReturnValue <> kCVReturnSuccess then begin
      {$IFDEF DEBUG}
      ALLog('TALWebRTC.renderFrame', ALFormatW('CVOpenGLESTextureCacheCreateTextureFromImage (Chroma) failed: %d', [LReturnValue]), TalLogType.Error);
      {$ENDIF}
      cfRElease(pointer(LTextureRefLuma));
      exit;
    end;


    /////////////
    // Y-plane //
    /////////////

    //-----
    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, CVOpenGLESTextureGetName(LTextureRefLuma));
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
    {$IFNDEF ALCompilerVersionSupported}
      {$MESSAGE WARN 'Check if FMX.Types3D.TTexture.SetSize is still the same and adjust the IFDEF'}
    {$ENDIF}
    TALTextureAccessPrivate(fiOSWebRTC.fWebRTC.FLocalBitmap).FWidth := LLumaWidth;
    TALTextureAccessPrivate(fiOSWebRTC.fWebRTC.FLocalBitmap).FHeight := LLumaHeight; // we can't use setsize because it's fill finalise the texture
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
    ITextureAccess(fiOSWebRTC.fWebRTC.FLocalBitmap).Handle := CVOpenGLESTextureGetName(LTextureRefLuma);

    //-----
    if fiOSWebRTC.fLocalVideoTextureRefLuma <> 0 then cfRElease(pointer(fiOSWebRTC.fLocalVideoTextureRefLuma));
    fiOSWebRTC.fLocalVideoTextureRefLuma := LTextureRefLuma;


    //////////////
    // UV-plane //
    //////////////

    //-----
    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, CVOpenGLESTextureGetName(LTextureRefChroma));
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
    ITextureAccess(fiOSWebRTC.fWebRTC.FLocalBitmap.SecondTexture).Handle := CVOpenGLESTextureGetName(LTextureRefChroma);

    //-----
    if fiOSWebRTC.fLocalVideoTextureRefChroma <> 0 then cfRElease(pointer(fiOSWebRTC.fLocalVideoTextureRefChroma));
    fiOSWebRTC.fLocalVideoTextureRefChroma := LTextureRefChroma;


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
  allog('TALWebRTC.TRemoteVideoTrackRenderer.setSize', TalLogType.verbose);
  {$ENDIF}

end;

{*********************************************************************************}
procedure TALiOSWebRTC.TRemoteVideoTrackRenderer.renderFrame(frame: RTCVideoFrame);
begin

  {$IFDEF DEBUG}
  //allog('TALWebRTC.TRemoteVideoTrackRenderer.renderFrame', TalLogType.verbose);
  {$ENDIF}

  //-----
  TThread.Synchronize(nil,
  procedure
  var LRTCI420Buffer: RTCI420Buffer;
  begin

    //-----
    if fiOSWebRTC.Terminated then exit;

    //-----
    LRTCI420Buffer := TRTCI420Buffer.Wrap(frame.buffer);

    //-----
    glBindTexture(GL_TEXTURE_2D, fiOSWebRTC.fWebRTC.fRemoteBitmap.Handle);
    glTexImage2D(GL_TEXTURE_2D,
                 0,
                 GL_RED_EXT,
                 LRTCI420Buffer.width,
                 LRTCI420Buffer.height,
                 0,
                 GL_RED_EXT,
                 GL_UNSIGNED_BYTE,
                 LRTCI420Buffer.dataY);
    TALTextureAccessPrivate(fiOSWebRTC.fWebRTC.fRemoteBitmap).FWidth := LRTCI420Buffer.width;
    TALTextureAccessPrivate(fiOSWebRTC.fWebRTC.fRemoteBitmap).FHeight := LRTCI420Buffer.height; // we can't use setsize because it's fill finalise the texture
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
                 LRTCI420Buffer.ChromaWidth,
                 LRTCI420Buffer.ChromaHeight,
                 0,
                 GL_RED_EXT,
                 GL_UNSIGNED_BYTE,
                 LRTCI420Buffer.dataU);

    //-----
    glBindTexture(GL_TEXTURE_2D, fiOSWebRTC.fWebRTC.fRemoteBitmap.ThirdTexture.Handle);
    glTexImage2D(GL_TEXTURE_2D,
                 0,
                 GL_RED_EXT,
                 LRTCI420Buffer.ChromaWidth,
                 LRTCI420Buffer.ChromaHeight,
                 0,
                 GL_RED_EXT,
                 GL_UNSIGNED_BYTE,
                 LRTCI420Buffer.dataV);

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

{$REGION ' MSWINDOWS / ALMacOS'}
{$IF defined(MSWINDOWS) or defined(ALMacOS)}

{*****************************************}
function TALWebRTC.getLocalBitmap: Tbitmap;
begin
  result := nil;
end;

{******************************************}
function TALWebRTC.getRemoteBitmap: Tbitmap;
begin
  result := nil;
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
Var LFieldTrials: NSDictionary;
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

      LFieldTrials := TNSDictionary.Wrap(TNSDictionary.Wrap(TNSDictionary.OCClass.alloc).init);
      try
        RTCInitFieldTrialDictionary((LFieldTrials as ILocalObject).GetObjectID);
      finally
        LFieldTrials.release;
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
