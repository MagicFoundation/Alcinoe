unit ALIosWebRTCApi;

interface

uses
  Macapi.CoreFoundation,
  Macapi.CoreServices,
  Macapi.Dispatch,
  Macapi.Mach,
  Macapi.ObjCRuntime,
  Macapi.ObjectiveC,
  iOSapi.AVFoundation,
  iOSapi.CocoaTypes,
  iOSapi.CoreGraphics,
  iOSapi.CoreVideo,
  iOSapi.Foundation,
  iOSapi.OpenGLES,
  iOSapi.UIKit;

{$M+}

type

  //Represents the signaling state of the peer connection.
  //typedef NS_ENUM(NSInteger, RTCSignalingState)
  RTCSignalingState = NSInteger;

const

  RTCSignalingStateStable = 0;
  RTCSignalingStateHaveLocalOffer = 1;
  RTCSignalingStateHaveLocalPrAnswer = 2;
  RTCSignalingStateHaveRemoteOffer = 3;
  RTCSignalingStateHaveRemotePrAnswer = 4;
  RTCSignalingStateClosed = 5;

type

  //Represents the ice connection state of the peer connection.
  //typedef NS_ENUM(NSInteger, RTCIceConnectionState)
  RTCIceConnectionState = NSInteger;

const

  RTCIceConnectionStateNew = 0;
  RTCIceConnectionStateChecking = 1;
  RTCIceConnectionStateConnected = 2;
  RTCIceConnectionStateCompleted = 3;
  RTCIceConnectionStateFailed = 4;
  RTCIceConnectionStateDisconnected = 5;
  RTCIceConnectionStateClosed = 6;
  RTCIceConnectionStateCount = 7;

type

  //Represents the ice gathering state of the peer connection.
  //typedef NS_ENUM(NSInteger, RTCIceGatheringState)
  RTCIceGatheringState = NSInteger;

const

  RTCIceGatheringStateNew = 0;
  RTCIceGatheringStateGathering = 1;
  RTCIceGatheringStateComplete = 2;

type

  //Represents the stats output level.
  //typedef NS_ENUM(NSInteger, RTCStatsOutputLevel)
  RTCStatsOutputLevel = NSInteger;

const

  RTCStatsOutputLevelStandard = 0;
  RTCStatsOutputLevelDebug = 1;

type

  //typedef NS_ENUM(NSInteger, RTCSourceState)
  RTCSourceState = NSInteger;

const

  RTCSourceStateInitializing = 0;
  RTCSourceStateLive = 1;
  RTCSourceStateEnded = 2;
  RTCSourceStateMuted = 3;

type

  //Represents the state of the track. This exposes the same states in C++.
  //typedef NS_ENUM(NSInteger, RTCMediaStreamTrackState)
  RTCMediaStreamTrackState = NSInteger;

const

  RTCMediaStreamTrackStateLive = 0;
  RTCMediaStreamTrackStateEnded = 1;

type

  //typedef NS_ENUM(NSInteger, RTCVideoRotation)
  RTCVideoRotation = NSInteger;

const

  RTCVideoRotation_0 = 0;
  RTCVideoRotation_90 = 90;
  RTCVideoRotation_180 = 180;
  RTCVideoRotation_270 = 270;

type

  //Subset of rtc::LoggingSeverity.
  //typedef NS_ENUM(NSInteger, RTCLoggingSeverity)
  RTCLoggingSeverity = NSInteger;

const

  RTCLoggingSeverityVerbose = 0;
  RTCLoggingSeverityInfo = 1;
  RTCLoggingSeverityWarning = 2;
  RTCLoggingSeverityError = 3;
  RTCLoggingSeverityNone = 4;

type

  //H264 Profiles and levels.
  //NS_ENUM(NSUInteger, RTCH264Profile)
  RTCH264Profile = NSUInteger;

const

  RTCH264ProfileConstrainedBaseline = 0;
  RTCH264ProfileBaseline = 1;
  RTCH264ProfileMain = 2;
  RTCH264ProfileConstrainedHigh = 3;
  RTCH264ProfileHigh = 4;

type

  //NS_ENUM(NSUInteger, RTCH264Level)
  RTCH264Level = NSUInteger;

const

  RTCH264Level1_b = 0;
  RTCH264Level1 = 10;
  RTCH264Level1_1 = 11;
  RTCH264Level1_2 = 12;
  RTCH264Level1_3 = 13;
  RTCH264Level2 = 20;
  RTCH264Level2_1 = 21;
  RTCH264Level2_2 = 22;
  RTCH264Level3 = 30;
  RTCH264Level3_1 = 31;
  RTCH264Level3_2 = 32;
  RTCH264Level4 = 40;
  RTCH264Level4_1 = 41;
  RTCH264Level4_2 = 42;
  RTCH264Level5 = 50;
  RTCH264Level5_1 = 51;
  RTCH264Level5_2 = 52;

type

  //Represents the chosen SDP semantics for the RTCPeerConnection.
  //NS_ENUM(NSInteger, RTCSdpSemantics)
  RTCSdpSemantics = NSInteger;

const

  RTCSdpSemanticsPlanB = 0;
  RTCSdpSemanticsUnifiedPlan = 1;

type

  //https://w3c.github.io/webrtc-pc/#dom-rtcrtptransceiverdirection
  //NS_ENUM(NSInteger, RTCRtpTransceiverDirection)
  RTCRtpTransceiverDirection = NSInteger;

const

  RTCRtpTransceiverDirectionSendRecv = 0;
  RTCRtpTransceiverDirectionSendOnly = 1;
  RTCRtpTransceiverDirectionRecvOnly = 2;
  RTCRtpTransceiverDirectionInactive = 3;

type

  //Represents the media type of the RtpReceiver.
  //NS_ENUM(NSInteger, RTCRtpMediaType)
  RTCRtpMediaType = NSInteger;

const

  RTCRtpMediaTypeAudio = 0;
  RTCRtpMediaTypeVideo = 1;
  RTCRtpMediaTypeData = 2;

type

  //Represents the session description type. This exposes the same types that are
  //in C++, which doesn't include the rollback type that is in the W3C spec.
  //NS_ENUM(NSInteger, RTCSdpType)
  RTCSdpType = NSInteger;

const

  RTCSdpTypeOffer = 0;
  RTCSdpTypePrAnswer = 1;
  RTCSdpTypeAnswer = 2;

type

  //typedef NS_OPTIONS(NSUInteger, AVAudioSessionCategoryOptions)
  AVAudioSessionCategoryOptions = NSUInteger;

const

	//MixWithOthers is only valid with AVAudioSessionCategoryPlayAndRecord, AVAudioSessionCategoryPlayback, and  AVAudioSessionCategoryMultiRoute
	AVAudioSessionCategoryOptionMixWithOthers = $1;
	//DuckOthers is only valid with AVAudioSessionCategoryAmbient, AVAudioSessionCategoryPlayAndRecord, AVAudioSessionCategoryPlayback, and AVAudioSessionCategoryMultiRoute
	AVAudioSessionCategoryOptionDuckOthers = $2;
	//AllowBluetooth is only valid with AVAudioSessionCategoryRecord and AVAudioSessionCategoryPlayAndRecord
	AVAudioSessionCategoryOptionAllowBluetooth = $4;
	//DefaultToSpeaker is only valid with AVAudioSessionCategoryPlayAndRecord
	AVAudioSessionCategoryOptionDefaultToSpeaker = $8;
	//InterruptSpokenAudioAndMixWithOthers is only valid with AVAudioSessionCategoryPlayAndRecord, AVAudioSessionCategoryPlayback, and AVAudioSessionCategoryMultiRoute
	AVAudioSessionCategoryOptionInterruptSpokenAudioAndMixWithOthers = $11;
	//AllowBluetoothA2DP is only valid with AVAudioSessionCategoryPlayAndRecord
	AVAudioSessionCategoryOptionAllowBluetoothA2DP = $20;
	//AllowAirPlay is only valid with AVAudioSessionCategoryPlayAndRecord
	AVAudioSessionCategoryOptionAllowAirPlay = $40;

//const
  //RTCH264PacketizationModeNonInterleaved = 0;
  //RTCH264PacketizationModeSingleNalUnit = 1;
  //RTCIceTransportPolicyNone = 0;
  //RTCIceTransportPolicyRelay = 1;
  //RTCIceTransportPolicyNoHost = 2;
  //RTCIceTransportPolicyAll = 3;
  //RTCBundlePolicyBalanced = 0;
  //RTCBundlePolicyMaxCompat = 1;
  //RTCBundlePolicyMaxBundle = 2;
  //RTCRtcpMuxPolicyNegotiate = 0;
  //RTCRtcpMuxPolicyRequire = 1;
  //RTCTcpCandidatePolicyEnabled = 0;
  //RTCTcpCandidatePolicyDisabled = 1;
  //RTCCandidateNetworkPolicyAll = 0;
  //RTCCandidateNetworkPolicyLowCost = 1;
  //RTCContinualGatheringPolicyGatherOnce = 0;
  //RTCContinualGatheringPolicyGatherContinually = 1;
  //RTCEncryptionKeyTypeRSA = 0;
  //RTCEncryptionKeyTypeECDSA = 1;
  //RTCDataChannelStateConnecting = 0;
  //RTCDataChannelStateOpen = 1;
  //RTCDataChannelStateClosing = 2;
  //RTCDataChannelStateClosed = 3;
  //RTCFrameTypeEmptyFrame = 0;
  //RTCFrameTypeAudioFrameSpeech = 1;
  //RTCFrameTypeAudioFrameCN = 2;
  //RTCFrameTypeVideoFrameKey = 3;
  //RTCFrameTypeVideoFrameDelta = 4;
  //RTCVideoContentTypeUnspecified = 0;
  //RTCVideoContentTypeScreenshare = 1;
  //RTCVideoCodecModeRealtimeVideo = 0;
  //RTCVideoCodecModeScreensharing = 1;
  //RTCDispatcherTypeMain = 0;
  //RTCDispatcherTypeCaptureSession = 1;
  //RTCDispatcherTypeAudioSession = 2;
  //RTCFileLoggerSeverityVerbose = 0;
  //RTCFileLoggerSeverityInfo = 1;
  //RTCFileLoggerSeverityWarning = 2;
  //RTCFileLoggerSeverityError = 3;
  //RTCFileLoggerTypeCall = 0;
  //RTCFileLoggerTypeApp = 1;
  //RTCTlsCertPolicySecure = 0;
  //RTCTlsCertPolicyInsecureNoCheck = 1;
  //RTCDeviceTypeUnknown = 0;
  //RTCDeviceTypeIPhone1G = 1;
  //RTCDeviceTypeIPhone3G = 2;
  //RTCDeviceTypeIPhone3GS = 3;
  //RTCDeviceTypeIPhone4 = 4;
  //RTCDeviceTypeIPhone4Verizon = 5;
  //RTCDeviceTypeIPhone4S = 6;
  //RTCDeviceTypeIPhone5GSM = 7;
  //RTCDeviceTypeIPhone5GSM_CDMA = 8;
  //RTCDeviceTypeIPhone5CGSM = 9;
  //RTCDeviceTypeIPhone5CGSM_CDMA = 10;
  //RTCDeviceTypeIPhone5SGSM = 11;
  //RTCDeviceTypeIPhone5SGSM_CDMA = 12;
  //RTCDeviceTypeIPhone6Plus = 13;
  //RTCDeviceTypeIPhone6 = 14;
  //RTCDeviceTypeIPhone6S = 15;
  //RTCDeviceTypeIPhone6SPlus = 16;
  //RTCDeviceTypeIPhone7 = 17;
  //RTCDeviceTypeIPhone7Plus = 18;
  //RTCDeviceTypeIPhoneSE = 19;
  //RTCDeviceTypeIPhone8 = 20;
  //RTCDeviceTypeIPhone8Plus = 21;
  //RTCDeviceTypeIPhoneX = 22;
  //RTCDeviceTypeIPhoneXS = 23;
  //RTCDeviceTypeIPhoneXSMax = 24;
  //RTCDeviceTypeIPhoneXR = 25;
  //RTCDeviceTypeIPodTouch1G = 26;
  //RTCDeviceTypeIPodTouch2G = 27;
  //RTCDeviceTypeIPodTouch3G = 28;
  //RTCDeviceTypeIPodTouch4G = 29;
  //RTCDeviceTypeIPodTouch5G = 30;
  //RTCDeviceTypeIPodTouch6G = 31;
  //RTCDeviceTypeIPad = 32;
  //RTCDeviceTypeIPad2Wifi = 33;
  //RTCDeviceTypeIPad2GSM = 34;
  //RTCDeviceTypeIPad2CDMA = 35;
  //RTCDeviceTypeIPad2Wifi2 = 36;
  //RTCDeviceTypeIPadMiniWifi = 37;
  //RTCDeviceTypeIPadMiniGSM = 38;
  //RTCDeviceTypeIPadMiniGSM_CDMA = 39;
  //RTCDeviceTypeIPad3Wifi = 40;
  //RTCDeviceTypeIPad3GSM_CDMA = 41;
  //RTCDeviceTypeIPad3GSM = 42;
  //RTCDeviceTypeIPad4Wifi = 43;
  //RTCDeviceTypeIPad4GSM = 44;
  //RTCDeviceTypeIPad4GSM_CDMA = 45;
  //RTCDeviceTypeIPad5 = 46;
  //RTCDeviceTypeIPad6 = 47;
  //RTCDeviceTypeIPadAirWifi = 48;
  //RTCDeviceTypeIPadAirCellular = 49;
  //RTCDeviceTypeIPadAirWifiCellular = 50;
  //RTCDeviceTypeIPadAir2 = 51;
  //RTCDeviceTypeIPadMini2GWifi = 52;
  //RTCDeviceTypeIPadMini2GCellular = 53;
  //RTCDeviceTypeIPadMini2GWifiCellular = 54;
  //RTCDeviceTypeIPadMini3 = 55;
  //RTCDeviceTypeIPadMini4 = 56;
  //RTCDeviceTypeIPadPro9Inch = 57;
  //RTCDeviceTypeIPadPro12Inch = 58;
  //RTCDeviceTypeIPadPro12Inch2 = 59;
  //RTCDeviceTypeIPadPro10Inch = 60;
  //RTCDeviceTypeSimulatori386 = 61;
  //RTCDeviceTypeSimulatorx86_64 = 62;

type

  RTCPeerConnection = interface;
  RTCVideoSource = interface;
  RTCVideoTrack = interface;
  RTCCertificate = interface;
  RTCSessionDescription = interface;
  RTCRtpParameters = interface;

  TWebRTCPeerConnectionOfferForConstraintsCompletionHandler = procedure(sdp: RTCSessionDescription; error: NSError) of object;
  TWebRTCPeerConnectionAnswerForConstraintsCompletionHandler = procedure(sdp: RTCSessionDescription; error: NSError) of object;
  TWebRTCPeerConnectionSetLocalDescriptionCompletionHandler = procedure(error: NSError) of object;
  TWebRTCPeerConnectionSetRemoteDescriptionCompletionHandler = procedure(error: NSError) of object;

  //NSInteger = Integer;
  //PNSInteger = ^NSInteger;

  //NSUInteger = Cardinal;
  //PNSUInteger = ^NSUInteger;

  //AVAudioSessionRouteChangeReason = NSUInteger;
  //NSTimeInterval = Double;
  //PNSTimeInterval = ^NSTimeInterval;

  //AVAudioSessionPortOverride = NSUInteger;
  //TWebRTCCallback = procedure(param1: NSString) of object;
  //CVBufferRef = Pointer;
  //PCVBufferRef = ^CVBufferRef;
  //CVImageBufferRef = CVBufferRef;
  //PCVImageBufferRef = ^CVImageBufferRef;
  //CVPixelBufferRef = CVImageBufferRef;
  //PCVPixelBufferRef = ^CVPixelBufferRef;
  //FourCharCode = UInt32;
  //PFourCharCode = ^FourCharCode;
  //TWebRTCCompletionHandler1 = procedure() of object;
  //RTCH264PacketizationMode = NSUInteger;
  //RTCIceTransportPolicy = NSInteger;
  //RTCBundlePolicy = NSInteger;
  //RTCRtcpMuxPolicy = NSInteger;
  //RTCTcpCandidatePolicy = NSInteger;
  //RTCCandidateNetworkPolicy = NSInteger;
  //RTCContinualGatheringPolicy = NSInteger;
  //RTCEncryptionKeyType = NSInteger;
  //RTCDataChannelState = NSInteger;
  //RTCFrameType = NSUInteger;
  //RTCVideoContentType = NSUInteger;
  //RTCVideoCodecMode = NSUInteger;
  //RTCVideoDecoderCallback = procedure(param1: RTCVideoFrame) of object;
  //RTCVideoEncoderCallback = function(param1: RTCEncodedImage; param2: Pointer; param3: RTCRtpFragmentationHeader): Boolean; cdecl;
  //RTCDispatcherQueueType = NSInteger;
  //CGFloat = Single;
  //PCGFloat = ^CGFloat;

  //CGSize = record
    //width: CGFloat;
    //height: CGFloat;
  //end;

  //PCGSize = ^CGSize;

  //GLuint = LongWord;
  //PGLuint = ^GLuint;

  //CGPoint = record
    //x: CGFloat;
    //y: CGFloat;
  //end;

  //PCGPoint = ^CGPoint;

  //CGRect = record
    //origin: CGPoint;
    //size: CGSize;
  //end;

  //PCGRect = ^CGRect;

  //__darwin_size_t = LongWord;
  //P__darwin_size_t = ^__darwin_size_t;

  //RTCFileLoggerSeverity = NSUInteger;
  //RTCFileLoggerRotationType = NSUInteger;
  //RTCFileVideoCapturerErrorBlock = procedure(param1: NSError) of object;
  //RTCTlsCertPolicy = NSUInteger;
  //CFTimeInterval = Double;
  //PCFTimeInterval = ^CFTimeInterval;

  //UIViewContentMode = NSInteger;
  //TWebRTCCompletionHandler3 = procedure(param1: NSArray) of object;
  //RTCDeviceType = NSInteger;

  //{*********************************************}
  //RTCAudioSessionClass = interface(NSObjectClass)
    //['{A6B6114F-8A68-4D27-B6DC-0D31A79AF1D9}']
    //{ class } function sharedInstance: Pointer { instancetype }; cdecl;
  //end;
  //RTCAudioSession = interface(NSObject)
    //['{72EA274E-32EC-4F26-A431-9209D20B8589}']
    //function session: AVAudioSession; cdecl;
    //function isActive: Boolean; cdecl;
    //function isLocked: Boolean; cdecl;
    //procedure setUseManualAudio(useManualAudio: Boolean); cdecl;
    //function useManualAudio: Boolean; cdecl;
    //procedure setIsAudioEnabled(isAudioEnabled: Boolean); cdecl;
    //function isAudioEnabled: Boolean; cdecl;
    //function category: NSString; cdecl;
    //function categoryOptions: AVAudioSessionCategoryOptions; cdecl;
    //function mode: NSString; cdecl;
    //function secondaryAudioShouldBeSilencedHint: Boolean; cdecl;
    //function currentRoute: AVAudioSessionRouteDescription; cdecl;
    //function maximumInputNumberOfChannels: NSInteger; cdecl;
    //function maximumOutputNumberOfChannels: NSInteger; cdecl;
    //function inputGain: Single; cdecl;
    //function inputGainSettable: Boolean; cdecl;
    //function inputAvailable: Boolean; cdecl;
    //function inputDataSources: NSArray; cdecl;
    //function inputDataSource: AVAudioSessionDataSourceDescription; cdecl;
    //function outputDataSources: NSArray; cdecl;
    //function outputDataSource: AVAudioSessionDataSourceDescription; cdecl;
    //function sampleRate: Double; cdecl;
    //function preferredSampleRate: Double; cdecl;
    //function inputNumberOfChannels: NSInteger; cdecl;
    //function outputNumberOfChannels: NSInteger; cdecl;
    //function outputVolume: Single; cdecl;
    //function inputLatency: NSTimeInterval; cdecl;
    //function outputLatency: NSTimeInterval; cdecl;
    //function IOBufferDuration: NSTimeInterval; cdecl;
    //function preferredIOBufferDuration: NSTimeInterval; cdecl;
    //procedure addDelegate(delegate: Pointer); cdecl;
    //procedure removeDelegate(delegate: Pointer); cdecl;
    //procedure lockForConfiguration; cdecl;
    //procedure unlockForConfiguration; cdecl;
    //function setActive(active: Boolean; error: NSError): Boolean; cdecl;
    //function setCategory(category: NSString; withOptions: AVAudioSessionCategoryOptions; error: NSError): Boolean; cdecl;
    //function setMode(mode: NSString; error: NSError): Boolean; cdecl;
    //function setInputGain(gain: Single; error: NSError): Boolean; cdecl;
    //function setPreferredSampleRate(sampleRate: Double; error: NSError): Boolean; cdecl;
    //function setPreferredIOBufferDuration(duration: NSTimeInterval; error: NSError): Boolean; cdecl;
    //function setPreferredInputNumberOfChannels(count: NSInteger; error: NSError): Boolean; cdecl;
    //function setPreferredOutputNumberOfChannels(count: NSInteger; error: NSError): Boolean; cdecl;
    //function overrideOutputAudioPort(portOverride: AVAudioSessionPortOverride; error: NSError): Boolean; cdecl;
    //function setPreferredInput(inPort: AVAudioSessionPortDescription; error: NSError): Boolean; cdecl;
    //function setInputDataSource(dataSource: AVAudioSessionDataSourceDescription; error: NSError): Boolean; cdecl;
    //function setOutputDataSource(dataSource: AVAudioSessionDataSourceDescription; error: NSError): Boolean; cdecl;
    //[MethodName('setConfiguration:error:')]
    //function setConfigurationError(configuration: RTCAudioSessionConfiguration; error: NSError): Boolean; cdecl;
    //[MethodName('setConfiguration:active:error:')]
    //function setConfigurationActiveError(configuration: RTCAudioSessionConfiguration; active: Boolean; error: NSError): Boolean; cdecl;
  //end;
  //TRTCAudioSession = class(TOCGenericImport<RTCAudioSessionClass, RTCAudioSession>) end;
  //PRTCAudioSession = Pointer;

  //{*************************************}
  RTCAudioSessionConfiguration = interface;

  //{************************************************}
  //@interface RTCAudioSessionConfiguration : NSObject
  RTCAudioSessionConfigurationClass = interface(NSObjectClass)
    ['{5391E964-2176-41A0-9362-E2C5D2230817}']

    //Returns the current configuration of the audio session.
    //+ (instancetype)currentConfiguration;
    //{ class } function currentConfiguration: Pointer { instancetype }; cdecl;

    //Returns the configuration that WebRTC needs.
    //+ (instancetype)webRTCConfiguration;
    { class } function webRTCConfiguration: Pointer { instancetype }; cdecl;

    //Provide a way to override the default configuration./
    //+ (void)setWebRTCConfiguration:(RTCAudioSessionConfiguration *)configuration;
    { class } procedure setWebRTCConfiguration(configuration: RTCAudioSessionConfiguration); cdecl;

  end;
  RTCAudioSessionConfiguration = interface(NSObject)
    ['{F0B3D9A6-B0B4-4CBD-9769-FF2FE4CA5F05}']

    //@property(nonatomic, strong) NSString *category;
    //procedure setCategory(category: NSString); cdecl;
    //function category: NSString; cdecl;

    //@property(nonatomic, assign) AVAudioSessionCategoryOptions categoryOptions;
    procedure setCategoryOptions(categoryOptions: AVAudioSessionCategoryOptions); cdecl;
    function categoryOptions: AVAudioSessionCategoryOptions; cdecl;

    //@property(nonatomic, strong) NSString *mode;
    //procedure setMode(mode: NSString); cdecl;
    //function mode: NSString; cdecl;

    //@property(nonatomic, assign) double sampleRate;
    //procedure setSampleRate(sampleRate: Double); cdecl;
    //function sampleRate: Double; cdecl;

    //@property(nonatomic, assign) NSTimeInterval ioBufferDuration;
    //procedure setIoBufferDuration(IOBufferDuration: NSTimeInterval); cdecl;
    //function IOBufferDuration: NSTimeInterval; cdecl;

    //@property(nonatomic, assign) NSInteger inputNumberOfChannels;
    //procedure setInputNumberOfChannels(inputNumberOfChannels: NSInteger); cdecl;
    //function inputNumberOfChannels: NSInteger; cdecl;

    //@property(nonatomic, assign) NSInteger outputNumberOfChannels;
    //procedure setOutputNumberOfChannels(outputNumberOfChannels: NSInteger); cdecl;
    //function outputNumberOfChannels: NSInteger; cdecl;

    //Initializes configuration to defaults.
    //- (instancetype)init NS_DESIGNATED_INITIALIZER;
    //function init: Pointer { instancetype }; cdecl;

  end;
  TRTCAudioSessionConfiguration = class(TOCGenericImport<RTCAudioSessionConfigurationClass, RTCAudioSessionConfiguration>) end;
  PRTCAudioSessionConfiguration = Pointer;

  {************************************}
  //@interface RTCMediaSource : NSObject
  RTCMediaSourceClass = interface(NSObjectClass)
    ['{BF16BBB0-F9DD-4483-AF2B-4503EC43D476}']
  end;
  RTCMediaSource = interface(NSObject)
    ['{77A69BA4-3968-4235-AF7B-7E5A224294B7}']

    //The current state of the RTCMediaSource.
    //@property(nonatomic, readonly) RTCSourceState state;
    function state: RTCSourceState; cdecl;

    //- (instancetype)init NS_UNAVAILABLE;

  end;
  TRTCMediaSource = class(TOCGenericImport<RTCMediaSourceClass, RTCMediaSource>) end;
  PRTCMediaSource = Pointer;

  {******************************************}
  //@interface RTCAudioSource : RTCMediaSource
  RTCAudioSourceClass = interface(RTCMediaSourceClass)
    ['{90AB4822-6838-4B2D-9BC8-A6798E51ADA5}']
  end;
  RTCAudioSource = interface(RTCMediaSource)
    ['{7018504B-82F1-4C7F-BAEE-141FEBDC296B}']

    //- (instancetype)init NS_UNAVAILABLE;

    //Sets the volume for the RTCMediaSource. |volume| is a gain value in the range [0, 10].
    //Temporary fix to be able to modify volume of remote audio tracks.
    //TODO(kthelgason): Property stays here temporarily until a proper volume-api
    //is available on the surface exposed by webrtc.
    //@property(nonatomic, assign) double volume;
    procedure setVolume(volume: Double); cdecl;
    function volume: Double; cdecl;

  end;
  TRTCAudioSource = class(TOCGenericImport<RTCAudioSourceClass, RTCAudioSource>) end;
  PRTCAudioSource = Pointer;

  {*****************************************}
  //@interface RTCMediaStreamTrack : NSObject
  RTCMediaStreamTrackClass = interface(NSObjectClass)
    ['{C08DFEAA-9074-49DC-B5B9-04A6BFB410C9}']
  end;
  RTCMediaStreamTrack = interface(NSObject)
    ['{12C83840-E22E-4DDF-A0C6-EA130FF79708}']

    //The kind of track. For example, "audio" if this track represents an audio
    //track and "video" if this track represents a video track.
    //@property(nonatomic, readonly) NSString *kind;
    function kind: NSString; cdecl;

    //An identifier string.
    //@property(nonatomic, readonly) NSString *trackId;
    //function trackId: NSString; cdecl;

    //The enabled state of the track.
    //@property(nonatomic, assign) BOOL isEnabled;
    procedure setIsEnabled(isEnabled: Boolean); cdecl;
    function isEnabled: Boolean; cdecl;

    //The state of the track.
    //@property(nonatomic, readonly) RTCMediaStreamTrackState readyState;
    //function readyState: RTCMediaStreamTrackState; cdecl;

    //- (instancetype)init NS_UNAVAILABLE;

  end;
  TRTCMediaStreamTrack = class(TOCGenericImport<RTCMediaStreamTrackClass, RTCMediaStreamTrack>) end;
  PRTCMediaStreamTrack = Pointer;

  {**********************************************}
  //@interface RTCAudioTrack : RTCMediaStreamTrack
  RTCAudioTrackClass = interface(RTCMediaStreamTrackClass)
    ['{4F246275-31AB-41F9-B091-18F051C7A486}']
  end;
  RTCAudioTrack = interface(RTCMediaStreamTrack)
    ['{A9828D51-5F7A-4F66-AAFC-EC173651A1DC}']

    //The audio source for this audio track.
    //@property(nonatomic, readonly) RTCAudioSource *source;
    //function source: RTCAudioSource; cdecl;

  end;
  TRTCAudioTrack = class(TOCGenericImport<RTCAudioTrackClass, RTCAudioTrack>) end;
  PRTCAudioTrack = Pointer;

  //{***********************************************}
  //RTCCallbackLoggerClass = interface(NSObjectClass)
    //['{D5096C46-8F94-4AAA-9A8A-06BA8576B3FD}']
  //end;
  //RTCCallbackLogger = interface(NSObject)
    //['{20D563D2-C274-42C1-943C-C0015D9A399A}']
    //procedure setSeverity(severity: RTCLoggingSeverity); cdecl;
    //function severity: RTCLoggingSeverity; cdecl;
    //procedure start(callback: TWebRTCCallback); cdecl;
    //procedure stop; cdecl;
  //end;
  //TRTCCallbackLogger = class(TOCGenericImport<RTCCallbackLoggerClass, RTCCallbackLogger>) end;
  //PRTCCallbackLogger = Pointer;

  //{************************************************}
  //RTCCameraPreviewViewClass = interface(UIViewClass)
    //['{F4BABECA-DE65-4237-ADF2-99628993E518}']
  //end;
  //RTCCameraPreviewView = interface(UIView)
    //['{BD3B6360-22DD-438B-877F-1687A37C7813}']
    //procedure setCaptureSession(captureSession: AVCaptureSession); cdecl;
    //function captureSession: AVCaptureSession; cdecl;
  //end;
  //TRTCCameraPreviewView = class(TOCGenericImport<RTCCameraPreviewViewClass, RTCCameraPreviewView>) end;
  //PRTCCameraPreviewView = Pointer;

  {***********************************}
  //@interface RTCVideoFrame : NSObject
  RTCVideoFrameClass = interface(NSObjectClass)
    ['{EFA1CAD4-76CA-4029-9AE4-FDF8185B63B8}']
  end;
  RTCVideoFrame = interface(NSObject)
    ['{BC992027-8BC1-4AFB-B839-E0F379070055}']

    //Width without rotation applied.
    //@property(nonatomic, readonly) int width;
    function width: Integer; cdecl;

    //Height without rotation applied.
    //@property(nonatomic, readonly) int height;
    function height: Integer; cdecl;

    //@property(nonatomic, readonly) RTCVideoRotation rotation;
    function rotation: RTCVideoRotation; cdecl;

    //Timestamp in nanoseconds.
    //@property(nonatomic, readonly) int64_t timeStampNs;
    function timeStampNs: Int64; cdecl;

    //Timestamp 90 kHz.
    //@property(nonatomic, assign) int32_t timeStamp;
    //procedure setTimeStamp(timeStamp: Int32); cdecl;
    function timeStamp: Int32; cdecl;

    //@property(nonatomic, readonly) id<RTCVideoFrameBuffer> buffer;
    function buffer: Pointer; cdecl;

    //- (instancetype)init NS_UNAVAILABLE;
    //- (instancetype) new NS_UNAVAILABLE;

    //Initialize an RTCVideoFrame from a pixel buffer, rotation, and timestamp.
    //Deprecated - initialize with a RTCCVPixelBuffer instead
    //- (instancetype)initWithPixelBuffer:(CVPixelBufferRef)pixelBuffer
    //                           rotation:(RTCVideoRotation)rotation
    //                        timeStampNs:(int64_t)timeStampNs
    //    DEPRECATED_MSG_ATTRIBUTE("use initWithBuffer instead");
    //[MethodName('initWithPixelBuffer:rotation:timeStampNs:')]
    //function initWithPixelBufferRotationTimeStampNs (pixelBuffer: CVPixelBufferRef; rotation: RTCVideoRotation; timeStampNs: Int64): Pointer { instancetype }; cdecl; deprecated;

    //Initialize an RTCVideoFrame from a pixel buffer combined with cropping and
    //scaling. Cropping will be applied first on the pixel buffer, followed by
    //scaling to the final resolution of scaledWidth x scaledHeight.
    //- (instancetype)initWithPixelBuffer:(CVPixelBufferRef)pixelBuffer
    //                        scaledWidth:(int)scaledWidth
    //                       scaledHeight:(int)scaledHeight
    //                          cropWidth:(int)cropWidth
    //                         cropHeight:(int)cropHeight
    //                              cropX:(int)cropX
    //                              cropY:(int)cropY
    //                           rotation:(RTCVideoRotation)rotation
    //                        timeStampNs:(int64_t)timeStampNs
    //    DEPRECATED_MSG_ATTRIBUTE("use initWithBuffer instead");
    //[MethodName ('initWithPixelBuffer:scaledWidth:scaledHeight:cropWidth:cropHeight:cropX:cropY:rotation:timeStampNs:') ]
    //function initWithPixelBufferScaledWidthScaledHeightCropWidthCropHeightCropXCropYRotationTimeStampNs (pixelBuffer: CVPixelBufferRef; scaledWidth: Integer; scaledHeight: Integer; cropWidth: Integer; cropHeight: Integer; cropX: Integer; cropY: Integer; rotation: RTCVideoRotation; timeStampNs: Int64): Pointer { instancetype }; cdecl; deprecated;

    //Initialize an RTCVideoFrame from a frame buffer, rotation, and timestamp.
    //- (instancetype)initWithBuffer:(id<RTCVideoFrameBuffer>)frameBuffer
    //                      rotation:(RTCVideoRotation)rotation
    //                   timeStampNs:(int64_t)timeStampNs;
    //function initWithBuffer(frameBuffer: Pointer; rotation: RTCVideoRotation; timeStampNs: Int64): Pointer { instancetype }; cdecl;

    //Return a frame that is guaranteed to be I420, i.e. it is possible to access
    //the YUV data on it.
    //- (RTCVideoFrame *)newI420VideoFrame;
    function newI420VideoFrame: RTCVideoFrame; cdecl;

  end;
  TRTCVideoFrame = class(TOCGenericImport<RTCVideoFrameClass, RTCVideoFrame>) end;
  PRTCVideoFrame = Pointer;

  {**************************************}
  //@interface RTCVideoCapturer : NSObject
  RTCVideoCapturerClass = interface(NSObjectClass)
    ['{A470FB28-9FFF-441E-97BB-C62B958F9C23}']
  end;
  RTCVideoCapturer = interface(NSObject)
    ['{C1C314C8-3EE1-4F48-9DCC-15969C3B3C11}']

    //@property(nonatomic, weak) id<RTCVideoCapturerDelegate> delegate;
    procedure setDelegate(delegate: Pointer); cdecl;
    function delegate: Pointer; cdecl;

    //- (instancetype)initWithDelegate:(id<RTCVideoCapturerDelegate>)delegate;
    function initWithDelegate(delegate: Pointer): Pointer { instancetype }; cdecl;

  end;
  TRTCVideoCapturer = class(TOCGenericImport<RTCVideoCapturerClass, RTCVideoCapturer>) end;
  PRTCVideoCapturer = Pointer;

  {****************************************************}
  //@interface RTCCameraVideoCapturer : RTCVideoCapturer
  RTCCameraVideoCapturerClass = interface(RTCVideoCapturerClass)
    ['{DCABFA66-0A5D-4881-B490-E31B73C73CD5}']

    //Returns list of available capture devices that support video capture.
    //+ (NSArray<AVCaptureDevice *> *)captureDevices;
    { class } function captureDevices: NSArray; cdecl;

    // Returns list of formats that are supported by this class for this device.
    //+ (NSArray<AVCaptureDeviceFormat *> *)supportedFormatsForDevice:(AVCaptureDevice *)device;
    { class } function supportedFormatsForDevice(device: AVCaptureDevice): NSArray; cdecl;

  end;
  RTCCameraVideoCapturer = interface(RTCVideoCapturer)
    ['{C8751868-E389-4BEB-AC6A-9F630E73BC58}']

    //Capture session that is used for capturing. Valid from initialization to dealloc.
    //@property(readonly, nonatomic) AVCaptureSession *captureSession;
    //function captureSession: AVCaptureSession; cdecl;

    //Returns the most efficient supported output pixel format for this capturer.
    //- (FourCharCode)preferredOutputPixelFormat;
    function preferredOutputPixelFormat: FourCharCode; cdecl;

    //Starts the capture session asynchronously and notifies callback on completion.
    //The device will capture video in the format given in the `format` parameter. If the pixel format
    //in `format` is supported by the WebRTC pipeline, the same pixel format will be used for the
    //output. Otherwise, the format returned by `preferredOutputPixelFormat` will be used.
    //- (void)startCaptureWithDevice:(AVCaptureDevice *)device
    //                        format:(AVCaptureDeviceFormat *)format
    //                           fps:(NSInteger)fps
    //             completionHandler:(nullable void (^)(NSError *))completionHandler;
    //[MethodName('startCaptureWithDevice:format:fps:completionHandler:')]
    //procedure startCaptureWithDeviceFormatFpsCompletionHandler (device: AVCaptureDevice; format: AVCaptureDeviceFormat; fps: NSInteger; completionHandler: TWebRTCCompletionHandler); cdecl;

    //Stops the capture session asynchronously and notifies callback on completion.
    //- (void)stopCaptureWithCompletionHandler:(nullable void (^)(void))completionHandler;
    //procedure stopCaptureWithCompletionHandler(completionHandler: TWebRTCCompletionHandler1); cdecl;

    //Starts the capture session asynchronously.
    //- (void)startCaptureWithDevice:(AVCaptureDevice *)device
    //                        format:(AVCaptureDeviceFormat *)format
    //                           fps:(NSInteger)fps;
    [MethodName('startCaptureWithDevice:format:fps:')]
    procedure startCaptureWithDeviceFormatFps(device: AVCaptureDevice; format: AVCaptureDeviceFormat; fps: NSInteger); cdecl;

    //Stops the capture session asynchronously.
    //- (void)stopCapture;
    procedure stopCapture; cdecl;

  end;
  TRTCCameraVideoCapturer = class(TOCGenericImport<RTCCameraVideoCapturerClass, RTCCameraVideoCapturer>) end;
  PRTCCameraVideoCapturer = Pointer;

  {************************************************}
  //@interface RTCCertificate : NSObject <NSCopying>
  RTCCertificateClass = interface(NSObjectClass)
    ['{C9AFAF4F-D1EB-4EA3-AE98-B00BF94AB5DC}']

    //Generate a new certificate for 're' use.
    //Optional dictionary of parameters. Defaults to KeyType ECDSA if none are
    //provided.
    //- name: "ECDSA" or "RSASSA-PKCS1-v1_5"
    //+ (nullable RTCCertificate *)generateCertificateWithParams:(NSDictionary *)params;
    { class } function generateCertificateWithParams(params: NSDictionary): RTCCertificate; cdecl;

  end;
  RTCCertificate = interface(NSObject)
    ['{3952697D-06D6-4E0B-BA49-CE026B8C41FD}']

    //Private key in PEM.
    //@property(nonatomic, readonly, copy) NSString *private_key;
    function private_key: NSString; cdecl;

    // Public key in an x509 cert encoded in PEM.
    //@property(nonatomic, readonly, copy) NSString *certificate;
    function certificate: NSString; cdecl;

    //Initialize an RTCCertificate with PEM strings for private_key and certificate.
    //- (instancetype)initWithPrivateKey:(NSString *)private_key
    //                       certificate:(NSString *)certificate NS_DESIGNATED_INITIALIZER;
    function initWithPrivateKey(private_key: NSString; certificate: NSString): Pointer { instancetype }; cdecl;

    //- (instancetype)init NS_UNAVAILABLE;

  end;
  TRTCCertificate = class(TOCGenericImport<RTCCertificateClass, RTCCertificate>) end;
  PRTCCertificate = Pointer;

  //{******************************************************}
  //RTCCodecSpecificInfoH264Class = interface(NSObjectClass)
    //['{96F3E93A-D2B8-401E-8016-612FA13D274B}']
  //end;
  //RTCCodecSpecificInfoH264 = interface(NSObject)
    //['{4502F845-EAA2-40B8-89D6-7793DFBEDB62}']
    //procedure setPacketizationMode(packetizationMode: RTCH264PacketizationMode); cdecl;
    //function packetizationMode: RTCH264PacketizationMode; cdecl;
  //end;
  //TRTCCodecSpecificInfoH264 = class (TOCGenericImport<RTCCodecSpecificInfoH264Class, RTCCodecSpecificInfoH264>) end;
  //PRTCCodecSpecificInfoH264 = Pointer;

  {**********************************}
  //@interface RTCIceServer : NSObject
  RTCIceServerClass = interface(NSObjectClass)
    ['{D15624FE-A7D9-4EFA-AD79-1251BE9107D3}']
  end;
  RTCIceServer = interface(NSObject)
    ['{B6AE16E6-34F2-4F29-A17D-E88F2E0397ED}']

    //URI(s) for this server represented as NSStrings.
    //@property(nonatomic, readonly) NSArray<NSString *> *urlStrings;
    function urlStrings: NSArray; cdecl;

    //Username to use if this RTCIceServer object is a TURN server.
    //@property(nonatomic, readonly, nullable) NSString *username;
    function username: NSString; cdecl;

    //Credential to use if this RTCIceServer object is a TURN server.
    //@property(nonatomic, readonly, nullable) NSString *credential;
    function credential: NSString; cdecl;

    //TLS certificate policy to use if this RTCIceServer object is a TURN server.
    //@property(nonatomic, readonly) RTCTlsCertPolicy tlsCertPolicy;
    //function tlsCertPolicy: RTCTlsCertPolicy; cdecl;

    //If the URIs in |urls| only contain IP addresses, this field can be used
    //to indicate the hostname, which may be necessary for TLS (using the SNI
    //extension). If |urls| itself contains the hostname, this isn't necessary.
    //@property(nonatomic, readonly, nullable) NSString *hostname;
    function hostname: NSString; cdecl;

    //List of protocols to be used in the TLS ALPN extension.
    //@property(nonatomic, readonly) NSArray<NSString *> *tlsAlpnProtocols;
    //function tlsAlpnProtocols: NSArray; cdecl;

    //List elliptic curves to be used in the TLS elliptic curves extension.
    //Only curve names supported by OpenSSL should be used (eg. "P-256","X25519").
    //@property(nonatomic, readonly) NSArray<NSString *> *tlsEllipticCurves;
    //function tlsEllipticCurves: NSArray; cdecl;

    //- (nonnull instancetype)init NS_UNAVAILABLE;

    //Convenience initializer for a server with no authentication (e.g. STUN).
    //- (instancetype)initWithURLStrings:(NSArray<NSString *> *)urlStrings;
    [MethodName('initWithURLStrings:')]
    function initWithURLStrings(urlStrings: NSArray): Pointer { instancetype }; cdecl;

    //Initialize an RTCIceServer with its associated URLs, optional username,
    //optional credential, and credentialType.
    //- (instancetype)initWithURLStrings:(NSArray<NSString *> *)urlStrings
    //                          username:(nullable NSString *)username
    //                        credential:(nullable NSString *)credential;
    [MethodName('initWithURLStrings:username:credential:')]
    function initWithURLStringsUsernameCredential(urlStrings: NSArray; username: NSString; credential: NSString): Pointer { instancetype }; cdecl;

    //Initialize an RTCIceServer with its associated URLs, optional username,
    //optional credential, and TLS cert policy.
    //- (instancetype)initWithURLStrings:(NSArray<NSString *> *)urlStrings
    //                          username:(nullable NSString *)username
    //                        credential:(nullable NSString *)credential
    //                     tlsCertPolicy:(RTCTlsCertPolicy)tlsCertPolicy;
    //[MethodName('initWithURLStrings:username:credential:tlsCertPolicy:')]
    //function initWithURLStringsUsernameCredentialTlsCertPolicy (urlStrings: NSArray; username: NSString; credential: NSString; tlsCertPolicy: RTCTlsCertPolicy): Pointer { instancetype }; cdecl;

    //Initialize an RTCIceServer with its associated URLs, optional username,
    //optional credential, TLS cert policy and hostname.
    //- (instancetype)initWithURLStrings:(NSArray<NSString *> *)urlStrings
    //                          username:(nullable NSString *)username
    //                        credential:(nullable NSString *)credential
    //                     tlsCertPolicy:(RTCTlsCertPolicy)tlsCertPolicy
    //                          hostname:(nullable NSString *)hostname;
    //[MethodName ('initWithURLStrings:username:credential:tlsCertPolicy:hostname:')]
    //function initWithURLStringsUsernameCredentialTlsCertPolicyHostname (urlStrings: NSArray; username: NSString; credential: NSString; tlsCertPolicy: RTCTlsCertPolicy; hostname: NSString): Pointer { instancetype }; cdecl;

    //Initialize an RTCIceServer with its associated URLs, optional username,
    //optional credential, TLS cert policy, hostname and ALPN protocols.
    //- (instancetype)initWithURLStrings:(NSArray<NSString *> *)urlStrings
    //                          username:(nullable NSString *)username
    //                        credential:(nullable NSString *)credential
    //                     tlsCertPolicy:(RTCTlsCertPolicy)tlsCertPolicy
    //                          hostname:(nullable NSString *)hostname
    //                  tlsAlpnProtocols:(NSArray<NSString *> *)tlsAlpnProtocols;
    //[MethodName ('initWithURLStrings:username:credential:tlsCertPolicy:hostname:tlsAlpnProtocols:') ]
    //function initWithURLStringsUsernameCredentialTlsCertPolicyHostnameTlsAlpnProtocols (urlStrings: NSArray; username: NSString; credential: NSString; tlsCertPolicy: RTCTlsCertPolicy; hostname: NSString; tlsAlpnProtocols: NSArray): Pointer { instancetype }; cdecl;

    //Initialize an RTCIceServer with its associated URLs, optional username,
    //optional credential, TLS cert policy, hostname, ALPN protocols and
    //elliptic curves.
    //- (instancetype)initWithURLStrings:(NSArray<NSString *> *)urlStrings
    //                          username:(nullable NSString *)username
    //                        credential:(nullable NSString *)credential
    //                     tlsCertPolicy:(RTCTlsCertPolicy)tlsCertPolicy
    //                          hostname:(nullable NSString *)hostname
    //                  tlsAlpnProtocols:(nullable NSArray<NSString *> *)tlsAlpnProtocols
    //                 tlsEllipticCurves:(nullable NSArray<NSString *> *)tlsEllipticCurves
    //    NS_DESIGNATED_INITIALIZER;
    //[MethodName ('initWithURLStrings:username:credential:tlsCertPolicy:hostname:tlsAlpnProtocols:tlsEllipticCurves:') ]
    //function initWithURLStringsUsernameCredentialTlsCertPolicyHostnameTlsAlpnProtocolsTlsEllipticCurves (urlStrings: NSArray; username: NSString; credential: NSString; tlsCertPolicy: RTCTlsCertPolicy; hostname: NSString; tlsAlpnProtocols: NSArray; tlsEllipticCurves: NSArray): Pointer { instancetype }; cdecl;

  end;
  TRTCIceServer = class(TOCGenericImport<RTCIceServerClass, RTCIceServer>) end;
  PRTCIceServer = Pointer;

  //{**********************************************}
  //RTCIntervalRangeClass = interface(NSObjectClass)
    //['{D9929EBD-D3EA-461A-B3D8-C6B564E7FAEB}']
  //end;
  //RTCIntervalRange = interface(NSObject)
    //['{68C6B303-BB26-43BB-89D0-1F0696FD94D2}']
    //function min: NSInteger; cdecl;
    //function max: NSInteger; cdecl;
    //function init: Pointer { instancetype }; cdecl;
    //function initWithMin(min: NSInteger; max: NSInteger): Pointer { instancetype }; cdecl;
  //end;
  //TRTCIntervalRange = class(TOCGenericImport<RTCIntervalRangeClass, RTCIntervalRange>) end;
  //PRTCIntervalRange = Pointer;

  {**************************************}
  //@interface RTCConfiguration : NSObject
  RTCConfigurationClass = interface(NSObjectClass)
    ['{B6FBFCDC-7338-4389-8449-F4F5107582BA}']
  end;
  RTCConfiguration = interface(NSObject)
    ['{D3B3564D-780E-497C-8E9C-FAEE23376E5C}']

    //An array of Ice Servers available to be used by ICE.
    //@property(nonatomic, copy) NSArray<RTCIceServer *> *iceServers;
    procedure setIceServers(iceServers: NSArray); cdecl;
    function iceServers: NSArray; cdecl;

    //An RTCCertificate for 're' use.
    //@property(nonatomic, nullable) RTCCertificate *certificate;
    procedure setCertificate(certificate: RTCCertificate); cdecl;
    function certificate: RTCCertificate; cdecl;

    //Which candidates the ICE agent is allowed to use. The W3C calls it
    //|iceTransportPolicy|, while in C++ it is called |type|.
    //@property(nonatomic, assign) RTCIceTransportPolicy iceTransportPolicy;
    //procedure setIceTransportPolicy(iceTransportPolicy: RTCIceTransportPolicy); cdecl;
    //function iceTransportPolicy: RTCIceTransportPolicy; cdecl;

    //The media-bundling policy to use when gathering ICE candidates.
    //@property(nonatomic, assign) RTCBundlePolicy bundlePolicy;
    //procedure setBundlePolicy(bundlePolicy: RTCBundlePolicy); cdecl;
    //function bundlePolicy: RTCBundlePolicy; cdecl;

    //The rtcp-mux policy to use when gathering ICE candidates.
    //@property(nonatomic, assign) RTCRtcpMuxPolicy rtcpMuxPolicy;
    //procedure setRtcpMuxPolicy(rtcpMuxPolicy: RTCRtcpMuxPolicy); cdecl;
    //function rtcpMuxPolicy: RTCRtcpMuxPolicy; cdecl;

    //@property(nonatomic, assign) RTCTcpCandidatePolicy tcpCandidatePolicy;
    //procedure setTcpCandidatePolicy(tcpCandidatePolicy: RTCTcpCandidatePolicy); cdecl;
    //function tcpCandidatePolicy: RTCTcpCandidatePolicy; cdecl;

    //@property(nonatomic, assign) RTCCandidateNetworkPolicy candidateNetworkPolicy;
    //procedure setCandidateNetworkPolicy(candidateNetworkPolicy: RTCCandidateNetworkPolicy); cdecl;
    //function candidateNetworkPolicy: RTCCandidateNetworkPolicy; cdecl;

    //@property(nonatomic, assign) RTCContinualGatheringPolicy continualGatheringPolicy;
    //procedure setContinualGatheringPolicy(continualGatheringPolicy: RTCContinualGatheringPolicy); cdecl;
    //function continualGatheringPolicy: RTCContinualGatheringPolicy; cdecl;

    //By default, the PeerConnection will use a limited number of IPv6 network
    //interfaces, in order to avoid too many ICE candidate pairs being created
    //and delaying ICE completion.
    //Can be set to INT_MAX to effectively disable the limit.
    //@property(nonatomic, assign) int maxIPv6Networks;
    //procedure setMaxIPv6Networks(maxIPv6Networks: Integer); cdecl;
    //function maxIPv6Networks: Integer; cdecl;

    //Exclude link-local network interfaces
    //from considertaion for gathering ICE candidates.
    //Defaults to NO.
    //@property(nonatomic, assign) BOOL disableLinkLocalNetworks;
    //procedure setDisableLinkLocalNetworks(disableLinkLocalNetworks: Boolean); cdecl;
    //function disableLinkLocalNetworks: Boolean; cdecl;

    //@property(nonatomic, assign) int audioJitterBufferMaxPackets;
    //procedure setAudioJitterBufferMaxPackets(audioJitterBufferMaxPackets: Integer); cdecl;
    //function audioJitterBufferMaxPackets: Integer; cdecl;

    //@property(nonatomic, assign) BOOL audioJitterBufferFastAccelerate;
    //procedure setAudioJitterBufferFastAccelerate(audioJitterBufferFastAccelerate: Boolean); cdecl;
    //function audioJitterBufferFastAccelerate: Boolean; cdecl;

    //@property(nonatomic, assign) int iceConnectionReceivingTimeout;
    //procedure setIceConnectionReceivingTimeout(iceConnectionReceivingTimeout: Integer); cdecl;
    //function iceConnectionReceivingTimeout: Integer; cdecl;

    //@property(nonatomic, assign) int iceBackupCandidatePairPingInterval;
    //procedure setIceBackupCandidatePairPingInterval (iceBackupCandidatePairPingInterval: Integer); cdecl;
    //function iceBackupCandidatePairPingInterval: Integer; cdecl;

    //Key type used to generate SSL identity. Default is ECDSA.
    //@property(nonatomic, assign) RTCEncryptionKeyType keyType;
    //procedure setKeyType(keyType: RTCEncryptionKeyType); cdecl;
    //function keyType: RTCEncryptionKeyType; cdecl;

    //ICE candidate pool size as defined in JSEP. Default is 0.
    //@property(nonatomic, assign) int iceCandidatePoolSize;
    //procedure setIceCandidatePoolSize(iceCandidatePoolSize: Integer); cdecl;
    //function iceCandidatePoolSize: Integer; cdecl;

    //Prune turn ports on the same network to the same turn server.
    //Default is NO.
    //@property(nonatomic, assign) BOOL shouldPruneTurnPorts;
    //procedure setShouldPruneTurnPorts(shouldPruneTurnPorts: Boolean); cdecl;
    //function shouldPruneTurnPorts: Boolean; cdecl;

    //If set to YES, this means the ICE transport should presume TURN-to-TURN
    //candidate pairs will succeed, even before a binding response is received.
    //@property(nonatomic, assign) BOOL shouldPresumeWritableWhenFullyRelayed;
    //procedure setShouldPresumeWritableWhenFullyRelayed (shouldPresumeWritableWhenFullyRelayed: Boolean); cdecl;
    //function shouldPresumeWritableWhenFullyRelayed: Boolean; cdecl;

    //If set to non-nil, controls the minimal interval between consecutive ICE
    //check packets.
    //@property(nonatomic, copy, nullable) NSNumber *iceCheckMinInterval;
    //procedure setIceCheckMinInterval(iceCheckMinInterval: NSNumber); cdecl;
    //function iceCheckMinInterval: NSNumber; cdecl;

    //ICE Periodic Regathering
    //If set, WebRTC will periodically create and propose candidates without
    //starting a new ICE generation. The regathering happens continuously with
    //interval specified in milliseconds by the uniform distribution [a, b].
    //@property(nonatomic, strong, nullable) RTCIntervalRange *iceRegatherIntervalRange;
    //procedure setIceRegatherIntervalRange(iceRegatherIntervalRange: RTCIntervalRange); cdecl;
    //function iceRegatherIntervalRange: RTCIntervalRange; cdecl;

    //Configure the SDP semantics used by this PeerConnection. Note that the
    //WebRTC 1.0 specification requires UnifiedPlan semantics. The
    //RTCRtpTransceiver API is only available with UnifiedPlan semantics.
    //
    //PlanB will cause RTCPeerConnection to create offers and answers with at
    //most one audio and one video m= section with multiple RTCRtpSenders and
    //RTCRtpReceivers specified as multiple a=ssrc lines within the section. This
    //will also cause RTCPeerConnection to ignore all but the first m= section of
    //the same media type.
    //
    //UnifiedPlan will cause RTCPeerConnection to create offers and answers with
    //multiple m= sections where each m= section maps to one RTCRtpSender and one
    //RTCRtpReceiver (an RTCRtpTransceiver), either both audio or both video. This
    //will also cause RTCPeerConnection to ignore all but the first a=ssrc lines
    //that form a Plan B stream.
    //
    //For users who wish to send multiple audio/video streams and need to stay
    //interoperable with legacy WebRTC implementations or use legacy APIs,
    //specify PlanB.
    //
    //For all other users, specify UnifiedPlan.
    //@property(nonatomic, assign) RTCSdpSemantics sdpSemantics;
    procedure setSdpSemantics(sdpSemantics: RTCSdpSemantics); cdecl;
    function sdpSemantics: RTCSdpSemantics; cdecl;

    //Actively reset the SRTP parameters when the DTLS transports underneath are
    //changed after offer/answer negotiation. This is only intended to be a
    //workaround for crbug.com/835958
    //@property(nonatomic, assign) BOOL activeResetSrtpParams;
    //procedure setActiveResetSrtpParams(activeResetSrtpParams: Boolean); cdecl;
    //function activeResetSrtpParams: Boolean; cdecl;

    //If MediaTransportFactory is provided in PeerConnectionFactory, this flag informs PeerConnection
    //that it should use the MediaTransportInterface.
    //@property(nonatomic, assign) BOOL useMediaTransport;
    //procedure setUseMediaTransport(useMediaTransport: Boolean); cdecl;
    //function useMediaTransport: Boolean; cdecl;

    //- (instancetype)init;
    //function init: Pointer { instancetype }; cdecl;

  end;
  TRTCConfiguration = class(TOCGenericImport<RTCConfigurationClass, RTCConfiguration>) end;
  PRTCConfiguration = Pointer;

  {************************************************************}
  //@interface RTCCVPixelBuffer : NSObject <RTCVideoFrameBuffer>
  RTCCVPixelBufferClass = interface(NSObjectClass)
    ['{AF44B741-EAD5-44A8-BB93-53A6303E5E7A}']

    //+ (NSSet<NSNumber *> *)supportedPixelFormats;
    { class } function supportedPixelFormats: NSSet; cdecl;

  end;
  RTCCVPixelBuffer = interface(NSObject)
    ['{82775AF1-460D-46D2-90D4-8FC2B1412A50}']

    //@property(nonatomic, readonly) CVPixelBufferRef pixelBuffer;
    function pixelBuffer: CVPixelBufferRef; cdecl;

    //@property(nonatomic, readonly) int cropX;
    function cropX: Integer; cdecl;

    //@property(nonatomic, readonly) int cropY;
    function cropY: Integer; cdecl;

    //@property(nonatomic, readonly) int cropWidth;
    function cropWidth: Integer; cdecl;

    //@property(nonatomic, readonly) int cropHeight;
    function cropHeight: Integer; cdecl;

    //- (instancetype)initWithPixelBuffer:(CVPixelBufferRef)pixelBuffer;
    //[MethodName('initWithPixelBuffer:')]
    //function initWithPixelBuffer(pixelBuffer: CVPixelBufferRef): Pointer { instancetype }; cdecl;

    //- (instancetype)initWithPixelBuffer:(CVPixelBufferRef)pixelBuffer
    //                       adaptedWidth:(int)adaptedWidth
    //                      adaptedHeight:(int)adaptedHeight
    //                          cropWidth:(int)cropWidth
    //                         cropHeight:(int)cropHeight
    //                              cropX:(int)cropX
    //                              cropY:(int)cropY;
    //[MethodName ('initWithPixelBuffer:adaptedWidth:adaptedHeight:cropWidth:cropHeight:cropX:cropY:') ]
    //function initWithPixelBufferAdaptedWidthAdaptedHeightCropWidthCropHeightCropXCropY (pixelBuffer: CVPixelBufferRef; adaptedWidth: Integer; adaptedHeight: Integer; cropWidth: Integer; cropHeight: Integer; cropX: Integer; cropY: Integer): Pointer { instancetype }; cdecl;

    //- (BOOL)requiresCropping;
    //function requiresCropping: Boolean; cdecl;

    //- (BOOL)requiresScalingToWidth:(int)width height:(int)height;
    //function requiresScalingToWidth(width: Integer; height: Integer): Boolean; cdecl;

    //- (int)bufferSizeForCroppingAndScalingToWidth:(int)width height:(int)height;
    //function bufferSizeForCroppingAndScalingToWidth(width: Integer; height: Integer): Integer; cdecl;

    //The minimum size of the |tmpBuffer| must be the number of bytes returned from the
    //bufferSizeForCroppingAndScalingToWidth:height: method.
    //If that size is 0, the |tmpBuffer| may be nil.
    //- (BOOL)cropAndScaleTo:(CVPixelBufferRef)outputPixelBuffer
    //        withTempBuffer:(nullable uint8_t *)tmpBuffer;
    //function cropAndScaleTo(outputPixelBuffer: CVPixelBufferRef; withTempBuffer: PByte): Boolean; cdecl;

  end;
  TRTCCVPixelBuffer = class(TOCGenericImport<RTCCVPixelBufferClass, RTCCVPixelBuffer>) end;
  PRTCCVPixelBuffer = Pointer;

  //{*******************************************}
  //RTCDataBufferClass = interface(NSObjectClass)
    //['{8B28D4C3-41A6-437C-92DE-68FC37CCF93F}']
  //end;
  //RTCDataBuffer = interface(NSObject)
    //['{94218B3E-1882-45BA-897A-BD136219374E}']
    //function data: NSData; cdecl;
    //function isBinary: Boolean; cdecl;
    //function initWithData(data: NSData; isBinary: Boolean): Pointer { instancetype }; cdecl;
  //end;
  //TRTCDataBuffer = class(TOCGenericImport<RTCDataBufferClass, RTCDataBuffer>) end;
  //PRTCDataBuffer = Pointer;

  {************************************}
  //@interface RTCDataChannel : NSObject
  RTCDataChannelClass = interface(NSObjectClass)
    ['{025C6769-3D1F-4FA4-90C2-CD10AD41AD33}']
  end;
  RTCDataChannel = interface(NSObject)
    ['{2FDBC0D3-56B7-4A43-A2CE-DAD670FB2940}']

    //A label that can be used to distinguish this data channel from other data
    //channel objects.
    //@property(nonatomic, readonly) NSString *label;
    function &label: NSString; cdecl;

    //Whether the data channel can send messages in unreliable mode.
    //@property(nonatomic, readonly) BOOL isReliable DEPRECATED_ATTRIBUTE;
    //function isReliable: Boolean; cdecl;

    //Returns whether this data channel is ordered or not.
    //@property(nonatomic, readonly) BOOL isOrdered;
    //function isOrdered: Boolean; cdecl;

    //Deprecated. Use maxPacketLifeTime.
    //@property(nonatomic, readonly) NSUInteger maxRetransmitTime DEPRECATED_ATTRIBUTE;
    //function maxRetransmitTime: NSUInteger; cdecl; deprecated;

    //The length of the time window (in milliseconds) during which transmissions
    //and retransmissions may occur in unreliable mode.
    //@property(nonatomic, readonly) uint16_t maxPacketLifeTime;
    //function maxPacketLifeTime: Word; cdecl;

    //The maximum number of retransmissions that are attempted in unreliable mode.
    //@property(nonatomic, readonly) uint16_t maxRetransmits;
    //function maxRetransmits: Word; cdecl;

    //The name of the sub-protocol used with this data channel, if any. Otherwise
    //this returns an empty string.
    //@property(nonatomic, readonly) NSString *protocol;
    //function protocol: NSString; cdecl;

    //Returns whether this data channel was negotiated by the application or not.
    //@property(nonatomic, readonly) BOOL isNegotiated;
    //function isNegotiated: Boolean; cdecl;

    //Deprecated. Use channelId.
    //@property(nonatomic, readonly) NSInteger streamId DEPRECATED_ATTRIBUTE;
    //function streamId: NSInteger; cdecl; deprecated;

    //The identifier for this data channel.
    //@property(nonatomic, readonly) int channelId;
    function channelId: Integer; cdecl;

    //The state of the data channel.
    //@property(nonatomic, readonly) RTCDataChannelState readyState;
    //function readyState: RTCDataChannelState; cdecl;

    //The number of bytes of application data that have been queued using
    //|sendData:| but that have not yet been transmitted to the network.
    //@property(nonatomic, readonly) uint64_t bufferedAmount;
    //function bufferedAmount: UInt64; cdecl;

    //The delegate for this data channel.
    //@property(nonatomic, weak) id<RTCDataChannelDelegate> delegate;
    //procedure setDelegate(delegate: Pointer); cdecl;
    //function delegate: Pointer; cdecl;

    //- (instancetype)init NS_UNAVAILABLE;

    //Closes the data channel.
    //- (void)close;
    //procedure close; cdecl;

    //Attempt to send |data| on this data channel's underlying data transport.
    //- (BOOL)sendData:(RTCDataBuffer *)data;
    //function sendData(data: RTCDataBuffer): Boolean; cdecl;

  end;
  TRTCDataChannel = class(TOCGenericImport<RTCDataChannelClass, RTCDataChannel>) end;
  PRTCDataChannel = Pointer;

  //{*********************************************************}
  //RTCDataChannelConfigurationClass = interface(NSObjectClass)
    //['{135B2907-3AF7-4050-ABEB-B3C05595693C}']
  //end;
  //RTCDataChannelConfiguration = interface(NSObject)
    //['{1F85DF5C-6E32-4B77-8959-9057060BE601}']
    //procedure setIsOrdered(isOrdered: Boolean); cdecl;
    //function isOrdered: Boolean; cdecl;
    //procedure setMaxRetransmitTimeMs(maxRetransmitTimeMs: NSInteger); cdecl;
    //function maxRetransmitTimeMs: NSInteger; cdecl;
    //procedure setMaxPacketLifeTime(maxPacketLifeTime: Integer); cdecl;
    //function maxPacketLifeTime: Integer; cdecl;
    //procedure setMaxRetransmits(maxRetransmits: Integer); cdecl;
    //function maxRetransmits: Integer; cdecl;
    //procedure setIsNegotiated(isNegotiated: Boolean); cdecl;
    //function isNegotiated: Boolean; cdecl;
    //procedure setStreamId(streamId: Integer); cdecl;
    //function streamId: Integer; cdecl;
    //procedure setChannelId(channelId: Integer); cdecl;
    //function channelId: Integer; cdecl;
    //procedure setProtocol(protocol: NSString); cdecl;
    //function protocol: NSString; cdecl;
  //end;
  //TRTCDataChannelConfiguration = class(TOCGenericImport<RTCDataChannelConfigurationClass, RTCDataChannelConfiguration>) end;
  //PRTCDataChannelConfiguration = Pointer;

  {*****************************************************************************}
  //Holds information to identify a codec. Corresponds to webrtc::SdpVideoFormat.
  //@interface RTCVideoCodecInfo : NSObject <NSCoding>
  RTCVideoCodecInfoClass = interface(NSObjectClass)
    ['{E9946732-49E4-43BE-B3B3-64FBA9779070}']
  end;
  RTCVideoCodecInfo = interface(NSObject)
    ['{1475B502-6FB2-4868-9624-CDC139211A85}']

    //- (instancetype)init NS_UNAVAILABLE;

    //- (instancetype)initWithName:(NSString *)name;
    [MethodName('initWithName:')]
    function initWithName(name: NSString): Pointer { instancetype }; cdecl;

    //- (instancetype)initWithName:(NSString *)name
    //                  parameters:(nullable NSDictionary<NSString *, NSString *> *)parameters
    //NS_DESIGNATED_INITIALIZER;
    [MethodName('initWithName:parameters:')]
    function initWithNameParameters(name: NSString; parameters: NSDictionary): Pointer { instancetype }; cdecl;

    //- (BOOL)isEqualToCodecInfo:(RTCVideoCodecInfo *)info;
    function isEqualToCodecInfo(info: RTCVideoCodecInfo): Boolean; cdecl;

    //@property(nonatomic, readonly) NSString *name;
    function name: NSString; cdecl;

    //@property(nonatomic, readonly) NSDictionary<NSString *, NSString *> *parameters;
    function parameters: NSDictionary; cdecl;

  end;
  TRTCVideoCodecInfo = class(TOCGenericImport<RTCVideoCodecInfoClass, RTCVideoCodecInfo>) end;
  PRTCVideoCodecInfo = Pointer;

  //{*********************************************}
  //RTCEncodedImageClass = interface(NSObjectClass)
    //['{D03D5F46-4AEA-4638-9C0C-D1F150622A8F}']
  //end;
  //RTCEncodedImage = interface(NSObject)
    //['{923A43FD-EFC0-4DD9-B200-3A4ACACA8E69}']
    //procedure setBuffer(buffer: NSData); cdecl;
    //function buffer: NSData; cdecl;
    //procedure setEncodedWidth(encodedWidth: Int32); cdecl;
    //function encodedWidth: Int32; cdecl;
    //procedure setEncodedHeight(encodedHeight: Int32); cdecl;
    //function encodedHeight: Int32; cdecl;
    //procedure setTimeStamp(timeStamp: LongWord); cdecl;
    //function timeStamp: LongWord; cdecl;
    //procedure setCaptureTimeMs(captureTimeMs: Int64); cdecl;
    //function captureTimeMs: Int64; cdecl;
    //procedure setNtpTimeMs(ntpTimeMs: Int64); cdecl;
    //function ntpTimeMs: Int64; cdecl;
    //procedure setFlags(flags: Byte); cdecl;
    //function flags: Byte; cdecl;
    //procedure setEncodeStartMs(encodeStartMs: Int64); cdecl;
    //function encodeStartMs: Int64; cdecl;
    //procedure setEncodeFinishMs(encodeFinishMs: Int64); cdecl;
    //function encodeFinishMs: Int64; cdecl;
    //procedure setFrameType(frameType: RTCFrameType); cdecl;
    //function frameType: RTCFrameType; cdecl;
    //procedure setRotation(rotation: RTCVideoRotation); cdecl;
    //function rotation: RTCVideoRotation; cdecl;
    //procedure setCompleteFrame(completeFrame: Boolean); cdecl;
    //function completeFrame: Boolean; cdecl;
    //procedure setQp(qp: NSNumber); cdecl;
    //function qp: NSNumber; cdecl;
    //procedure setContentType(contentType: RTCVideoContentType); cdecl;
    //function contentType: RTCVideoContentType; cdecl;
  //end;
  //TRTCEncodedImage = class(TOCGenericImport<RTCEncodedImageClass, RTCEncodedImage>) end;
  //PRTCEncodedImage = Pointer;

  //{*****************************************************}
  //RTCVideoEncoderSettingsClass = interface(NSObjectClass)
    //['{0CE09797-07F1-4609-A56B-2A0C412037CD}']
  //end;
  //RTCVideoEncoderSettings = interface(NSObject)
    //['{2BA97AC8-2DAB-4E0F-86D4-8FDF1166F156}']
    //procedure setName(name: NSString); cdecl;
    //function name: NSString; cdecl;
    //procedure setWidth(width: Word); cdecl;
    //function width: Word; cdecl;
    //procedure setHeight(height: Word); cdecl;
    //function height: Word; cdecl;
    //procedure setStartBitrate(startBitrate: Cardinal); cdecl;
    //function startBitrate: Cardinal; cdecl;
    //procedure setMaxBitrate(maxBitrate: Cardinal); cdecl;
    //function maxBitrate: Cardinal; cdecl;
    //procedure setMinBitrate(minBitrate: Cardinal); cdecl;
    //function minBitrate: Cardinal; cdecl;
    //procedure setTargetBitrate(targetBitrate: Cardinal); cdecl;
    //function targetBitrate: Cardinal; cdecl;
    //procedure setMaxFramerate(maxFramerate: LongWord); cdecl;
    //function maxFramerate: LongWord; cdecl;
    //procedure setQpMax(qpMax: Cardinal); cdecl;
    //function qpMax: Cardinal; cdecl;
    //procedure setMode(mode: RTCVideoCodecMode); cdecl;
    //function mode: RTCVideoCodecMode; cdecl;
  //end;
  //TRTCVideoEncoderSettings = class(TOCGenericImport<RTCVideoEncoderSettingsClass, RTCVideoEncoderSettings>) end;
  //PRTCVideoEncoderSettings = Pointer;

  {****************************************************************************************}
  //This decoder factory include support for all codecs bundled with WebRTC. If using custom
  //codecs, create custom implementations of RTCVideoEncoderFactory and RTCVideoDecoderFactory.
  //@interface RTCDefaultVideoDecoderFactory : NSObject <RTCVideoDecoderFactory>
  RTCDefaultVideoDecoderFactoryClass = interface(NSObjectClass)
    ['{F72D68FF-8EC7-47AE-B260-37CF79B19418}']
  end;
  RTCDefaultVideoDecoderFactory = interface(NSObject)
    ['{6BD5BA9E-A505-460C-BBEF-2841DF55C7D3}']
  end;
  TRTCDefaultVideoDecoderFactory = class(TOCGenericImport<RTCDefaultVideoDecoderFactoryClass, RTCDefaultVideoDecoderFactory>) end;
  PRTCDefaultVideoDecoderFactory = Pointer;

  //{*******************************************************}
  //RTCRtpFragmentationHeaderClass = interface(NSObjectClass)
    //['{1DB1DFAD-ACBE-4F4C-95AE-D592774CB766}']
  //end;
  //RTCRtpFragmentationHeader = interface(NSObject)
    //['{72DC1468-0712-4DED-8CB0-955DA0E21CD8}']
    //procedure setFragmentationOffset(fragmentationOffset: NSArray); cdecl;
    //function fragmentationOffset: NSArray; cdecl;
    //procedure setFragmentationLength(fragmentationLength: NSArray); cdecl;
    //function fragmentationLength: NSArray; cdecl;
    //procedure setFragmentationTimeDiff(fragmentationTimeDiff: NSArray); cdecl;
    //function fragmentationTimeDiff: NSArray; cdecl;
    //procedure setFragmentationPlType(fragmentationPlType: NSArray); cdecl;
    //function fragmentationPlType: NSArray; cdecl;
  //end;
  //TRTCRtpFragmentationHeader = class(TOCGenericImport<RTCRtpFragmentationHeaderClass, RTCRtpFragmentationHeader>) end;
  //PRTCRtpFragmentationHeader = Pointer;

  //{*********************************************************}
  //RTCVideoEncoderQpThresholdsClass = interface(NSObjectClass)
    //['{0B602B2D-CAD2-47BC-BDD6-90686D731BF1}']
  //end;
  //RTCVideoEncoderQpThresholds = interface(NSObject)
    //['{A5687575-ABC5-44F9-9808-DA92EA96C774}']
    //function initWithThresholdsLow(low: NSInteger; high: NSInteger): Pointer { instancetype }; cdecl;
    //function low: NSInteger; cdecl;
    //function high: NSInteger; cdecl;
  //end;
  //TRTCVideoEncoderQpThresholds = class(TOCGenericImport<RTCVideoEncoderQpThresholdsClass, RTCVideoEncoderQpThresholds>) end;
  //PRTCVideoEncoderQpThresholds = Pointer;

  {****************************************************************************************}
  //This encoder factory include support for all codecs bundled with WebRTC. If using custom
  //codecs, create custom implementations of RTCVideoEncoderFactory and RTCVideoDecoderFactory.
  //@interface RTCDefaultVideoEncoderFactory : NSObject <RTCVideoEncoderFactory>
  RTCDefaultVideoEncoderFactoryClass = interface(NSObjectClass)
    ['{06A55D95-5B90-4E2B-98F6-E03F35C746C3}']

    //+ (NSArray<RTCVideoCodecInfo *> *)supportedCodecs;
    { class } function supportedCodecs: NSArray; cdecl;

  end;
  RTCDefaultVideoEncoderFactory = interface(NSObject)
    ['{1205BC8D-3182-42AD-BB6A-02E06E17A866}']

    //@property(nonatomic, retain) RTCVideoCodecInfo *preferredCodec;
    procedure setPreferredCodec(preferredCodec: RTCVideoCodecInfo); cdecl;
    function preferredCodec: RTCVideoCodecInfo; cdecl;

  end;
  TRTCDefaultVideoEncoderFactory = class(TOCGenericImport<RTCDefaultVideoEncoderFactoryClass, RTCDefaultVideoEncoderFactory>) end;
  PRTCDefaultVideoEncoderFactory = Pointer;

  //{*******************************************}
  //RTCDispatcherClass = interface(NSObjectClass)
    //['{BE18E651-39AE-49CC-B161-0F83CE99E90E}']
    //{ class } procedure dispatchAsyncOnType(dispatchType: RTCDispatcherQueueType; block: Pointer { dispatch_block_t } ); cdecl;
    //{ class } function isOnQueueForType(dispatchType: RTCDispatcherQueueType): Boolean; cdecl;
  //end;
  //RTCDispatcher = interface(NSObject)
    //['{53C8DE2D-B117-4265-92F4-5FE9BCE7FA76}']
  //end;
  //TRTCDispatcher = class(TOCGenericImport<RTCDispatcherClass, RTCDispatcher>) end;
  //PRTCDispatcher = Pointer;

  //{********************************************}
  //RTCEAGLVideoViewClass = interface(UIViewClass)
    //['{F498FC75-6552-49DD-8E8A-1F98B77DA8B7}']
  //end;
  //RTCEAGLVideoView = interface(UIView)
    //['{CE927B4A-A238-4CCB-B371-A88236D87761}']
    //procedure setDelegate(delegate: Pointer); cdecl;
    //function delegate: Pointer; cdecl;
    //function initWithFrame(frame: CGRect; shader: Pointer): Pointer { instancetype }; cdecl;
    //function initWithCoder(aDecoder: NSCoder; shader: Pointer): Pointer { instancetype }; cdecl;
  //end;
  //TRTCEAGLVideoView = class(TOCGenericImport<RTCEAGLVideoViewClass, RTCEAGLVideoView>) end;
  //PRTCEAGLVideoView = Pointer;

  //{*******************************************}
  //RTCFileLoggerClass = interface(NSObjectClass)
    //['{8CFD272C-B1A1-4595-AE58-F2CCFA45DEA0}']
  //end;
  //RTCFileLogger = interface(NSObject)
    //['{D5706349-63B1-4055-B178-25D937340C7E}']
    //procedure setSeverity(severity: RTCFileLoggerSeverity); cdecl;
    //function severity: RTCFileLoggerSeverity; cdecl;
    //function rotationType: RTCFileLoggerRotationType; cdecl;
    //procedure setShouldDisableBuffering(shouldDisableBuffering: Boolean); cdecl;
    //function shouldDisableBuffering: Boolean; cdecl;
    //function init: Pointer { instancetype }; cdecl;
    //[MethodName('initWithDirPath:maxFileSize:')]
    //function initWithDirPathMaxFileSize(dirPath: NSString; maxFileSize: NSUInteger): Pointer { instancetype }; cdecl;
    //[MethodName('initWithDirPath:maxFileSize:rotationType:')]
    //function initWithDirPathMaxFileSizeRotationType(dirPath: NSString; maxFileSize: NSUInteger; rotationType: RTCFileLoggerRotationType): Pointer { instancetype }; cdecl;
    //procedure start; cdecl;
    //procedure stop; cdecl;
    //function logData: NSData; cdecl;
  //end;
  //TRTCFileLogger = class(TOCGenericImport<RTCFileLoggerClass, RTCFileLogger>) end;
  //PRTCFileLogger = Pointer;

  //{**********************************************************}
  //RTCFileVideoCapturerClass = interface(RTCVideoCapturerClass)
    //['{B039F9AD-EB1C-45C0-97DD-9F25828B6420}']
  //end;
  //RTCFileVideoCapturer = interface(RTCVideoCapturer)
    //['{61A9BD01-16F2-4244-B1A0-0D7894DA5E95}']
    //procedure startCapturingFromFileNamed(nameOfFile: NSString; onError: RTCFileVideoCapturerErrorBlock); cdecl;
    //procedure stopCapture; cdecl;
  //end;
  //TRTCFileVideoCapturer = class(TOCGenericImport<RTCFileVideoCapturerClass, RTCFileVideoCapturer>) end;
  //PRTCFileVideoCapturer = Pointer;

  {*******************************************}
  //@interface RTCH264ProfileLevelId : NSObject
  RTCH264ProfileLevelIdClass = interface(NSObjectClass)
    ['{4967D0E3-E801-4551-9529-D81B051D4075}']
  end;
  RTCH264ProfileLevelId = interface(NSObject)
    ['{4961CCA6-548C-44A1-BC10-3D9884820149}']

    //@property(nonatomic, readonly) RTCH264Profile profile;
    function profile: RTCH264Profile; cdecl;

    //@property(nonatomic, readonly) RTCH264Level level;
    function level: RTCH264Level; cdecl;

    //@property(nonatomic, readonly) NSString *hexString;
    function hexString: NSString; cdecl;

    //- (instancetype)initWithHexString:(NSString *)hexString;
    function initWithHexString(hexString: NSString): Pointer { instancetype }; cdecl;

    //- (instancetype)initWithProfile:(RTCH264Profile)profile level:(RTCH264Level)level;
    function initWithProfile(profile: RTCH264Profile; level: RTCH264Level): Pointer { instancetype }; cdecl;

  end;
  TRTCH264ProfileLevelId = class(TOCGenericImport<RTCH264ProfileLevelIdClass, RTCH264ProfileLevelId>) end;
  PRTCH264ProfileLevelId = Pointer;

  {*************************************}
  //@interface RTCIceCandidate : NSObject
  RTCIceCandidateClass = interface(NSObjectClass)
    ['{A096C28C-D525-4E4C-8875-5235481CDAB9}']
  end;
  RTCIceCandidate = interface(NSObject)
    ['{029E5282-6864-4B38-B93A-9449CDF82EF7}']

    //If present, the identifier of the "media stream identification" for the media
    //component this candidate is associated with.
    //@property(nonatomic, readonly, nullable) NSString *sdpMid;
    function sdpMid: NSString; cdecl;

    //The index (starting at zero) of the media description this candidate is
    //associated with in the SDP.
    //@property(nonatomic, readonly) int sdpMLineIndex;
    function sdpMLineIndex: Integer; cdecl;

    //The SDP string for this candidate.
    //@property(nonatomic, readonly) NSString *sdp;
    function sdp: NSString; cdecl;

    //The URL of the ICE server which this candidate is gathered from.
    //@property(nonatomic, readonly, nullable) NSString *serverUrl;
    function serverUrl: NSString; cdecl;

    //- (instancetype)init NS_UNAVAILABLE;

    //Initialize an RTCIceCandidate from SDP.
    //- (instancetype)initWithSdp:(NSString *)sdp
    //              sdpMLineIndex:(int)sdpMLineIndex
    //                     sdpMid:(nullable NSString *)sdpMid NS_DESIGNATED_INITIALIZER;
    function initWithSdp(sdp: NSString; sdpMLineIndex: Integer; sdpMid: NSString): Pointer { instancetype }; cdecl;

  end;
  TRTCIceCandidate = class(TOCGenericImport<RTCIceCandidateClass, RTCIceCandidate>) end;
  PRTCIceCandidate = Pointer;

  //{**************************************************}
  //RTCLegacyStatsReportClass = interface(NSObjectClass)
    //['{6CDFB5E6-6BBC-410E-B34E-7AE4030A3BD7}']
  //end;
  //RTCLegacyStatsReport = interface(NSObject)
    //['{173B5644-F310-4338-A0A3-13779F046F24}']
    //function timeStamp: CFTimeInterval; cdecl;
    //function &type: NSString; cdecl;
    //function reportId: NSString; cdecl;
    //function values: NSDictionary; cdecl;
  //end;
  //TRTCLegacyStatsReport = class(TOCGenericImport<RTCLegacyStatsReportClass, RTCLegacyStatsReport>) end;
  //PRTCLegacyStatsReport = Pointer;

  {*****************************************}
  //@interface RTCMediaConstraints : NSObject
  RTCMediaConstraintsClass = interface(NSObjectClass)
    ['{8235D2CB-7359-4F52-9F78-7BF285C1BE26}']
  end;
  RTCMediaConstraints = interface(NSObject)
    ['{99D7E0CC-E384-4C0E-9B41-B6E3411568D8}']

    //- (instancetype)init NS_UNAVAILABLE;

    //Initialize with mandatory and/or optional constraints.
    //- (instancetype)
    //    initWithMandatoryConstraints:(nullable NSDictionary<NSString *, NSString *> *)mandatory
    //             optionalConstraints:(nullable NSDictionary<NSString *, NSString *> *)optional
    //    NS_DESIGNATED_INITIALIZER;
    function initWithMandatoryConstraints(mandatory: NSDictionary; optionalConstraints: NSDictionary): Pointer { instancetype }; cdecl;

  end;
  TRTCMediaConstraints = class(TOCGenericImport<RTCMediaConstraintsClass, RTCMediaConstraints>) end;
  PRTCMediaConstraints = Pointer;

  {**********************************************}
  //@interface RTCPeerConnectionFactory : NSObject
  RTCPeerConnectionFactoryClass = interface(NSObjectClass)
    ['{60AF5637-A719-4C21-A04A-FE89DA61E82B}']
  end;
  RTCPeerConnectionFactory = interface(NSObject)
    ['{AC489494-5259-4C88-81A9-6AA20A16E36A}']

    //Initialize object with default H264 video encoder/decoder factories
    //- (instancetype)init;
    function init: Pointer { instancetype }; cdecl;

    //Initialize object with injectable video encoder/decoder factories
    //- (instancetype)initWithEncoderFactory:(nullable id<RTCVideoEncoderFactory>)encoderFactory
    //                        decoderFactory:(nullable id<RTCVideoDecoderFactory>)decoderFactory;
    function initWithEncoderFactory(encoderFactory: Pointer; decoderFactory: Pointer): Pointer { instancetype }; cdecl;

    //Initialize an RTCAudioSource with constraints.
    //- (RTCAudioSource *)audioSourceWithConstraints:(nullable RTCMediaConstraints *)constraints;
    function audioSourceWithConstraints(constraints: RTCMediaConstraints): RTCAudioSource; cdecl;

    //Initialize an RTCAudioTrack with an id. Convenience ctor to use an audio source with no constraints.
    //- (RTCAudioTrack *)audioTrackWithTrackId:(NSString *)trackId;
    //function audioTrackWithTrackId(trackId: NSString): RTCAudioTrack; cdecl;

    //Initialize an RTCAudioTrack with a source and an id.
    //- (RTCAudioTrack *)audioTrackWithSource:(RTCAudioSource *)source trackId:(NSString *)trackId;
    function audioTrackWithSource(source: RTCAudioSource; trackId: NSString): RTCAudioTrack; cdecl;

    //Initialize a generic RTCVideoSource. The RTCVideoSource should be passed to a RTCVideoCapturer
    //implementation, e.g. RTCCameraVideoCapturer, in order to produce frames.
    //- (RTCVideoSource *)videoSource;
    function videoSource: RTCVideoSource; cdecl;

    //Initialize an RTCVideoTrack with a source and an id.
    //- (RTCVideoTrack *)videoTrackWithSource:(RTCVideoSource *)source trackId:(NSString *)trackId;
    function videoTrackWithSource(source: RTCVideoSource; trackId: NSString): RTCVideoTrack; cdecl;

    //Initialize an RTCMediaStream with an id.
    //- (RTCMediaStream *)mediaStreamWithStreamId:(NSString *)streamId;
    //function mediaStreamWithStreamId(streamId: NSString): RTCMediaStream; cdecl;

    //Initialize an RTCPeerConnection with a configuration, constraints, and delegate.
    //- (RTCPeerConnection *)peerConnectionWithConfiguration:(RTCConfiguration *)configuration
    //                                           constraints:(RTCMediaConstraints *)constraints
    //                                              delegate:(nullable id<RTCPeerConnectionDelegate>)delegate;
    function peerConnectionWithConfiguration(configuration: RTCConfiguration; constraints: RTCMediaConstraints; delegate: Pointer): RTCPeerConnection; cdecl;

    //Set the options to be used for subsequently created RTCPeerConnections
    //- (void)setOptions:(nonnull RTCPeerConnectionFactoryOptions *)options;
    //procedure setOptions(options: RTCPeerConnectionFactoryOptions); cdecl;

    //Start an AecDump recording. This API call will likely change in the future.
    //- (BOOL)startAecDumpWithFilePath:(NSString *)filePath maxSizeInBytes:(int64_t)maxSizeInBytes;
    //function startAecDumpWithFilePath(filePath: NSString; maxSizeInBytes: Int64): Boolean; cdecl;

    //Stop an active AecDump recording
    //- (void)stopAecDump;
    //procedure stopAecDump; cdecl;

  end;
  TRTCPeerConnectionFactory = class(TOCGenericImport<RTCPeerConnectionFactoryClass, RTCPeerConnectionFactory>) end;
  PRTCPeerConnectionFactory = Pointer;

  {**********************************************}
  //@interface RTCVideoTrack : RTCMediaStreamTrack
  RTCVideoTrackClass = interface(RTCMediaStreamTrackClass)
    ['{42239C62-26A1-46EA-87EC-B897743D22A4}']
  end;
  RTCVideoTrack = interface(RTCMediaStreamTrack)
    ['{E87017EA-5159-4770-BC33-BFBE9BD5AE86}']

    //The video source for this video track.
    //@property(nonatomic, readonly) RTCVideoSource *source;
    function source: RTCVideoSource; cdecl;

    //- (instancetype)init NS_UNAVAILABLE;

    //Register a renderer that will render all frames received on this track.
    //- (void)addRenderer:(id<RTCVideoRenderer>)renderer;
    procedure addRenderer(renderer: Pointer); cdecl;

    //Deregister a renderer.
    //- (void)removeRenderer:(id<RTCVideoRenderer>)renderer;
    procedure removeRenderer(renderer: Pointer); cdecl;

  end;
  TRTCVideoTrack = class(TOCGenericImport<RTCVideoTrackClass, RTCVideoTrack>) end;
  PRTCVideoTrack = Pointer;

  {************************************}
  //@interface RTCMediaStream : NSObject
  RTCMediaStreamClass = interface(NSObjectClass)
    ['{DD661051-CD33-460C-A285-BDE3C672670D}']
  end;
  RTCMediaStream = interface(NSObject)
    ['{89DE6B02-4016-4CF2-9164-59C006DF9B4A}']

    //The audio tracks in this stream.
    //@property(nonatomic, strong, readonly) NSArray<RTCAudioTrack *> *audioTracks;
    //function audioTracks: NSArray; cdecl;

    //The video tracks in this stream.
    //@property(nonatomic, strong, readonly) NSArray<RTCVideoTrack *> *videoTracks;
    //function videoTracks: NSArray; cdecl;

    //An identifier for this media stream.
    //@property(nonatomic, readonly) NSString *streamId;
    function streamId: NSString; cdecl;

    //- (instancetype)init NS_UNAVAILABLE;

    //Adds the given audio track to this media stream.
    //- (void)addAudioTrack:(RTCAudioTrack *)audioTrack;
    //procedure addAudioTrack(audioTrack: RTCAudioTrack); cdecl;

    //Adds the given video track to this media stream.
    //- (void)addVideoTrack:(RTCVideoTrack *)videoTrack;
    //procedure addVideoTrack(videoTrack: RTCVideoTrack); cdecl;

    //Removes the given audio track to this media stream.
    //- (void)removeAudioTrack:(RTCAudioTrack *)audioTrack;
    //procedure removeAudioTrack(audioTrack: RTCAudioTrack); cdecl;

    //Removes the given video track to this media stream.
    //- (void)removeVideoTrack:(RTCVideoTrack *)videoTrack;
    //procedure removeVideoTrack(videoTrack: RTCVideoTrack); cdecl;

  end;
  TRTCMediaStream = class(TOCGenericImport<RTCMediaStreamClass, RTCMediaStream>) end;
  PRTCMediaStream = Pointer;

  //{**************************************************}
  //RTCMetricsSampleInfoClass = interface(NSObjectClass)
    //['{3299719D-AFF9-4EC0-B86F-9DB3CB1334C1}']
  //end;
  //RTCMetricsSampleInfo = interface(NSObject)
    //['{A909A86F-F4C3-42DF-8B46-E7479A6A674D}']
    //function name: NSString; cdecl;
    //function min: Integer; cdecl;
    //function max: Integer; cdecl;
    //function bucketCount: Integer; cdecl;
    //function samples: NSDictionary; cdecl;
  //end;
  //TRTCMetricsSampleInfo = class(TOCGenericImport<RTCMetricsSampleInfoClass, RTCMetricsSampleInfo>) end;
  //PRTCMetricsSampleInfo = Pointer;

  //{*******************************************}
  //RTCMTLVideoViewClass = interface(UIViewClass)
    //['{BA32C167-DB82-4A95-87AD-14D09A9D54AC}']
  //end;
  //RTCMTLVideoView = interface(UIView)
    //['{5FA2C7EE-0A9E-470F-B434-9C8530CD29C0}']
    //procedure setDelegate(delegate: Pointer); cdecl;
    //function delegate: Pointer; cdecl;
    //procedure setVideoContentMode(videoContentMode: UIViewContentMode); cdecl;
    //function videoContentMode: UIViewContentMode; cdecl;
    //procedure setEnabled(enabled: Boolean); cdecl;
    //function isEnabled: Boolean; cdecl;
    //procedure setRotationOverride(rotationOverride: NSValue); cdecl;
    //function rotationOverride: NSValue; cdecl;
  //end;
  //TRTCMTLVideoView = class(TOCGenericImport<RTCMTLVideoViewClass, RTCMTLVideoView>) end;
  //PRTCMTLVideoView = Pointer;

  {*******************************************}
  RTCI420BufferClass = interface(NSObjectClass)
    ['{CD148B90-CE83-4B88-BE05-4248C65A57B3}']
  end;
  RTCI420Buffer = interface(NSObject)
    ['{EBB60134-F56C-441D-8407-4C628BD81CE4}']

    //@property(nonatomic, readonly) int width;
    function width: Integer; cdecl;

    //@property(nonatomic, readonly) int height;
    function height: Integer; cdecl;

    //- (id<RTCI420Buffer>)toI420;
    //function toI420: Pointer; cdecl;

    //@property(nonatomic, readonly) int chromaWidth;
    function chromaWidth: Integer; cdecl;

    //@property(nonatomic, readonly) int chromaHeight;
    function chromaHeight: Integer; cdecl;

    //@property(nonatomic, readonly) const uint8_t *dataY;
    function dataY: PByte; cdecl;

    //@property(nonatomic, readonly) const uint8_t *dataU;
    function dataU: PByte; cdecl;

    //@property(nonatomic, readonly) const uint8_t *dataV;
    function dataV: PByte; cdecl;

    //@property(nonatomic, readonly) int strideY;
    function strideY: Integer; cdecl;

    //@property(nonatomic, readonly) int strideU;
    function strideU: Integer; cdecl;

    //@property(nonatomic, readonly) int strideV;
    function strideV: Integer; cdecl;

    //- (instancetype)initWithWidth:(int)width
    //                       height:(int)height
    //                        dataY:(const uint8_t *)dataY
    //                        dataU:(const uint8_t *)dataU
    //                        dataV:(const uint8_t *)dataV;
    //[MethodName('initWithWidth:height:dataY:dataU:dataV:')]
    //function initWithWidthHeightDataYDataUDataV(width: Integer; height: Integer; dataY: PByte; dataU: PByte; dataV: PByte): Pointer { instancetype }; cdecl;

    //- (instancetype)initWithWidth:(int)width height:(int)height;
    //[MethodName('initWithWidth:height:')]
    //function initWithWidthHeight(width: Integer; height: Integer): Pointer { instancetype }; cdecl;

    //- (instancetype)initWithWidth:(int)width
    //                       height:(int)height
    //                      strideY:(int)strideY
    //                      strideU:(int)strideU
    //                      strideV:(int)strideV;
    //[MethodName('initWithWidth:height:strideY:strideU:strideV:')]
    //function initWithWidthHeightStrideYStrideUStrideV(width: Integer; height: Integer; strideY: Integer; strideU: Integer; strideV: Integer): Pointer { instancetype }; cdecl;

  end;
  TRTCI420Buffer = class(TOCGenericImport<RTCI420BufferClass, RTCI420Buffer>) end;
  PRTCI420Buffer = Pointer;

  //{*******************************************}
  //RTCMutableI420Buffer = interface(IObjectiveC)
    //['{EEB18C16-8EBF-462A-8FA0-EDEC2F03E588}']
  //end;

  {***********************************}
  //@protocol RTCRtpReceiver <NSObject>
  //@interface RTCRtpReceiver : NSObject <RTCRtpReceiver>
  RTCRtpReceiverClass = interface(NSObjectClass)
    ['{1AAB73CC-2B5B-41B4-8771-CBFCF003223F}']
  end;
  RTCRtpReceiver = interface(NSObject)
    ['{C24EB32B-1630-457B-A0F8-F495370ADF57}']

    //A unique identifier for this receiver.
    //@property(nonatomic, readonly) NSString *receiverId;
    //function receiverId: NSString; cdecl;

    //The currently active RTCRtpParameters, as defined in
    //https://www.w3.org/TR/webrtc/#idl-def-RTCRtpParameters.
    //The WebRTC specification only defines RTCRtpParameters in terms of senders,
    //but this API also applies them to receivers, similar to ORTC:
    //http://ortc.org/wp-content/uploads/2016/03/ortc.html#rtcrtpparameters*.
    //@property(nonatomic, readonly) RTCRtpParameters *parameters;
    //function parameters: RTCRtpParameters; cdecl;

    //The RTCMediaStreamTrack associated with the receiver.
    //Note: reading this property returns a new instance of
    //RTCMediaStreamTrack. Use isEqual: instead of == to compare
    //RTCMediaStreamTrack instances.
    //@property(nonatomic, readonly, nullable) RTCMediaStreamTrack *track;
    function track: RTCMediaStreamTrack; cdecl;

    //The delegate for this RtpReceiver.
    //@property(nonatomic, weak) id<RTCRtpReceiverDelegate> delegate;
    //procedure setDelegate(delegate: Pointer); cdecl;
    //function delegate: Pointer; cdecl;

  end;
  TRTCRtpReceiver = class(TOCGenericImport<RTCRtpReceiverClass, RTCRtpReceiver>) end;
  PRTCRtpReceiver = Pointer;

  {*********************************}
  //@protocol RTCRtpSender <NSObject>
  RTCRtpSenderClass = interface(NSObjectClass)
    ['{0AE0ED18-0EB7-485A-B6AF-0730F5608655}']
  end;
  RTCRtpSender = interface(NSObject)
    ['{F51462D8-AC7F-45DB-9B4C-6D8237AD14AB}']

    //A unique identifier for this sender.
    //@property(nonatomic, readonly) NSString *senderId;
    //function senderId: NSString; cdecl;

    //The currently active RTCRtpParameters, as defined in
    //https://www.w3.org/TR/webrtc/#idl-def-RTCRtpParameters.
    //@property(nonatomic, copy) RTCRtpParameters *parameters;
    procedure setParameters(parameters: RTCRtpParameters); cdecl;
    function parameters: RTCRtpParameters; cdecl;

    //The RTCMediaStreamTrack associated with the sender.
    //Note: reading this property returns a new instance of
    //RTCMediaStreamTrack. Use isEqual: instead of == to compare
    //RTCMediaStreamTrack instances.
    //@property(nonatomic, copy, nullable) RTCMediaStreamTrack *track;
    procedure setTrack(track: RTCMediaStreamTrack); cdecl;
    function track: RTCMediaStreamTrack; cdecl;

    //The RTCDtmfSender accociated with the RTP sender.
    //@property(nonatomic, readonly, nullable) id<RTCDtmfSender> dtmfSender;
    //function dtmfSender: Pointer; cdecl;

  end;
  TRTCRtpSender = class(TOCGenericImport<RTCRtpSenderClass, RTCRtpSender>) end;
  PRTCRtpSender = Pointer;

  {*************************}
  //The RTCRtpTransceiver maps to the RTCRtpTransceiver defined by the WebRTC
  //specification. A transceiver represents a combination of an RTCRtpSender
  //and an RTCRtpReceiver that share a common mid. As defined in JSEP, an
  //RTCRtpTransceiver is said to be associated with a media description if its
  //mid property is non-nil; otherwise, it is said to be disassociated.
  //JSEP: https://tools.ietf.org/html/draft-ietf-rtcweb-jsep-24
  //Note that RTCRtpTransceivers are only supported when using
  //RTCPeerConnection with Unified Plan SDP.
  //WebRTC specification for RTCRtpTransceiver, the JavaScript analog:
  //https://w3c.github.io/webrtc-pc/#dom-rtcrtptransceiver
  //@protocol RTCRtpTransceiver <NSObject>
  //@interface RTCRtpTransceiver : NSObject <RTCRtpTransceiver>
  RTCRtpTransceiverClass = interface(NSObjectClass)
    ['{86DACB46-DC39-493E-9B6F-CC1A998D351A}']
  end;
  RTCRtpTransceiver = interface(NSObject)
    ['{3322A9ED-3447-4596-BEE6-B7F3E9A71B4A}']

    //Media type of the transceiver. The sender and receiver will also have this
    //type.
    //@property(nonatomic, readonly) RTCRtpMediaType mediaType;
    function mediaType: RTCRtpMediaType; cdecl;

    //The mid attribute is the mid negotiated and present in the local and
    //remote descriptions. Before negotiation is complete, the mid value may be
    //nil. After rollbacks, the value may change from a non-nil value to nil.
    //https://w3c.github.io/webrtc-pc/#dom-rtcrtptransceiver-mid
    //@property(nonatomic, readonly) NSString *mid;
    //function mid: NSString; cdecl;

    //The sender attribute exposes the RTCRtpSender corresponding to the RTP
    //media that may be sent with the transceiver's mid. The sender is always
    //present, regardless of the direction of media.
    //https://w3c.github.io/webrtc-pc/#dom-rtcrtptransceiver-sender
    //@property(nonatomic, readonly) RTCRtpSender *sender;
    //function sender: RTCRtpSender; cdecl;

    //The receiver attribute exposes the RTCRtpReceiver corresponding to the RTP
    //media that may be received with the transceiver's mid. The receiver is
    //always present, regardless of the direction of media.
    //https://w3c.github.io/webrtc-pc/#dom-rtcrtptransceiver-receiver
    //@property(nonatomic, readonly) RTCRtpReceiver *receiver;
    function receiver: RTCRtpReceiver; cdecl;

    //The isStopped attribute indicates that the sender of this transceiver will
    //no longer send, and that the receiver will no longer receive. It is true if
    //either stop has been called or if setting the local or remote description
    //has caused the RTCRtpTransceiver to be stopped.
    //https://w3c.github.io/webrtc-pc/#dom-rtcrtptransceiver-stopped
    //@property(nonatomic, readonly) BOOL isStopped;
    //function isStopped: Boolean; cdecl;

    //The direction attribute indicates the preferred direction of this
    //transceiver, which will be used in calls to createOffer and createAnswer.
    //An update of directionality does not take effect immediately. Instead,
    //future calls to createOffer and createAnswer mark the corresponding media
    //descriptions as sendrecv, sendonly, recvonly, or inactive.
    //https://w3c.github.io/webrtc-pc/#dom-rtcrtptransceiver-direction
    //@property(nonatomic) RTCRtpTransceiverDirection direction;
    //procedure setDirection(direction: RTCRtpTransceiverDirection); cdecl;
    //function direction: RTCRtpTransceiverDirection; cdecl;

    //The currentDirection attribute indicates the current direction negotiated
    //for this transceiver. If this transceiver has never been represented in an
    //offer/answer exchange, or if the transceiver is stopped, the value is not
    //present and this method returns NO.
    //https://w3c.github.io/webrtc-pc/#dom-rtcrtptransceiver-currentdirection
    //- (BOOL)currentDirection:(RTCRtpTransceiverDirection *)currentDirectionOut;
    //function currentDirection(currentDirectionOut: PRTCRtpTransceiverDirection): Boolean; cdecl;

    //The stop method irreversibly stops the RTCRtpTransceiver. The sender of
    //this transceiver will no longer send, the receiver will no longer receive.
    //https://w3c.github.io/webrtc-pc/#dom-rtcrtptransceiver-stop
    //- (void)stop;
    //procedure stop; cdecl;

  end;
  TRTCRtpTransceiver = class(TOCGenericImport<RTCRtpTransceiverClass, RTCRtpTransceiver>) end;
  PRTCRtpTransceiver = Pointer;

  //{***************************************************}
  //RTCRtpTransceiverInitClass = interface(NSObjectClass)
    //['{2A8922EB-41E8-41E6-966F-26C914045A32}']
  //end;
  //RTCRtpTransceiverInit = interface(NSObject)
    //['{49DD4612-4E85-40E0-8FF4-A53B35217CE9}']
    //procedure setDirection(direction: RTCRtpTransceiverDirection); cdecl;
    //function direction: RTCRtpTransceiverDirection; cdecl;
    //procedure setStreamIds(streamIds: NSArray); cdecl;
    //function streamIds: NSArray; cdecl;
    //procedure setSendEncodings(sendEncodings: NSArray); cdecl;
    //function sendEncodings: NSArray; cdecl;
  //end;
  //TRTCRtpTransceiverInit = class(TOCGenericImport<RTCRtpTransceiverInitClass, RTCRtpTransceiverInit>) end;
  //PRTCRtpTransceiverInit = Pointer;

  {***************************************************}
  RTCSessionDescriptionClass = interface(NSObjectClass)
    ['{A5B3B5A3-5F03-4ED0-B9CC-3A8B0E894050}']

    //+ (NSString *)stringForType:(RTCSdpType)type;
    { class } function stringForType(&type: RTCSdpType): NSString; cdecl;

    //+ (RTCSdpType)typeForString:(NSString *)string;
    { class } function typeForString(&string: NSString): RTCSdpType; cdecl;

  end;
  RTCSessionDescription = interface(NSObject)
    ['{E3C8A270-2141-4F67-BDC5-36F42368E506}']

    //The type of session description.
    //@property(nonatomic, readonly) RTCSdpType type;
    function &type: RTCSdpType; cdecl;

    //The SDP string representation of this session description.
    //@property(nonatomic, readonly) NSString *sdp;
    function sdp: NSString; cdecl;

    //- (instancetype)init NS_UNAVAILABLE;

    //Initialize a session description with a type and SDP string.
    //- (instancetype)initWithType:(RTCSdpType)type sdp:(NSString *)sdp NS_DESIGNATED_INITIALIZER;
    function initWithType(&type: RTCSdpType; sdp: NSString): Pointer { instancetype }; cdecl;

  end;
  TRTCSessionDescription = class(TOCGenericImport<RTCSessionDescriptionClass, RTCSessionDescription>) end;
  PRTCSessionDescription = Pointer;

  {***********************************************}
  //@interface RTCPeerConnection : NSObject
  RTCPeerConnectionClass = interface(NSObjectClass)
    ['{72035A56-D3E9-42F0-BA42-986BC2E10196}']
  end;
  RTCPeerConnection = interface(NSObject)
    ['{F5473616-4DB0-4E75-B869-555761CA6794}']

    //The object that will be notifed about events such as state changes and
    //streams being added or removed.
    //@property(nonatomic, weak, nullable) id<RTCPeerConnectionDelegate> delegate;
    procedure setDelegate(delegate: Pointer); cdecl;
    function delegate: Pointer; cdecl;

    //This property is not available with RTCSdpSemanticsUnifiedPlan. Please use
    //|senders| instead.
    //@property(nonatomic, readonly) NSArray<RTCMediaStream *> *localStreams;
    //function localStreams: NSArray; cdecl;

    //@property(nonatomic, readonly, nullable) RTCSessionDescription *localDescription;
    function localDescription: RTCSessionDescription; cdecl;

    //@property(nonatomic, readonly, nullable) RTCSessionDescription *remoteDescription;
    function remoteDescription: RTCSessionDescription; cdecl;

    //@property(nonatomic, readonly) RTCSignalingState signalingState;
    function signalingState: RTCSignalingState; cdecl;

    //@property(nonatomic, readonly) RTCIceConnectionState iceConnectionState;
    function iceConnectionState: RTCIceConnectionState; cdecl;

    //@property(nonatomic, readonly) RTCIceGatheringState iceGatheringState;
    function iceGatheringState: RTCIceGatheringState; cdecl;

    //@property(nonatomic, readonly, copy) RTCConfiguration *configuration;
    //function configuration: RTCConfiguration; cdecl;

    //Gets all RTCRtpSenders associated with this peer connection.
    //Note: reading this property returns different instances of RTCRtpSender.
    //Use isEqual: instead of == to compare RTCRtpSender instances.
    //@property(nonatomic, readonly) NSArray<RTCRtpSender *> *senders;
    function senders: NSArray; cdecl;

    //Gets all RTCRtpReceivers associated with this peer connection.
    //Note: reading this property returns different instances of RTCRtpReceiver.
    //Use isEqual: instead of == to compare RTCRtpReceiver instances.
    //@property(nonatomic, readonly) NSArray<RTCRtpReceiver *> *receivers;
    //function receivers: NSArray; cdecl;

    //Gets all RTCRtpTransceivers associated with this peer connection.
    //Note: reading this property returns different instances of
    //RTCRtpTransceiver. Use isEqual: instead of == to compare RTCRtpTransceiver
    //instances.
    //This is only available with RTCSdpSemanticsUnifiedPlan specified.
    //@property(nonatomic, readonly) NSArray<RTCRtpTransceiver *> *transceivers;
    function transceivers: NSArray; cdecl;

    //- (instancetype)init NS_UNAVAILABLE;

    //Sets the PeerConnection's global configuration to |configuration|.
    //Any changes to STUN/TURN servers or ICE candidate policy will affect the
    //next gathering phase, and cause the next call to createOffer to generate
    //new ICE credentials. Note that the BUNDLE and RTCP-multiplexing policies
    //cannot be changed with this method.
    //- (BOOL)setConfiguration:(RTCConfiguration *)configuration;
    //function setConfiguration(configuration: RTCConfiguration): Boolean; cdecl;

    //Terminate all media and close the transport.
    //- (void)close;
    procedure close; cdecl;

    //Provide a remote candidate to the ICE Agent.
    //- (void)addIceCandidate:(RTCIceCandidate *)candidate;
    procedure addIceCandidate(candidate: RTCIceCandidate); cdecl;

    //Remove a group of remote candidates from the ICE Agent.
    //- (void)removeIceCandidates:(NSArray<RTCIceCandidate *> *)candidates;
    procedure removeIceCandidates(candidates: NSArray); cdecl;

    //Add a new media stream to be sent on this peer connection.
    //This method is not supported with RTCSdpSemanticsUnifiedPlan. Please use
    //addTrack instead.
    //- (void)addStream:(RTCMediaStream *)stream;
    //procedure addStream(stream: RTCMediaStream); cdecl;

    //Remove the given media stream from this peer connection.
    //This method is not supported with RTCSdpSemanticsUnifiedPlan. Please use
    //removeTrack instead.
    //- (void)removeStream:(RTCMediaStream *)stream;
    //procedure removeStream(stream: RTCMediaStream); cdecl;

    //Add a new media stream track to be sent on this peer connection, and return
    //the newly created RTCRtpSender. The RTCRtpSender will be associated with
    //the streams specified in the |streamIds| list.
    //
    //Errors: If an error occurs, returns nil. An error can occur if:
    //- A sender already exists for the track.
    //- The peer connection is closed.
    //- (RTCRtpSender *)addTrack:(RTCMediaStreamTrack *)track streamIds:(NSArray<NSString *> *)streamIds;
    function addTrack(track: RTCMediaStreamTrack; streamIds: NSArray): RTCRtpSender; cdecl;

    //With PlanB semantics, removes an RTCRtpSender from this peer connection.
    //
    //With UnifiedPlan semantics, sets sender's track to null and removes the
    //send component from the associated RTCRtpTransceiver's direction.
    //
    //Returns YES on success.
    //- (BOOL)removeTrack:(RTCRtpSender *)sender;
    function removeTrack(sender: RTCRtpSender): Boolean; cdecl;

    //addTransceiver creates a new RTCRtpTransceiver and adds it to the set of
    //transceivers. Adding a transceiver will cause future calls to CreateOffer
    //to add a media description for the corresponding transceiver.
    //
    //The initial value of |mid| in the returned transceiver is nil. Setting a
    //new session description may change it to a non-nil value.
    //
    //https://w3c.github.io/webrtc-pc/#dom-rtcpeerconnection-addtransceiver
    //
    //Optionally, an RtpTransceiverInit structure can be specified to configure
    //the transceiver from construction. If not specified, the transceiver will
    //default to having a direction of kSendRecv and not be part of any streams.
    //
    //These methods are only available when Unified Plan is enabled (see
    //RTCConfiguration).
    //
    //Adds a transceiver with a sender set to transmit the given track. The kind
    //of the transceiver (and sender/receiver) will be derived from the kind of
    //the track.
    //- (RTCRtpTransceiver *)addTransceiverWithTrack:(RTCMediaStreamTrack *)track;
    //[MethodName('addTransceiverWithTrack:')]
    //function addTransceiverWithTrack(track: RTCMediaStreamTrack): RTCRtpTransceiver; cdecl;

    //- (RTCRtpTransceiver *)addTransceiverWithTrack:(RTCMediaStreamTrack *)track
    //                                          init:(RTCRtpTransceiverInit *)init;
    //[MethodName('addTransceiverWithTrack:init:')]
    //function addTransceiverWithTrackInit(track: RTCMediaStreamTrack; init: RTCRtpTransceiverInit): RTCRtpTransceiver; cdecl;

    //Adds a transceiver with the given kind. Can either be RTCRtpMediaTypeAudio
    //or RTCRtpMediaTypeVideo.
    //- (RTCRtpTransceiver *)addTransceiverOfType:(RTCRtpMediaType)mediaType;
    //[MethodName('addTransceiverOfType:')]
    //function addTransceiverOfType(mediaType: RTCRtpMediaType): RTCRtpTransceiver; cdecl;

    //- (RTCRtpTransceiver *)addTransceiverOfType:(RTCRtpMediaType)mediaType
    //                                       init:(RTCRtpTransceiverInit *)init;
    //[MethodName('addTransceiverOfType:init:')]
    //function addTransceiverOfTypeInit(mediaType: RTCRtpMediaType; init: RTCRtpTransceiverInit): RTCRtpTransceiver; cdecl;

    //Generate an SDP offer.
    //- (void)offerForConstraints:(RTCMediaConstraints *)constraints
    //          completionHandler:(nullable void (^)(RTCSessionDescription *_Nullable sdp,
    //                                               NSError *_Nullable error))completionHandler;
    procedure offerForConstraints(constraints: RTCMediaConstraints; completionHandler: TWebRTCPeerConnectionOfferForConstraintsCompletionHandler); cdecl;

    //Generate an SDP answer.
    //- (void)answerForConstraints:(RTCMediaConstraints *)constraints
    //           completionHandler:(nullable void (^)(RTCSessionDescription *_Nullable sdp,
    //                                                NSError *_Nullable error))completionHandler;
    procedure answerForConstraints(constraints: RTCMediaConstraints; completionHandler: TWebRTCPeerConnectionAnswerForConstraintsCompletionHandler); cdecl;

    //Apply the supplied RTCSessionDescription as the local description.
    //- (void)setLocalDescription:(RTCSessionDescription *)sdp
    //          completionHandler:(nullable void (^)(NSError *_Nullable error))completionHandler;
    procedure setLocalDescription(sdp: RTCSessionDescription; completionHandler: TWebRTCPeerConnectionSetLocalDescriptionCompletionHandler); cdecl;

    //Apply the supplied RTCSessionDescription as the remote description.
    //- (void)setRemoteDescription:(RTCSessionDescription *)sdp
    //           completionHandler:(nullable void (^)(NSError *_Nullable error))completionHandler;
    procedure setRemoteDescription(sdp: RTCSessionDescription; completionHandler: TWebRTCPeerConnectionSetRemoteDescriptionCompletionHandler); cdecl;

    //Limits the bandwidth allocated for all RTP streams sent by this
    //PeerConnection. Nil parameters will be unchanged. Setting
    //|currentBitrateBps| will force the available bitrate estimate to the given
    //value. Returns YES if the parameters were successfully updated.
    //- (BOOL)setBweMinBitrateBps:(nullable NSNumber *)minBitrateBps
    //          currentBitrateBps:(nullable NSNumber *)currentBitrateBps
    //              maxBitrateBps:(nullable NSNumber *)maxBitrateBps;
    //function setBweMinBitrateBps(minBitrateBps: NSNumber; currentBitrateBps: NSNumber; maxBitrateBps: NSNumber): Boolean; cdecl;

    //Start or stop recording an Rtc EventLog.
    //- (BOOL)startRtcEventLogWithFilePath:(NSString *)filePath maxSizeInBytes:(int64_t)maxSizeInBytes;
    function startRtcEventLogWithFilePath(filePath: NSString; maxSizeInBytes: Int64): Boolean; cdecl;

    //- (void)stopRtcEventLog;
    procedure stopRtcEventLog; cdecl;

    //Create an RTCRtpSender with the specified kind and media stream ID.
    //See RTCMediaStreamTrack.h for available kinds.
    //This method is not supported with RTCSdpSemanticsUnifiedPlan. Please use
    //addTransceiver instead.
    //- (RTCRtpSender *)senderWithKind:(NSString *)kind streamId:(NSString *)streamId;
    //function senderWithKind(kind: NSString; streamId: NSString): RTCRtpSender; cdecl;

    //Create a new data channel with the given label and configuration.
    //- (nullable RTCDataChannel *)dataChannelForLabel:(NSString *)label
    //                                   configuration:(RTCDataChannelConfiguration *)configuration;
    //function dataChannelForLabel(&label: NSString; configuration: RTCDataChannelConfiguration): RTCDataChannel; cdecl;

    //Gather stats for the given RTCMediaStreamTrack. If |mediaStreamTrack| is nil
    //statistics are gathered for all tracks.
    //- (void)statsForTrack:(nullable RTCMediaStreamTrack *)mediaStreamTrack
    //     statsOutputLevel:(RTCStatsOutputLevel)statsOutputLevel
    //    completionHandler:(nullable void (^)(NSArray<RTCLegacyStatsReport *> *stats))completionHandler;
    //procedure statsForTrack(mediaStreamTrack: RTCMediaStreamTrack; statsOutputLevel: RTCStatsOutputLevel; completionHandler: TWebRTCCompletionHandler3); cdecl;

  end;
  TRTCPeerConnection = class(TOCGenericImport<RTCPeerConnectionClass, RTCPeerConnection>) end;
  PRTCPeerConnection = Pointer;

  {*********************************************************************}
  //@interface RTCVideoSource : RTCMediaSource <RTCVideoCapturerDelegate>
  RTCVideoSourceClass = interface(RTCMediaSourceClass)
    ['{81E090DA-C21F-4034-A60C-311FF5E99DE2}']
  end;
  RTCVideoSource = interface(RTCMediaSource)
    ['{889191B4-4AB0-4AF3-B527-2ACB2759FFE3}']

    //- (instancetype)init NS_UNAVAILABLE;

    //Calling this function will cause frames to be scaled down to the
    //requested resolution. Also, frames will be cropped to match the
    //requested aspect ratio, and frames will be dropped to match the
    //requested fps. The requested aspect ratio is orientation agnostic and
    //will be adjusted to maintain the input orientation, so it doesn't
    //matter if e.g. 1280x720 or 720x1280 is requested.
    //- (void)adaptOutputFormatToWidth:(int)width height:(int)height fps:(int)fps;
    procedure adaptOutputFormatToWidth(width: Integer; height: Integer; fps: Integer); cdecl;

  end;
  TRTCVideoSource = class(TOCGenericImport<RTCVideoSourceClass, RTCVideoSource>) end;
  PRTCVideoSource = Pointer;

  //{*************************************************************}
  //RTCPeerConnectionFactoryOptionsClass = interface(NSObjectClass)
    //['{8E5529AB-311A-4C3C-A854-2C0EA00230FD}']
  //end;
  //RTCPeerConnectionFactoryOptions = interface(NSObject)
    //['{C67B18A4-5472-42D5-8C92-CF89C3AC3566}']
    //procedure setDisableEncryption(disableEncryption: Boolean); cdecl;
    //function disableEncryption: Boolean; cdecl;
    //procedure setDisableNetworkMonitor(disableNetworkMonitor: Boolean); cdecl;
    //function disableNetworkMonitor: Boolean; cdecl;
    //procedure setIgnoreLoopbackNetworkAdapter(ignoreLoopbackNetworkAdapter: Boolean); cdecl;
    //function ignoreLoopbackNetworkAdapter: Boolean; cdecl;
    //procedure setIgnoreVPNNetworkAdapter(ignoreVPNNetworkAdapter: Boolean); cdecl;
    //function ignoreVPNNetworkAdapter: Boolean; cdecl;
    //procedure setIgnoreCellularNetworkAdapter(ignoreCellularNetworkAdapter: Boolean); cdecl;
    //function ignoreCellularNetworkAdapter: Boolean; cdecl;
    //procedure setIgnoreWiFiNetworkAdapter(ignoreWiFiNetworkAdapter: Boolean); cdecl;
    //function ignoreWiFiNetworkAdapter: Boolean; cdecl;
    //procedure setIgnoreEthernetNetworkAdapter(ignoreEthernetNetworkAdapter: Boolean); cdecl;
    //function ignoreEthernetNetworkAdapter: Boolean; cdecl;
    //procedure setEnableAes128Sha1_32CryptoCipher(enableAes128Sha1_32CryptoCipher: Boolean); cdecl;
    //function enableAes128Sha1_32CryptoCipher: Boolean; cdecl;
    //procedure setEnableGcmCryptoSuites(enableGcmCryptoSuites: Boolean); cdecl;
    //function enableGcmCryptoSuites: Boolean; cdecl;
    //procedure setRequireFrameEncryption(requireFrameEncryption: Boolean); cdecl;
    //function requireFrameEncryption: Boolean; cdecl;
    //function init: Pointer { instancetype }; cdecl;
  //end;
  //TRTCPeerConnectionFactoryOptions = class(TOCGenericImport<RTCPeerConnectionFactoryOptionsClass, RTCPeerConnectionFactoryOptions>) end;
  //PRTCPeerConnectionFactoryOptions = Pointer;

  //{***********************************************}
  //RTCRtcpParametersClass = interface(NSObjectClass)
    //['{E8094EC1-7A9B-4487-9FC9-F8C92D8F6DDD}']
  //end;
  //RTCRtcpParameters = interface(NSObject)
    //['{0D8FD227-2A68-4018-85CC-5A3499E57C53}']
    //function cname: NSString; cdecl;
    //procedure setIsReducedSize(isReducedSize: Boolean); cdecl;
    //function isReducedSize: Boolean; cdecl;
    //function init: Pointer { instancetype }; cdecl;
  //end;
  //TRTCRtcpParameters = class(TOCGenericImport<RTCRtcpParametersClass, RTCRtcpParameters>) end;
  //PRTCRtcpParameters = Pointer;

  //{***************************************************}
  //RTCRtpCodecParametersClass = interface(NSObjectClass)
    //['{37FE9BA9-2F15-4E8C-9128-49475017F625}']
  //end;
  //RTCRtpCodecParameters = interface(NSObject)
    //['{1BEBD056-C52B-4D12-8443-165BEA74E193}']
    //procedure setPayloadType(payloadType: Integer); cdecl;
    //function payloadType: Integer; cdecl;
    //function name: NSString; cdecl;
    //function kind: NSString; cdecl;
    //function clockRate: NSNumber; cdecl;
    //function numChannels: NSNumber; cdecl;
    //function parameters: NSDictionary; cdecl;
    //function init: Pointer { instancetype }; cdecl;
  //end;
  //TRTCRtpCodecParameters = class(TOCGenericImport<RTCRtpCodecParametersClass, RTCRtpCodecParameters>) end;
  //PRTCRtpCodecParameters = Pointer;

  {**********************************************}
  //@interface RTCRtpEncodingParameters : NSObject
  RTCRtpEncodingParametersClass = interface(NSObjectClass)
    ['{B4DA3F62-1BF3-48BB-8F95-1BE788B12E73}']
  end;
  RTCRtpEncodingParameters = interface(NSObject)
    ['{6B905749-0D33-4DF7-9121-B18EB2E8E071}']

    //Controls whether the encoding is currently transmitted.
    //@property(nonatomic, assign) BOOL isActive;
    //procedure setIsActive(isActive: Boolean); cdecl;
    //function isActive: Boolean; cdecl;

    //The maximum bitrate to use for the encoding, or nil if there is no
    //limit.
    //@property(nonatomic, copy, nullable) NSNumber *maxBitrateBps;
    procedure setMaxBitrateBps(maxBitrateBps: NSNumber); cdecl;
    function maxBitrateBps: NSNumber; cdecl;

    //The minimum bitrate to use for the encoding, or nil if there is no
    //limit.
    //@property(nonatomic, copy, nullable) NSNumber *minBitrateBps;
    //procedure setMinBitrateBps(minBitrateBps: NSNumber); cdecl;
    //function minBitrateBps: NSNumber; cdecl;

    //The maximum framerate to use for the encoding, or nil if there is no
    //limit.
    //@property(nonatomic, copy, nullable) NSNumber *maxFramerate;
    //procedure setMaxFramerate(maxFramerate: NSNumber); cdecl;
    //function maxFramerate: NSNumber; cdecl;

    //The requested number of temporal layers to use for the encoding, or nil
    //if the default should be used.
    //@property(nonatomic, copy, nullable) NSNumber *numTemporalLayers;
    //procedure setNumTemporalLayers(numTemporalLayers: NSNumber); cdecl;
    //function numTemporalLayers: NSNumber; cdecl;

    //The SSRC being used by this encoding. */
    //@property(nonatomic, readonly, nullable) NSNumber *ssrc;
    //function ssrc: NSNumber; cdecl;

    //- (instancetype)init NS_DESIGNATED_INITIALIZER;
    //function init: Pointer { instancetype }; cdecl;

  end;
  TRTCRtpEncodingParameters = class(TOCGenericImport<RTCRtpEncodingParametersClass, RTCRtpEncodingParameters>) end;
  PRTCRtpEncodingParameters = Pointer;

  //{***************************************************}
  //RTCRtpHeaderExtensionClass = interface(NSObjectClass)
    //['{F8164A6A-F3AD-4347-BF90-B41A004FF44E}']
  //end;
  //RTCRtpHeaderExtension = interface(NSObject)
    //['{7D06813C-1545-463D-A4FA-FB3D4FB4915A}']
    //function uri: NSString; cdecl;
    //function id: Integer; cdecl;
    //function isEncrypted: Boolean; cdecl;
    //function init: Pointer { instancetype }; cdecl;
  //end;
  //TRTCRtpHeaderExtension = class(TOCGenericImport<RTCRtpHeaderExtensionClass, RTCRtpHeaderExtension>) end;
  //PRTCRtpHeaderExtension = Pointer;

  {**************************************}
  //@interface RTCRtpParameters : NSObject
  RTCRtpParametersClass = interface(NSObjectClass)
    ['{9FD0BAE1-C448-4A1E-A244-78B31E04AAB1}']
  end;
  RTCRtpParameters = interface(NSObject)
    ['{41D43D16-F1A1-419F-A356-BFC8FEE7A66F}']

    //A unique identifier for the last set of parameters applied.
    //@property(nonatomic, copy) NSString *transactionId;
    procedure setTransactionId(transactionId: NSString); cdecl;
    function transactionId: NSString; cdecl;

    //Parameters used for RTCP.
    //@property(nonatomic, readonly, copy) RTCRtcpParameters *rtcp;
    //function rtcp: RTCRtcpParameters; cdecl;

    //An array containing parameters for RTP header extensions.
    //@property(nonatomic, readonly, copy) NSArray<RTCRtpHeaderExtension *> *headerExtensions;
    function headerExtensions: NSArray; cdecl;

    //The currently active encodings in the order of preference.
    //@property(nonatomic, copy) NSArray<RTCRtpEncodingParameters *> *encodings;
    procedure setEncodings(encodings: NSArray); cdecl;
    function encodings: NSArray; cdecl;

    //The negotiated set of send codecs in order of preference.
    //@property(nonatomic, copy) NSArray<RTCRtpCodecParameters *> *codecs;
    procedure setCodecs(codecs: NSArray); cdecl;
    function codecs: NSArray; cdecl;

    //- (instancetype)init NS_DESIGNATED_INITIALIZER;
    function init: Pointer { instancetype }; cdecl;

  end;
  TRTCRtpParameters = class(TOCGenericImport<RTCRtpParametersClass, RTCRtpParameters>) end;
  PRTCRtpParameters = Pointer;

  //{********************************************************}
  //RTCVideoDecoderFactoryH264Class = interface(NSObjectClass)
    //['{AB6A5448-09E3-40F4-8BA4-C98EF63768D9}']
  //end;
  //RTCVideoDecoderFactoryH264 = interface(NSObject)
    //['{CC95BD22-56FA-4B33-BE77-0EC5F85B49B6}']
  //end;
  //TRTCVideoDecoderFactoryH264 = class(TOCGenericImport<RTCVideoDecoderFactoryH264Class, RTCVideoDecoderFactoryH264>) end;
  //PRTCVideoDecoderFactoryH264 = Pointer;

  //{*************************************************}
  //RTCVideoDecoderH264Class = interface(NSObjectClass)
    //['{4AE2C9E4-82F9-4FE6-8DA5-ECD9A275EAC0}']
  //end;
  //RTCVideoDecoderH264 = interface(NSObject)
    //['{3536063B-C468-4A52-86EC-6950E6980EF7}']
  //end;
  //TRTCVideoDecoderH264 = class(TOCGenericImport<RTCVideoDecoderH264Class, RTCVideoDecoderH264>) end;
  //PRTCVideoDecoderH264 = Pointer;

  //{************************************************}
  //RTCVideoDecoderVP8Class = interface(NSObjectClass)
    //['{E0189880-17DA-445F-AE7F-87945B87AC14}']
    //{ class } function vp8Decoder: Pointer; cdecl;
  //end;
  //RTCVideoDecoderVP8 = interface(NSObject)
    //['{A81E1B58-629B-4CD3-9374-8571A0306EE2}']
  //end;
  //TRTCVideoDecoderVP8 = class(TOCGenericImport<RTCVideoDecoderVP8Class, RTCVideoDecoderVP8>) end;
  //PRTCVideoDecoderVP8 = Pointer;

  //{************************************************}
  //RTCVideoDecoderVP9Class = interface(NSObjectClass)
    //['{49ADA230-1A6D-4470-B9D0-47C6161B4CAC}']
    //{ class } function vp9Decoder: Pointer; cdecl;
  //end;
  //RTCVideoDecoderVP9 = interface(NSObject)
    //['{1DF313DA-FCEB-4C7B-90FB-414906295340}']
  //end;
  //TRTCVideoDecoderVP9 = class(TOCGenericImport<RTCVideoDecoderVP9Class, RTCVideoDecoderVP9>) end;
  //PRTCVideoDecoderVP9 = Pointer;

  //{********************************************************}
  //RTCVideoEncoderFactoryH264Class = interface(NSObjectClass)
    //['{17D653AF-6374-415F-9464-FB3E2CF10EC4}']
  //end;
  //RTCVideoEncoderFactoryH264 = interface(NSObject)
    //['{3544F254-E238-4514-8358-AD68F66231DD}']
  //end;
  //TRTCVideoEncoderFactoryH264 = class(TOCGenericImport<RTCVideoEncoderFactoryH264Class, RTCVideoEncoderFactoryH264>) end;
  //PRTCVideoEncoderFactoryH264 = Pointer;

  //{*************************************************}
  //RTCVideoEncoderH264Class = interface(NSObjectClass)
    //['{7E87C686-5E63-4474-B292-F675A02852F5}']
  //end;
  //RTCVideoEncoderH264 = interface(NSObject)
    //['{DCB7D623-99E1-4D50-B8BA-DC7CDA4A443C}']
    //function initWithCodecInfo(codecInfo: RTCVideoCodecInfo): Pointer { instancetype }; cdecl;
  //end;
  //TRTCVideoEncoderH264 = class(TOCGenericImport<RTCVideoEncoderH264Class, RTCVideoEncoderH264>) end;
  //PRTCVideoEncoderH264 = Pointer;

  //{************************************************}
  //RTCVideoEncoderVP8Class = interface(NSObjectClass)
    //['{50C9BCEF-C572-43CB-8B45-2D41F47EC199}']
    //{ class } function vp8Encoder: Pointer; cdecl;
  //end;
  //RTCVideoEncoderVP8 = interface(NSObject)
    //['{9C42EE69-7803-4ED1-9490-5363AA9CE379}']
  //end;
  //TRTCVideoEncoderVP8 = class(TOCGenericImport<RTCVideoEncoderVP8Class, RTCVideoEncoderVP8>) end;
  //PRTCVideoEncoderVP8 = Pointer;

  //{************************************************}
  //RTCVideoEncoderVP9Class = interface(NSObjectClass)
    //['{F85718EE-23DC-4D16-BB84-CD6C44879F01}']
    //{ class } function vp9Encoder: Pointer; cdecl;
  //end;
  //RTCVideoEncoderVP9 = interface(NSObject)
    //['{70A9750A-F8E2-4ABC-B41A-B9749F56B43F}']
  //end;
  //TRTCVideoEncoderVP9 = class(TOCGenericImport<RTCVideoEncoderVP9Class, RTCVideoEncoderVP9>) end;
  //PRTCVideoEncoderVP9 = Pointer;

  //{********************************}
  //RTCDevice = interface(IObjectiveC)
    //['{3F119A2E-EC1F-4C58-A3BB-01F225062DA5}']
    //function deviceType: RTCDeviceType; cdecl;
    //function isIOS11OrLater: Boolean; cdecl;
  //end;

  //{**********************************************}
  //RTCAudioSessionDelegate = interface(IObjectiveC)
    //['{D688ED74-51FA-4722-96A8-F6D4B9F480A9}']
    //procedure audioSessionDidBeginInterruption(session: RTCAudioSession); cdecl;
    //procedure audioSessionDidEndInterruption(session: RTCAudioSession; shouldResumeSession: Boolean); cdecl;
    //procedure audioSessionDidChangeRoute(session: RTCAudioSession; reason: AVAudioSessionRouteChangeReason; previousRoute: AVAudioSessionRouteDescription); cdecl;
    //procedure audioSessionMediaServerTerminated (session: RTCAudioSession); cdecl;
    //procedure audioSessionMediaServerReset(session: RTCAudioSession); cdecl;
    //[MethodName('audioSession:didChangeCanPlayOrRecord:')]
    //procedure audioSessionDidChangeCanPlayOrRecord(session: RTCAudioSession; didChangeCanPlayOrRecord: Boolean); cdecl;
    //procedure audioSessionDidStartPlayOrRecord(session: RTCAudioSession); cdecl;
    //procedure audioSessionDidStopPlayOrRecord(session: RTCAudioSession); cdecl;
    //[MethodName('audioSession:didChangeOutputVolume:')]
    //procedure audioSessionDidChangeOutputVolume(audioSession: RTCAudioSession; didChangeOutputVolume: Single); cdecl;
    //[MethodName('audioSession:didDetectPlayoutGlitch:')]
    //procedure audioSessionDidDetectPlayoutGlitch(audioSession: RTCAudioSession; didDetectPlayoutGlitch: Int64); cdecl;
    //[MethodName('audioSession:willSetActive:')]
    //procedure audioSessionWillSetActive(audioSession: RTCAudioSession; willSetActive: Boolean); cdecl;
    //[MethodName('audioSession:didSetActive:')]
    //procedure audioSessionDidSetActive(audioSession: RTCAudioSession; didSetActive: Boolean); cdecl;
    //[MethodName('audioSession:failedToSetActive:error:')]
    //procedure audioSessionFailedToSetActiveError(audioSession: RTCAudioSession; failedToSetActive: Boolean; error: NSError); cdecl;
  //end;

  //{********************************************************}
  //RTCAudioSessionActivationDelegate = interface(IObjectiveC)
    //['{15161B43-B6A7-4D66-AA08-586FEC66D10B}']
    //procedure audioSessionDidActivate(session: AVAudioSession); cdecl;
    //procedure audioSessionDidDeactivate(session: AVAudioSession); cdecl;
  //end;

  {****************************************}
  //@protocol RTCVideoFrameBuffer <NSObject>
  //RTCVideoFrameBuffer = interface(IObjectiveC)
    //['{EAEE9751-BBF7-44DC-918D-80D87B1A1BC9}']

    //@property(nonatomic, readonly) int width;
    //function width: Integer; cdecl;

    //@property(nonatomic, readonly) int height;
    //function height: Integer; cdecl;

    //- (id<RTCI420Buffer>)toI420;
    //function toI420: Pointer; cdecl;

  //end;

  {*********************************************}
  //@protocol RTCVideoCapturerDelegate <NSObject>
  RTCVideoCapturerDelegate = interface(IObjectiveC)
    ['{128218E8-24D4-4AE6-B4E2-998C3B7AB6E9}']

    //- (void)capturer:(RTCVideoCapturer *)capturer didCaptureVideoFrame:(RTCVideoFrame *)frame;
    procedure capturer(capturer: RTCVideoCapturer; didCaptureVideoFrame: RTCVideoFrame); cdecl;

  end;

  //{*******************************************}
  //RTCCodecSpecificInfo = interface(IObjectiveC)
    //['{A4E42988-D478-4D4C-ACA7-C173335E0D85}']
  //end;

  //{*********************************************}
  //RTCDataChannelDelegate = interface(IObjectiveC)
    //['{FE3BF9F7-BEF6-42A2-BBA2-BF923C87446B}']
    //procedure dataChannelDidChangeState(dataChannel: RTCDataChannel); cdecl;
    //[MethodName('dataChannel:didReceiveMessageWithBuffer:')]
    //procedure dataChannelDidReceiveMessageWithBuffer (dataChannel: RTCDataChannel; didReceiveMessageWithBuffer: RTCDataBuffer); cdecl;
    //[MethodName('dataChannel:didChangeBufferedAmount:')]
    //procedure dataChannelDidChangeBufferedAmount(dataChannel: RTCDataChannel; didChangeBufferedAmount: UInt64); cdecl;
  //end;

  //{**************************************}
  //RTCVideoDecoder = interface(IObjectiveC)
    //['{CA3AC1F9-7545-466E-8832-0FA2E44D413F}']
    //procedure setCallback(callback: RTCVideoDecoderCallback); cdecl;
    //function startDecodeWithSettings(settings: RTCVideoEncoderSettings; numberOfCores: Integer): NSInteger; cdecl;
    //function releaseDecoder: NSInteger; cdecl;
    //function decode(encodedImage: RTCEncodedImage; missingFrames: Boolean; codecSpecificInfo: Pointer; renderTimeMs: Int64): NSInteger; cdecl;
    //function implementationName: NSString; cdecl;
    //function startDecodeWithNumberOfCores(numberOfCores: Integer): NSInteger; cdecl;
  //end;

  //{*********************************************}
  //RTCVideoDecoderFactory = interface(IObjectiveC)
    //['{78896CE3-4543-41A4-8451-0BF665F321ED}']
    //function createDecoder(info: RTCVideoCodecInfo): Pointer; cdecl;
    //function supportedCodecs: NSArray; cdecl;
  //end;

  //{**************************************}
  //RTCVideoEncoder = interface(IObjectiveC)
    //['{7310F706-7913-4B4C-AE9F-924BBB273649}']
    //procedure setCallback(callback: RTCVideoEncoderCallback); cdecl;
    //function startEncodeWithSettings(settings: RTCVideoEncoderSettings; numberOfCores: Integer): NSInteger; cdecl;
    //function releaseEncoder: NSInteger; cdecl;
    //function encode(frame: RTCVideoFrame; codecSpecificInfo: Pointer; frameTypes: NSArray): NSInteger; cdecl;
    //function setBitrate(bitrateKbit: LongWord; framerate: LongWord): Integer; cdecl;
    //function implementationName: NSString; cdecl;
    //function scalingSettings: RTCVideoEncoderQpThresholds; cdecl;
  //end;

  //{*********************************************}
  //RTCVideoEncoderFactory = interface(IObjectiveC)
    //['{2440D44B-5404-4460-B60C-C1BBF5AB8BCA}']
    //function createEncoder(info: RTCVideoCodecInfo): Pointer; cdecl;
    //function supportedCodecs: NSArray; cdecl;
  //end;

  //{************************************}
  //RTCDtmfSender = interface(IObjectiveC)
    //['{32F586AC-065E-45E4-A704-D04BF78739A0}']
    //function canInsertDtmf: Boolean; cdecl;
    //function insertDtmf(tones: NSString; duration: NSTimeInterval; interToneGap: NSTimeInterval): Boolean; cdecl;
    //function remainingTones: NSString; cdecl;
    //function duration: NSTimeInterval; cdecl;
    //function interToneGap: NSTimeInterval; cdecl;
  //end;

  {*************************************}
  //@protocol RTCVideoRenderer <NSObject>
  RTCVideoRenderer = interface(IObjectiveC)
    ['{161733F8-4379-4F41-B7F7-1985FAAF9A11}']

    //The size of the frame.
    //- (void)setSize:(CGSize)size;
    procedure setSize(size: CGSize); cdecl;

    //The frame to be displayed.
    //- (void)renderFrame:(nullable RTCVideoFrame *)frame;
    procedure renderFrame(frame: RTCVideoFrame); cdecl;

  end;

  //{*******************************************}
  //RTCVideoViewDelegate = interface(IObjectiveC)
    //['{62563606-CFC0-47FC-B147-FD8881C8A6CE}']
    //procedure videoView(videoView: Pointer; didChangeVideoSize: CGSize); cdecl;
  //end;

  //{******************************************}
  //RTCVideoViewShading = interface(IObjectiveC)
    //['{88021F97-F59B-408E-AA7E-FD0BA1D15E0A}']
    //[MethodName ('applyShadingForFrameWithWidth:height:rotation:yPlane:uPlane:vPlane:')]
    //procedure applyShadingForFrameWithWidthHeightRotationYPlaneUPlaneVPlane (width: Integer; height: Integer; rotation: RTCVideoRotation; yPlane: GLuint; uPlane: GLuint; vPlane: GLuint); cdecl;
    //[MethodName ('applyShadingForFrameWithWidth:height:rotation:yPlane:uvPlane:')]
    //procedure applyShadingForFrameWithWidthHeightRotationYPlaneUvPlane (width: Integer; height: Integer; rotation: RTCVideoRotation; yPlane: GLuint; uvPlane: GLuint); cdecl;
  //end;

  //{*****************************************}
  //RTCYUVPlanarBuffer = interface(IObjectiveC)
    //['{B2C25DC6-5976-4911-B710-67E03A53F16D}']
    //function chromaWidth: Integer; cdecl;
    //function chromaHeight: Integer; cdecl;
    //function dataY: PByte; cdecl;
    //function dataU: PByte; cdecl;
    //function dataV: PByte; cdecl;
    //function strideY: Integer; cdecl;
    //function strideU: Integer; cdecl;
    //function strideV: Integer; cdecl;
    //[MethodName('initWithWidth:height:dataY:dataU:dataV:')]
    //function initWithWidthHeightDataYDataUDataV(width: Integer; height: Integer; dataY: PByte; dataU: PByte; dataV: PByte): Pointer { instancetype }; cdecl;
    //[MethodName('initWithWidth:height:')]
    //function initWithWidthHeight(width: Integer; height: Integer): Pointer { instancetype }; cdecl;
    //[MethodName('initWithWidth:height:strideY:strideU:strideV:')]
    //function initWithWidthHeightStrideYStrideUStrideV(width: Integer; height: Integer; strideY: Integer; strideU: Integer; strideV: Integer): Pointer { instancetype }; cdecl;
  //end;

  //{************************************************}
  //RTCMutableYUVPlanarBuffer = interface(IObjectiveC)
    //['{BCF71D8A-E4BD-4506-ACE6-3CE0EE691EE5}']
    //function mutableDataY: PByte; cdecl;
    //function mutableDataU: PByte; cdecl;
    //function mutableDataV: PByte; cdecl;
  //end;

  {**********************************************}
  //@protocol RTCPeerConnectionDelegate <NSObject>
  RTCPeerConnectionDelegate = interface(IObjectiveC)
    ['{511BE845-750C-4A21-9125-C996A51BEF2C}']

    //Called when the SignalingState changed.
    //- (void)peerConnection:(RTCPeerConnection *)peerConnection
    //    didChangeSignalingState:(RTCSignalingState)stateChanged;
    [MethodName('peerConnection:didChangeSignalingState:')]
    procedure peerConnectionDidChangeSignalingState(peerConnection: RTCPeerConnection; didChangeSignalingState: RTCSignalingState); cdecl;

    //Called when media is received on a new stream from remote peer.
    //- (void)peerConnection:(RTCPeerConnection *)peerConnection didAddStream:(RTCMediaStream *)stream;
    [MethodName('peerConnection:didAddStream:')]
    procedure peerConnectionDidAddStream(peerConnection: RTCPeerConnection; didAddStream: RTCMediaStream); cdecl;

    //Called when a remote peer closes a stream.
    //This is not called when RTCSdpSemanticsUnifiedPlan is specified.
    //- (void)peerConnection:(RTCPeerConnection *)peerConnection didRemoveStream:(RTCMediaStream *)stream;
    [MethodName('peerConnection:didRemoveStream:')]
    procedure peerConnectionDidRemoveStream(peerConnection: RTCPeerConnection; didRemoveStream: RTCMediaStream); cdecl;

    //Called when negotiation is needed, for example ICE has restarted.
    //- (void)peerConnectionShouldNegotiate:(RTCPeerConnection *)peerConnection;
    procedure peerConnectionShouldNegotiate(peerConnection: RTCPeerConnection); cdecl;

    //Called any time the IceConnectionState changes.
    //- (void)peerConnection:(RTCPeerConnection *)peerConnection
    //    didChangeIceConnectionState:(RTCIceConnectionState)newState;
    [MethodName('peerConnection:didChangeIceConnectionState:')]
    procedure peerConnectionDidChangeIceConnectionState(peerConnection: RTCPeerConnection; didChangeIceConnectionState: RTCIceConnectionState); cdecl;

    //Called any time the IceGatheringState changes.
    //- (void)peerConnection:(RTCPeerConnection *)peerConnection
    //    didChangeIceGatheringState:(RTCIceGatheringState)newState;
    [MethodName('peerConnection:didChangeIceGatheringState:')]
    procedure peerConnectionDidChangeIceGatheringState(peerConnection: RTCPeerConnection; didChangeIceGatheringState: RTCIceGatheringState); cdecl;

    //New ice candidate has been found.
    //- (void)peerConnection:(RTCPeerConnection *)peerConnection
    //    didGenerateIceCandidate:(RTCIceCandidate *)candidate;
    [MethodName('peerConnection:didGenerateIceCandidate:')]
    procedure peerConnectionDidGenerateIceCandidate(peerConnection: RTCPeerConnection; didGenerateIceCandidate: RTCIceCandidate); cdecl;

    //Called when a group of local Ice candidates have been removed.
    //- (void)peerConnection:(RTCPeerConnection *)peerConnection
    //    didRemoveIceCandidates:(NSArray<RTCIceCandidate *> *)candidates;
    [MethodName('peerConnection:didRemoveIceCandidates:')]
    procedure peerConnectionDidRemoveIceCandidates(peerConnection: RTCPeerConnection; didRemoveIceCandidates: NSArray); cdecl;

    //New data channel has been opened.
    //- (void)peerConnection:(RTCPeerConnection *)peerConnection
    //    didOpenDataChannel:(RTCDataChannel *)dataChannel;
    [MethodName('peerConnection:didOpenDataChannel:')]
    procedure peerConnectionDidOpenDataChannel(peerConnection: RTCPeerConnection; didOpenDataChannel: RTCDataChannel); cdecl;

    //Called when signaling indicates a transceiver will be receiving media from
    //the remote endpoint.
    //This is only called with RTCSdpSemanticsUnifiedPlan specified.
    //@optional
    //- (void)peerConnection:(RTCPeerConnection *)peerConnection
    //    didStartReceivingOnTransceiver:(RTCRtpTransceiver *)transceiver;
    [MethodName('peerConnection:didStartReceivingOnTransceiver:')]
    procedure peerConnectionDidStartReceivingOnTransceiver(peerConnection: RTCPeerConnection; didStartReceivingOnTransceiver: RTCRtpTransceiver); cdecl;

    //Called when a receiver and its track are created.
    //@optional
    //- (void)peerConnection:(RTCPeerConnection *)peerConnection
    //        didAddReceiver:(RTCRtpReceiver *)rtpReceiver
    //               streams:(NSArray<RTCMediaStream *> *)mediaStreams;
    [MethodName('peerConnection:didAddReceiver:streams:')]
    procedure peerConnectionDidAddReceiverStreams(peerConnection: RTCPeerConnection; didAddReceiver: RTCRtpReceiver; streams: NSArray); cdecl;

    //Called when the receiver and its track are removed.
    //- (void)peerConnection:(RTCPeerConnection *)peerConnection
    //     didRemoveReceiver:(RTCRtpReceiver *)rtpReceiver;
    [MethodName('peerConnection:didRemoveReceiver:')]
    procedure peerConnectionDidRemoveReceiver(peerConnection: RTCPeerConnection; didRemoveReceiver: RTCRtpReceiver); cdecl;

  end;

  //{*********************************************}
  //RTCRtpReceiverDelegate = interface(IObjectiveC)
    //['{8E81C44C-673D-4CBB-86C8-E19CBEFED21C}']
    //procedure rtpReceiver(rtpReceiver: RTCRtpReceiver; didReceiveFirstPacketForMediaType: RTCRtpMediaType); cdecl;
  //end;

//{*********************************************}
//function kRTCAudioSessionErrorDomain: NSString;
//function kRTCAudioSessionErrorLockRequired: Pointer;
//function kRTCAudioSessionErrorConfiguration: Pointer;
//function kRTCAudioSessionPreferredNumberOfChannels: Pointer;
//function kRTCAudioSessionHighPerformanceSampleRate: Pointer;
//function kRTCAudioSessionLowComplexitySampleRate: Pointer;
//function kRTCAudioSessionHighPerformanceIOBufferDuration: Pointer;
//function kRTCAudioSessionLowComplexityIOBufferDuration: Pointer;
//function kRTCMediaStreamTrackKindAudio: NSString;
//function kRTCMediaStreamTrackKindVideo: NSString;
//function kRTCFieldTrialAudioSendSideBweKey: NSString;
//function kRTCFieldTrialAudioSendSideBweForVideoKey: NSString;
//function kRTCFieldTrialAudioForceNoTWCCKey: NSString;
//function kRTCFieldTrialAudioForceABWENoTWCCKey: NSString;
//function kRTCFieldTrialSendSideBweWithOverheadKey: NSString;
//function kRTCFieldTrialFlexFec03AdvertisedKey: NSString;
//function kRTCFieldTrialFlexFec03Key: NSString;
//function kRTCFieldTrialImprovedBitrateEstimateKey: NSString;
//function kRTCFieldTrialH264HighProfileKey: NSString;
//function kRTCFieldTrialMinimizeResamplingOnMobileKey: NSString;
//function kRTCFieldTrialEnabledValue: NSString;
//function kRTCFieldTrialMedianSlopeFilterKey: NSString;
//function kRTCFieldTrialTrendlineFilterKey: NSString;
//function kRTCVideoCodecH264Name: NSString;
//function kRTCLevel31ConstrainedHigh: NSString;
//function kRTCLevel31ConstrainedBaseline: NSString;
//function kRTCMaxSupportedH264ProfileLevelConstrainedHigh: NSString;
//function kRTCMaxSupportedH264ProfileLevelConstrainedBaseline: NSString;
//function kRTCMediaConstraintsMinAspectRatio: NSString;
//function kRTCMediaConstraintsMaxAspectRatio: NSString;
//function kRTCMediaConstraintsMaxWidth: NSString;
//function kRTCMediaConstraintsMinWidth: NSString;
//function kRTCMediaConstraintsMaxHeight: NSString;
//function kRTCMediaConstraintsMinHeight: NSString;
//function kRTCMediaConstraintsMaxFrameRate: NSString;
//function kRTCMediaConstraintsMinFrameRate: NSString;
//function kRTCMediaConstraintsAudioNetworkAdaptorConfig: NSString;
//function kRTCMediaConstraintsIceRestart: NSString;
//function kRTCMediaConstraintsOfferToReceiveAudio: NSString;
//function kRTCMediaConstraintsOfferToReceiveVideo: NSString;
//function kRTCMediaConstraintsVoiceActivityDetection: NSString;
//function kRTCMediaConstraintsValueTrue: NSString;
//function kRTCMediaConstraintsValueFalse: NSString;
//function kRTCPeerConnectionErrorDomain: NSString;
//function kRTCSessionDescriptionErrorCode: Pointer;
//function kRTCRtxCodecName: NSString;
//function kRTCRedCodecName: NSString;
//function kRTCUlpfecCodecName: NSString;
//function kRTCFlexfecCodecName: NSString;
//function kRTCOpusCodecName: NSString;
//function kRTCIsacCodecName: NSString;
//function kRTCL16CodecName: NSString;
//function kRTCG722CodecName: NSString;
//function kRTCIlbcCodecName: NSString;
//function kRTCPcmuCodecName: NSString;
//function kRTCPcmaCodecName: NSString;
//function kRTCDtmfCodecName: NSString;
//function kRTCComfortNoiseCodecName: NSString;
//function kRTCVp8CodecName: NSString;
//function kRTCVp9CodecName: NSString;
//function kRTCH264CodecName: NSString;
//function kRTCVideoCodecVp8Name: NSString;
//function kRTCVideoCodecVp9Name: NSString;

//RTC_EXTERN void RTCSetupInternalTracer(void);
procedure RTCSetupInternalTracer; cdecl; external 'WebRTC' name _PU + 'RTCSetupInternalTracer';

//Starts capture to specified file. Must be a valid writable path.
//Returns YES if capture starts.
//RTC_EXTERN BOOL RTCStartInternalCapture(NSString* filePath);
function RTCStartInternalCapture(filePath: Pointer { NSString }): Boolean; cdecl; external 'WebRTC' name _PU + 'RTCStartInternalCapture';

//RTC_EXTERN void RTCStopInternalCapture(void);
procedure RTCStopInternalCapture; cdecl; external 'WebRTC' name _PU + 'RTCStopInternalCapture';

//RTC_EXTERN void RTCShutdownInternalTracer(void);
procedure RTCShutdownInternalTracer; cdecl; external 'WebRTC' name _PU + 'RTCShutdownInternalTracer';

//Initialize field trials using a dictionary mapping field trial keys to their
//values. See above for valid keys and values. Must be called before any other
//call into WebRTC. See: webrtc/system_wrappers/include/field_trial.h
//RTC_EXTERN void RTCInitFieldTrialDictionary(NSDictionary<NSString *, NSString *> *fieldTrials);
procedure RTCInitFieldTrialDictionary(fieldTrials: Pointer { NSDictionary } ); cdecl; external 'WebRTC' name _PU + 'RTCInitFieldTrialDictionary';

//Initialize and clean up the SSL library. Failure is fatal. These call the
//corresponding functions in webrtc/rtc_base/ssladapter.h.
//RTC_EXTERN BOOL RTCInitializeSSL(void);
function RTCInitializeSSL: Boolean; cdecl; external 'WebRTC' name _PU + 'RTCInitializeSSL';

//RTC_EXTERN BOOL RTCCleanupSSL(void);
function RTCCleanupSSL: Boolean; cdecl; external 'WebRTC' name _PU + 'RTCCleanupSSL';

//Wrapper for rtc::LogMessage::LogToDebug.
//Sets the minimum severity to be logged to console.
//RTC_EXTERN void RTCSetMinDebugLogLevel(RTCLoggingSeverity severity);
procedure RTCSetMinDebugLogLevel(severity: RTCLoggingSeverity); cdecl; external 'WebRTC' name _PU + 'RTCSetMinDebugLogLevel';

//procedure RTCLogEx(severity: RTCLoggingSeverity; log_string: Pointer { NSString } ); cdecl; external libWebRTC name _PU + 'RTCLogEx';
//function RTCFileName(filePath: MarshaledAString): Pointer { NSString }; cdecl; external libWebRTC name _PU + 'RTCFileName';
//function RTCFieldTrialMedianSlopeFilterValue(windowSize: LongWord; thresholdGain: Double): Pointer { NSString }; cdecl; external libWebRTC name _PU + 'RTCFieldTrialMedianSlopeFilterValue';
//function RTCFieldTrialTrendlineFilterValue(windowSize: LongWord; smoothingCoeff: Double; thresholdGain: Double): Pointer { NSString }; cdecl; external libWebRTC name _PU + 'RTCFieldTrialTrendlineFilterValue';
//procedure RTCEnableMetrics; cdecl; external libWebRTC name _PU + 'RTCEnableMetrics';
//function RTCGetAndResetMetrics: Pointer { NSArray }; cdecl; external libWebRTC name _PU + 'RTCGetAndResetMetrics';


implementation

//{*********************************************}
//function kRTCAudioSessionErrorDomain: NSString;
//begin
  //Result := CocoaNSStringConst(libWebRTC, 'kRTCAudioSessionErrorDomain');
//end;

//{***********************************************}
//function kRTCMediaStreamTrackKindAudio: NSString;
//begin
  //Result := CocoaNSStringConst(libWebRTC, 'kRTCMediaStreamTrackKindAudio');
//end;

//{***********************************************}
//function kRTCMediaStreamTrackKindVideo: NSString;
//begin
  //Result := CocoaNSStringConst(libWebRTC, 'kRTCMediaStreamTrackKindVideo');
//end;

//{***************************************************}
//function kRTCFieldTrialAudioSendSideBweKey: NSString;
//begin
  //Result := CocoaNSStringConst(libWebRTC, 'kRTCFieldTrialAudioSendSideBweKey');
//end;

//{***********************************************************}
//function kRTCFieldTrialAudioSendSideBweForVideoKey: NSString;
//begin
  //Result := CocoaNSStringConst(libWebRTC, 'kRTCFieldTrialAudioSendSideBweForVideoKey');
//end;

//{***************************************************}
//function kRTCFieldTrialAudioForceNoTWCCKey: NSString;
//begin
  //Result := CocoaNSStringConst(libWebRTC, 'kRTCFieldTrialAudioForceNoTWCCKey');
//end;

//{*******************************************************}
//function kRTCFieldTrialAudioForceABWENoTWCCKey: NSString;
//begin
  //Result := CocoaNSStringConst(libWebRTC, 'kRTCFieldTrialAudioForceABWENoTWCCKey');
//end;

//{**********************************************************}
//function kRTCFieldTrialSendSideBweWithOverheadKey: NSString;
//begin
  //Result := CocoaNSStringConst(libWebRTC, 'kRTCFieldTrialSendSideBweWithOverheadKey');
//end;

//{******************************************************}
//function kRTCFieldTrialFlexFec03AdvertisedKey: NSString;
//begin
  //Result := CocoaNSStringConst(libWebRTC, 'kRTCFieldTrialFlexFec03AdvertisedKey');
//end;

//{********************************************}
//function kRTCFieldTrialFlexFec03Key: NSString;
//begin
  //Result := CocoaNSStringConst(libWebRTC, 'kRTCFieldTrialFlexFec03Key');
//end;

//{**********************************************************}
//function kRTCFieldTrialImprovedBitrateEstimateKey: NSString;
//begin
  //Result := CocoaNSStringConst(libWebRTC, 'kRTCFieldTrialImprovedBitrateEstimateKey');
//end;

//{**************************************************}
//function kRTCFieldTrialH264HighProfileKey: NSString;
//begin
  //Result := CocoaNSStringConst(libWebRTC, 'kRTCFieldTrialH264HighProfileKey');
//end;

//{*************************************************************}
//function kRTCFieldTrialMinimizeResamplingOnMobileKey: NSString;
//begin
  //Result := CocoaNSStringConst(libWebRTC, 'kRTCFieldTrialMinimizeResamplingOnMobileKey');
//end;

//{********************************************}
//function kRTCFieldTrialEnabledValue: NSString;
//begin
  //Result := CocoaNSStringConst(libWebRTC, 'kRTCFieldTrialEnabledValue');
//end;

//{****************************************************}
//function kRTCFieldTrialMedianSlopeFilterKey: NSString;
//begin
  //Result := CocoaNSStringConst(libWebRTC, 'kRTCFieldTrialMedianSlopeFilterKey');
//end;

//{**************************************************}
//function kRTCFieldTrialTrendlineFilterKey: NSString;
//begin
  //Result := CocoaNSStringConst(libWebRTC, 'kRTCFieldTrialTrendlineFilterKey');
//end;

//{****************************************}
//function kRTCVideoCodecH264Name: NSString;
//begin
  //Result := CocoaNSStringConst(libWebRTC, 'kRTCVideoCodecH264Name');
//end;

//{********************************************}
//function kRTCLevel31ConstrainedHigh: NSString;
//begin
  //Result := CocoaNSStringConst(libWebRTC, 'kRTCLevel31ConstrainedHigh');
//end;

//{************************************************}
//function kRTCLevel31ConstrainedBaseline: NSString;
//begin
  //Result := CocoaNSStringConst(libWebRTC, 'kRTCLevel31ConstrainedBaseline');
//end;

//{*****************************************************************}
//function kRTCMaxSupportedH264ProfileLevelConstrainedHigh: NSString;
//begin
  //Result := CocoaNSStringConst(libWebRTC, 'kRTCMaxSupportedH264ProfileLevelConstrainedHigh');
//end;

//{*********************************************************************}
//function kRTCMaxSupportedH264ProfileLevelConstrainedBaseline: NSString;
//begin
  //Result := CocoaNSStringConst(libWebRTC, 'kRTCMaxSupportedH264ProfileLevelConstrainedBaseline');
//end;

//{****************************************************}
//function kRTCMediaConstraintsMinAspectRatio: NSString;
//begin
  //Result := CocoaNSStringConst(libWebRTC, 'kRTCMediaConstraintsMinAspectRatio');
//end;

//{****************************************************}
//function kRTCMediaConstraintsMaxAspectRatio: NSString;
//begin
  //Result := CocoaNSStringConst(libWebRTC, 'kRTCMediaConstraintsMaxAspectRatio');
//end;

//{**********************************************}
//function kRTCMediaConstraintsMaxWidth: NSString;
//begin
  //Result := CocoaNSStringConst(libWebRTC, 'kRTCMediaConstraintsMaxWidth');
//end;

//{**********************************************}
//function kRTCMediaConstraintsMinWidth: NSString;
//begin
  //Result := CocoaNSStringConst(libWebRTC, 'kRTCMediaConstraintsMinWidth');
//end;

//{***********************************************}
//function kRTCMediaConstraintsMaxHeight: NSString;
//begin
  //Result := CocoaNSStringConst(libWebRTC, 'kRTCMediaConstraintsMaxHeight');
//end;

//{***********************************************}
//function kRTCMediaConstraintsMinHeight: NSString;
//begin
  //Result := CocoaNSStringConst(libWebRTC, 'kRTCMediaConstraintsMinHeight');
//end;

//{**************************************************}
//function kRTCMediaConstraintsMaxFrameRate: NSString;
//begin
  //Result := CocoaNSStringConst(libWebRTC, 'kRTCMediaConstraintsMaxFrameRate');
//end;

//{**************************************************}
//function kRTCMediaConstraintsMinFrameRate: NSString;
//begin
  //Result := CocoaNSStringConst(libWebRTC, 'kRTCMediaConstraintsMinFrameRate');
//end;

//{***************************************************************}
//function kRTCMediaConstraintsAudioNetworkAdaptorConfig: NSString;
//begin
  //Result := CocoaNSStringConst(libWebRTC, 'kRTCMediaConstraintsAudioNetworkAdaptorConfig');
//end;

//{************************************************}
//function kRTCMediaConstraintsIceRestart: NSString;
//begin
  //Result := CocoaNSStringConst(libWebRTC, 'kRTCMediaConstraintsIceRestart');
//end;

//{*********************************************************}
//function kRTCMediaConstraintsOfferToReceiveAudio: NSString;
//begin
  //Result := CocoaNSStringConst(libWebRTC, 'kRTCMediaConstraintsOfferToReceiveAudio');
//end;

//{*********************************************************}
//function kRTCMediaConstraintsOfferToReceiveVideo: NSString;
//begin
  //Result := CocoaNSStringConst(libWebRTC, 'kRTCMediaConstraintsOfferToReceiveVideo');
//end;

//{************************************************************}
//function kRTCMediaConstraintsVoiceActivityDetection: NSString;
//begin
  //Result := CocoaNSStringConst(libWebRTC, 'kRTCMediaConstraintsVoiceActivityDetection');
//end;

//{***********************************************}
//function kRTCMediaConstraintsValueTrue: NSString;
//begin
  //Result := CocoaNSStringConst(libWebRTC, 'kRTCMediaConstraintsValueTrue');
//end;

//{************************************************}
//function kRTCMediaConstraintsValueFalse: NSString;
//begin
  //Result := CocoaNSStringConst(libWebRTC, 'kRTCMediaConstraintsValueFalse');
//end;

//{***********************************************}
//function kRTCPeerConnectionErrorDomain: NSString;
//begin
  //Result := CocoaNSStringConst(libWebRTC, 'kRTCPeerConnectionErrorDomain');
//end;

//{**********************************}
//function kRTCRtxCodecName: NSString;
//begin
  //Result := CocoaNSStringConst(libWebRTC, 'kRTCRtxCodecName');
//end;

//{**********************************}
//function kRTCRedCodecName: NSString;
//begin
  //Result := CocoaNSStringConst(libWebRTC, 'kRTCRedCodecName');
//end;

//{*************************************}
//function kRTCUlpfecCodecName: NSString;
//begin
  //Result := CocoaNSStringConst(libWebRTC, 'kRTCUlpfecCodecName');
//end;

//{**************************************}
//function kRTCFlexfecCodecName: NSString;
//begin
  //Result := CocoaNSStringConst(libWebRTC, 'kRTCFlexfecCodecName');
//end;

//{***********************************}
//function kRTCOpusCodecName: NSString;
//begin
  //Result := CocoaNSStringConst(libWebRTC, 'kRTCOpusCodecName');
//end;

//{***********************************}
//function kRTCIsacCodecName: NSString;
//begin
  //Result := CocoaNSStringConst(libWebRTC, 'kRTCIsacCodecName');
//end;

//{**********************************}
//function kRTCL16CodecName: NSString;
//begin
  //Result := CocoaNSStringConst(libWebRTC, 'kRTCL16CodecName');
//end;

//{***********************************}
//function kRTCG722CodecName: NSString;
//begin
  //Result := CocoaNSStringConst(libWebRTC, 'kRTCG722CodecName');
//end;

//{***********************************}
//function kRTCIlbcCodecName: NSString;
//begin
  //Result := CocoaNSStringConst(libWebRTC, 'kRTCIlbcCodecName');
//end;

//{***********************************}
//function kRTCPcmuCodecName: NSString;
//begin
  //Result := CocoaNSStringConst(libWebRTC, 'kRTCPcmuCodecName');
//end;

//{***********************************}
//function kRTCPcmaCodecName: NSString;
//begin
  //Result := CocoaNSStringConst(libWebRTC, 'kRTCPcmaCodecName');
//end;

//{***********************************}
//function kRTCDtmfCodecName: NSString;
//begin
  //Result := CocoaNSStringConst(libWebRTC, 'kRTCDtmfCodecName');
//end;

//{*******************************************}
//function kRTCComfortNoiseCodecName: NSString;
//begin
  //Result := CocoaNSStringConst(libWebRTC, 'kRTCComfortNoiseCodecName');
//end;

//{**********************************}
//function kRTCVp8CodecName: NSString;
//begin
  //Result := CocoaNSStringConst(libWebRTC, 'kRTCVp8CodecName');
//end;

//{**********************************}
//function kRTCVp9CodecName: NSString;
//begin
  //Result := CocoaNSStringConst(libWebRTC, 'kRTCVp9CodecName');
//end;

//{***********************************}
//function kRTCH264CodecName: NSString;
//begin
  //Result := CocoaNSStringConst(libWebRTC, 'kRTCH264CodecName');
//end;

//{***************************************}
//function kRTCVideoCodecVp8Name: NSString;
//begin
  //Result := CocoaNSStringConst(libWebRTC, 'kRTCVideoCodecVp8Name');
//end;

//{***************************************}
//function kRTCVideoCodecVp9Name: NSString;
//begin
  //Result := CocoaNSStringConst(libWebRTC, 'kRTCVideoCodecVp9Name');
//end;

//{**************************************************}
//function kRTCAudioSessionErrorLockRequired: Pointer;
//begin
  //Result := CocoaPointerConst(libWebRTC, 'kRTCAudioSessionErrorLockRequired');
//end;

//{***************************************************}
//function kRTCAudioSessionErrorConfiguration: Pointer;
//begin
  //Result := CocoaPointerConst(libWebRTC, 'kRTCAudioSessionErrorConfiguration');
//end;

//{**********************************************************}
//function kRTCAudioSessionPreferredNumberOfChannels: Pointer;
//begin
  //Result := CocoaPointerConst(libWebRTC, 'kRTCAudioSessionPreferredNumberOfChannels');
//end;

//{**********************************************************}
//function kRTCAudioSessionHighPerformanceSampleRate: Pointer;
//begin
  //Result := CocoaPointerConst(libWebRTC, 'kRTCAudioSessionHighPerformanceSampleRate');
//end;

//{********************************************************}
//function kRTCAudioSessionLowComplexitySampleRate: Pointer;
//begin
  //Result := CocoaPointerConst(libWebRTC, 'kRTCAudioSessionLowComplexitySampleRate');
//end;

//{****************************************************************}
//function kRTCAudioSessionHighPerformanceIOBufferDuration: Pointer;
//begin
  //Result := CocoaPointerConst(libWebRTC, 'kRTCAudioSessionHighPerformanceIOBufferDuration');
//end;

//{**************************************************************}
//function kRTCAudioSessionLowComplexityIOBufferDuration: Pointer;
//begin
  //Result := CocoaPointerConst(libWebRTC, 'kRTCAudioSessionLowComplexityIOBufferDuration');
//end;

//{************************************************}
//function kRTCSessionDescriptionErrorCode: Pointer;
//begin
  //Result := CocoaPointerConst(libWebRTC, 'kRTCSessionDescriptionErrorCode');
//end;

{$IF defined(CPUARM)}

procedure StubProc1;  cdecl; external 'WebRTC' name 'OBJC_CLASS_$_RTCPeerConnectionFactory';

{$ELSE}

// i don't know how to do under ios simulator :(

{$ENDIF}

end.
