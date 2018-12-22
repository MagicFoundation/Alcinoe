unit ALAndroidWebRTCApi;

interface

uses Androidapi.JNIBridge,
     Androidapi.JNI.GraphicsContentViewText,
     Androidapi.JNI.JavaTypes,
     Androidapi.JNI.App;

type

  {****************************************}
  JPeerConnection_TlsCertPolicy = interface;
  JPeerConnection_IceServer = interface;
  JPeerConnection_IceServer_Builder = interface;
  JPeerConnection_IceConnectionState = interface;
  JIceCandidate = interface;
  JSessionDescription_Type = interface;
  JSessionDescription = interface;
  JALWebRTC_Listener = interface;
  JALWebRTC_PeerConnectionParameters = interface;
  JALWebRTC = interface;

  {********************************************************}
  JPeerConnection_TlsCertPolicyClass = interface(JEnumClass)
    ['{A9BDA269-B178-459A-8892-FC9D31652D8A}']
    {class} function _GetTLS_CERT_POLICY_INSECURE_NO_CHECK: JPeerConnection_TlsCertPolicy; cdecl;
    {class} function _GetTLS_CERT_POLICY_SECURE: JPeerConnection_TlsCertPolicy; cdecl;
    {class} //function valueOf(name: JString): JPeerConnection_TlsCertPolicy; cdecl;
    {class} //function values: TJavaObjectArray<JPeerConnection_TlsCertPolicy>; cdecl;
    {class} property TLS_CERT_POLICY_INSECURE_NO_CHECK: JPeerConnection_TlsCertPolicy read _GetTLS_CERT_POLICY_INSECURE_NO_CHECK;
    {class} property TLS_CERT_POLICY_SECURE: JPeerConnection_TlsCertPolicy read _GetTLS_CERT_POLICY_SECURE;
  end;

  {********************************************************}
  [JavaSignature('org/webrtc/PeerConnection$TlsCertPolicy')]
  JPeerConnection_TlsCertPolicy = interface(JEnum)
    ['{96BA5191-41C9-4968-98CA-38F71EF07FC0}']
  end;
  TJPeerConnection_TlsCertPolicy = class(TJavaGenericImport<JPeerConnection_TlsCertPolicyClass, JPeerConnection_TlsCertPolicy>) end;

  {******************************************************}
  JPeerConnection_IceServerClass = interface(JObjectClass)
    ['{3D0BB0FF-EFD1-429F-9C66-C9EB712F0B12}']
    {class} function builder(uri: JString): JPeerConnection_IceServer_Builder; cdecl; overload;
    {class} function builder(urls: JList): JPeerConnection_IceServer_Builder; cdecl; overload;
    {class} function init(uri: JString): JPeerConnection_IceServer; cdecl; overload; deprecated;
    {class} function init(uri: JString; username: JString; password: JString): JPeerConnection_IceServer; cdecl; overload; deprecated;
    {class} function init(uri: JString; username: JString; password: JString; tlsCertPolicy: JPeerConnection_TlsCertPolicy): JPeerConnection_IceServer; cdecl; overload; deprecated;
    {class} function init(uri: JString; username: JString; password: JString; tlsCertPolicy: JPeerConnection_TlsCertPolicy; hostname: JString): JPeerConnection_IceServer; cdecl; overload; deprecated;
  end;

  {****************************************************}
  [JavaSignature('org/webrtc/PeerConnection$IceServer')]
  JPeerConnection_IceServer = interface(JObject)
    ['{D31A3643-8680-4230-B255-D8E1E1220D2D}']
    function _Gethostname: JString; cdecl;
    function _Getpassword: JString; cdecl;
    function _GettlsAlpnProtocols: JList; cdecl;
    function _GettlsCertPolicy: JPeerConnection_TlsCertPolicy; cdecl;
    function _GettlsEllipticCurves: JList; cdecl;
    function _Geturi: JString; cdecl;
    function _Geturls: JList; cdecl;
    function _Getusername: JString; cdecl;
    function toString: JString; cdecl;
    property hostname: JString read _Gethostname;
    property password: JString read _Getpassword;
    property tlsAlpnProtocols: JList read _GettlsAlpnProtocols;
    property tlsCertPolicy: JPeerConnection_TlsCertPolicy read _GettlsCertPolicy;
    property tlsEllipticCurves: JList read _GettlsEllipticCurves;
    property uri: JString read _Geturi; // deprecated
    property urls: JList read _Geturls;
    property username: JString read _Getusername;
  end;
  TJPeerConnection_IceServer = class(TJavaGenericImport<JPeerConnection_IceServerClass, JPeerConnection_IceServer>) end;

  {**************************************************************}
  JPeerConnection_IceServer_BuilderClass = interface(JObjectClass)
    ['{70F73023-A27C-4503-8EF6-794EB98332B4}']
  end;

  {************************************************************}
  [JavaSignature('org/webrtc/PeerConnection$IceServer$Builder')]
  JPeerConnection_IceServer_Builder = interface(JObject)
    ['{F7153AF4-045D-42E7-BCB2-CA8B29A01160}']
    function createIceServer: JPeerConnection_IceServer; cdecl;
    function setHostname(hostname: JString): JPeerConnection_IceServer_Builder; cdecl;
    function setPassword(password: JString): JPeerConnection_IceServer_Builder; cdecl;
    function setTlsAlpnProtocols(tlsAlpnProtocols: JList): JPeerConnection_IceServer_Builder; cdecl;
    function setTlsCertPolicy(tlsCertPolicy: JPeerConnection_TlsCertPolicy): JPeerConnection_IceServer_Builder; cdecl;
    function setTlsEllipticCurves(tlsEllipticCurves: JList): JPeerConnection_IceServer_Builder; cdecl;
    function setUsername(username: JString): JPeerConnection_IceServer_Builder; cdecl;
  end;
  TJPeerConnection_IceServer_Builder = class(TJavaGenericImport<JPeerConnection_IceServer_BuilderClass, JPeerConnection_IceServer_Builder>) end;

  {*************************************************************}
  JPeerConnection_IceConnectionStateClass = interface(JEnumClass)
    ['{8DE67926-D8D5-4034-808D-8CFDF8CC3CE8}']
    {class} function _GetCHECKING: JPeerConnection_IceConnectionState; cdecl;
    {class} function _GetCLOSED: JPeerConnection_IceConnectionState; cdecl;
    {class} function _GetCOMPLETED: JPeerConnection_IceConnectionState; cdecl;
    {class} function _GetCONNECTED: JPeerConnection_IceConnectionState; cdecl;
    {class} function _GetDISCONNECTED: JPeerConnection_IceConnectionState; cdecl;
    {class} function _GetFAILED: JPeerConnection_IceConnectionState; cdecl;
    {class} function _GetNEW: JPeerConnection_IceConnectionState; cdecl;
    {class} //function valueOf(name: JString): JPeerConnection_IceConnectionState; cdecl;
    {class} //function values: TJavaObjectArray<JPeerConnection_IceConnectionState>; cdecl;
    {class} property CHECKING: JPeerConnection_IceConnectionState read _GetCHECKING;
    {class} property CLOSED: JPeerConnection_IceConnectionState read _GetCLOSED;
    {class} property COMPLETED: JPeerConnection_IceConnectionState read _GetCOMPLETED;
    {class} property CONNECTED: JPeerConnection_IceConnectionState read _GetCONNECTED;
    {class} property DISCONNECTED: JPeerConnection_IceConnectionState read _GetDISCONNECTED;
    {class} property FAILED: JPeerConnection_IceConnectionState read _GetFAILED;
    {class} property NEW: JPeerConnection_IceConnectionState read _GetNEW;
  end;

  {*************************************************************}
  [JavaSignature('org/webrtc/PeerConnection$IceConnectionState')]
  JPeerConnection_IceConnectionState = interface(JEnum)
    ['{8C929E60-7AE3-4881-867B-9D6041DF2467}']
  end;
  TJPeerConnection_IceConnectionState = class(TJavaGenericImport<JPeerConnection_IceConnectionStateClass, JPeerConnection_IceConnectionState>) end;

  {******************************************}
  JIceCandidateClass = interface(JObjectClass)
    ['{A403A5B2-A9E4-4FC8-A0C9-F9EB2193F02B}']
    {class} function init(sdpMid: JString; sdpMLineIndex: Integer; sdp: JString): JIceCandidate; cdecl; overload;
  end;

  {****************************************}
  [JavaSignature('org/webrtc/IceCandidate')]
  JIceCandidate = interface(JObject)
    ['{F35CB50D-E2AB-4C86-9877-B3B311C944C6}']
    function _GetsdpMid: JString; cdecl;
    function _GetsdpMLineIndex: Integer; cdecl;
    function _Getsdp: JString; cdecl;
    function _GetserverUrl: JString; cdecl;
    function toString: JString; cdecl;
    property sdpMid: JString read _GetsdpMid;
    property sdpMLineIndex: Integer read _GetsdpMLineIndex;
    property sdp: JString read _Getsdp;
    property serverUrl: JString read _GetserverUrl;
  end;
  TJIceCandidate = class(TJavaGenericImport<JIceCandidateClass, JIceCandidate>) end;

  {***************************************************}
  JSessionDescription_TypeClass = interface(JEnumClass)
    ['{DBA47293-EFE0-4ABE-9402-6BBB906689EF}']
    {class} function _GetANSWER: JSessionDescription_Type; cdecl;
    {class} function _GetOFFER: JSessionDescription_Type; cdecl;
    {class} function _GetPRANSWER: JSessionDescription_Type; cdecl;
    {class} //function valueOf(name: JString): JSessionDescription_Type; cdecl;
    {class} //function values: TJavaObjectArray<JSessionDescription_Type>; cdecl;
    {class} function fromCanonicalForm(canonical: JString): JSessionDescription_Type; cdecl;
    {class} property ANSWER: JSessionDescription_Type read _GetANSWER;
    {class} property OFFER: JSessionDescription_Type read _GetOFFER;
    {class} property PRANSWER: JSessionDescription_Type read _GetPRANSWER;
  end;

  {***************************************************}
  [JavaSignature('org/webrtc/SessionDescription$Type')]
  JSessionDescription_Type = interface(JEnum)
    ['{D19319E6-779F-4468-AF80-48D0586F584E}']
    function canonicalForm: JString; cdecl;
  end;
  TJSessionDescription_Type = class(TJavaGenericImport<JSessionDescription_TypeClass, JSessionDescription_Type>) end;


  {************************************************}
  JSessionDescriptionClass = interface(JObjectClass)
    ['{0C213174-3BB0-44AE-AD47-325249363921}']
    {class} function init(&type: JSessionDescription_Type; description: JString): JSessionDescription; cdecl;
  end;

  {**********************************************}
  [JavaSignature('org/webrtc/SessionDescription')]
  JSessionDescription = interface(JObject)
    ['{0AD8B7AA-9D94-42FC-98EA-5754BBB3CB32}']
    function _Getdescription: JString; cdecl;
    function _Gettype: JSessionDescription_Type; cdecl;
    property description: JString read _Getdescription;
    property &type: JSessionDescription_Type read _Gettype;
  end;
  TJSessionDescription = class(TJavaGenericImport<JSessionDescriptionClass, JSessionDescription>) end;

  {*********************************************}
  JALWebRTC_ListenerClass = interface(IJavaClass)
    ['{90F484CB-D786-42A5-BF8F-E2F1A4339D19}']
  end;

  {*****************************************************}
  [JavaSignature('com/alcinoe/webrtc/ALWebRTC$Listener')]
  JALWebRTC_Listener = interface(IJavaInstance)
    ['{C758A92B-AE93-4291-BFD5-1EBB83041CC9}']
    procedure onLocalFrameAvailable(textureId: integer; width: integer; height: integer; rotation: integer); cdecl;
    procedure onRemoteFrameAvailable(textureId: integer; width: integer; height: integer; rotation: integer); cdecl;
    procedure onLocalDescription(sdp: JSessionDescription); cdecl;
    procedure onIceCandidate(candidate: JIceCandidate); cdecl;
    procedure onIceCandidatesRemoved(candidates: TJavaObjectArray<JIceCandidate>); cdecl;
    procedure onIceConnectionChange(newState: JPeerConnection_IceConnectionState); cdecl;
    procedure onError(code: Integer; description: JString); cdecl;
  end;
  TJALWebRTC_Listener = class(TJavaGenericImport<JALWebRTC_ListenerClass, JALWebRTC_Listener>) end;

  {***************************************************************}
  JALWebRTC_PeerConnectionParametersClass = interface(JObjectClass)
    ['{0775B3FE-432A-41F5-BAC0-C9C2609AB4E1}']
    {class} function init: JALWebRTC_PeerConnectionParameters; cdecl;
  end;

  {*********************************************************************}
  [JavaSignature('com/alcinoe/webrtc/ALWebRTC$PeerConnectionParameters')]
  JALWebRTC_PeerConnectionParameters = interface(JObject)
    ['{AF3349B3-C8B9-4CA4-8D64-7E8711E7A2F4}']
    function _GetaecDump: Boolean; cdecl;
    procedure _SetaecDump(Value: Boolean); cdecl;
    function _GetaudioCodec: JString; cdecl;
    procedure _SetaudioCodec(Value: JString); cdecl;
    function _GetaudioStartBitrate: Integer; cdecl;
    procedure _SetaudioStartBitrate(Value: Integer); cdecl;
    function _GetdataChannelEnabled: Boolean; cdecl;
    procedure _SetdataChannelEnabled(Value: Boolean); cdecl;
    function _GetdataChannelId: Integer; cdecl;
    procedure _SetdataChannelId(Value: Integer); cdecl;
    function _GetdataChannelMaxRetransmitTimeMs: Integer; cdecl;
    procedure _SetdataChannelMaxRetransmitTimeMs(Value: Integer); cdecl;
    function _GetdataChannelMaxRetransmits: Integer; cdecl;
    procedure _SetdataChannelMaxRetransmits(Value: Integer); cdecl;
    function _GetdataChannelNegotiated: Boolean; cdecl;
    procedure _SetdataChannelNegotiated(Value: Boolean); cdecl;
    function _GetdataChannelOrdered: Boolean; cdecl;
    procedure _SetdataChannelOrdered(Value: Boolean); cdecl;
    function _GetdataChannelProtocol: JString; cdecl;
    procedure _SetdataChannelProtocol(Value: JString); cdecl;
    function _GetdisableBuiltInAEC: Boolean; cdecl;
    procedure _SetdisableBuiltInAEC(Value: Boolean); cdecl;
    function _GetdisableBuiltInNS: Boolean; cdecl;
    procedure _SetdisableBuiltInNS(Value: Boolean); cdecl;
    function _GetnoAudioProcessing: Boolean; cdecl;
    procedure _SetnoAudioProcessing(Value: Boolean); cdecl;
    function _GetvideoCallEnabled: Boolean; cdecl;
    procedure _SetvideoCallEnabled(Value: Boolean); cdecl;
    function _GetvideoCodec: JString; cdecl;
    procedure _SetvideoCodec(Value: JString); cdecl;
    function _GetvideoCodecHwAcceleration: Boolean; cdecl;
    procedure _SetvideoCodecHwAcceleration(Value: Boolean); cdecl;
    function _GetvideoFps: Integer; cdecl;
    procedure _SetvideoFps(Value: Integer); cdecl;
    function _GetvideoHeight: Integer; cdecl;
    procedure _SetvideoHeight(Value: Integer); cdecl;
    function _GetvideoMaxBitrate: Integer; cdecl;
    procedure _SetvideoMaxBitrate(Value: Integer); cdecl;
    function _GetvideoWidth: Integer; cdecl;
    procedure _SetvideoWidth(Value: Integer); cdecl;
    property videoCallEnabled: Boolean read _GetvideoCallEnabled write _SetvideoCallEnabled;
    property videoWidth: Integer read _GetvideoWidth write _SetvideoWidth;
    property videoHeight: Integer read _GetvideoHeight write _SetvideoHeight;
    property videoFps: Integer read _GetvideoFps write _SetvideoFps;
    property videoMaxBitrate: Integer read _GetvideoMaxBitrate write _SetvideoMaxBitrate;
    property videoCodec: JString read _GetvideoCodec write _SetvideoCodec;
    property videoCodecHwAcceleration: Boolean read _GetvideoCodecHwAcceleration write _SetvideoCodecHwAcceleration;
    property audioStartBitrate: Integer read _GetaudioStartBitrate write _SetaudioStartBitrate;
    property audioCodec: JString read _GetaudioCodec write _SetaudioCodec;
    property noAudioProcessing: Boolean read _GetnoAudioProcessing write _SetnoAudioProcessing;
    property aecDump: Boolean read _GetaecDump write _SetaecDump;
    property disableBuiltInAEC: Boolean read _GetdisableBuiltInAEC write _SetdisableBuiltInAEC;
    property disableBuiltInNS: Boolean read _GetdisableBuiltInNS write _SetdisableBuiltInNS;
    property dataChannelEnabled: Boolean read _GetdataChannelEnabled write _SetdataChannelEnabled;
    property dataChannelOrdered: Boolean read _GetdataChannelOrdered write _SetdataChannelOrdered;
    property dataChannelMaxRetransmitTimeMs: Integer read _GetdataChannelMaxRetransmitTimeMs write _SetdataChannelMaxRetransmitTimeMs;
    property dataChannelMaxRetransmits: Integer read _GetdataChannelMaxRetransmits write _SetdataChannelMaxRetransmits;
    property dataChannelProtocol: JString read _GetdataChannelProtocol write _SetdataChannelProtocol;
    property dataChannelNegotiated: Boolean read _GetdataChannelNegotiated write _SetdataChannelNegotiated;
    property dataChannelId: Integer read _GetdataChannelId write _SetdataChannelId;
  end;
  TJALWebRTC_PeerConnectionParameters = class(TJavaGenericImport<JALWebRTC_PeerConnectionParametersClass, JALWebRTC_PeerConnectionParameters>) end;

  {**************************************}
  JALWebRTCClass = interface(JObjectClass)
    ['{3F5C8CF8-CEB7-4C92-B6E4-FD43F27BF995}']
    {class} procedure initializeLibrary(appContext: JContext); cdecl;
    {class} procedure finalizeLibrary; cdecl;
    {class} function init(appContext: JContext; eglContext: int64; iceServers: JList; peerConnectionParameters: JALWebRTC_PeerConnectionParameters): JALWebRTC; cdecl;
  end;

  {********************************************}
  [JavaSignature('com/alcinoe/webrtc/ALWebRTC')]
  JALWebRTC = interface(JObject)
    ['{45B6132D-E7E4-4D39-A150-E489495FC19D}']
    procedure setListener(listener: JALWebRTC_Listener); cdecl;
    function start: boolean; cdecl;
    procedure stop; cdecl;
    procedure setAudioEnabled(enable: boolean); cdecl;
    procedure setVideoEnabled(enable: boolean); cdecl;
    function setVideoMaxBitrate(maxBitrateKbps: integer): boolean; cdecl;
    procedure createOffer; cdecl;
    procedure createAnswer; cdecl;
    procedure setRemoteDescription(sdpType: JSessionDescription_Type; sdpDescription: Jstring); cdecl;
    procedure addRemoteIceCandidate(sdpMid: JString; sdpMLineIndex: integer; sdp: JString); cdecl;
    procedure removeRemoteIceCandidates(candidates: TJavaObjectArray<JIceCandidate>); cdecl;
    procedure switchCamera; cdecl;
    procedure changeCaptureFormat(width: integer; height: integer; framerate: integer); cdecl;
  end;
  TJALWebRTC = class(TJavaGenericImport<JALWebRTCClass, JALWebRTC>) end;

implementation

procedure RegisterTypes;
begin
  TRegTypes.RegisterType('ALAndroidWebRTCApi.JPeerConnection_TlsCertPolicy', TypeInfo(ALAndroidWebRTCApi.JPeerConnection_TlsCertPolicy));
  TRegTypes.RegisterType('ALAndroidWebRTCApi.JPeerConnection_IceServer', TypeInfo(ALAndroidWebRTCApi.JPeerConnection_IceServer));
  TRegTypes.RegisterType('ALAndroidWebRTCApi.JPeerConnection_IceServer_Builder', TypeInfo(ALAndroidWebRTCApi.JPeerConnection_IceServer_Builder));
  TRegTypes.RegisterType('ALAndroidWebRTCApi.JPeerConnection_IceConnectionState', TypeInfo(ALAndroidWebRTCApi.JPeerConnection_IceConnectionState));
  TRegTypes.RegisterType('ALAndroidWebRTCApi.JIceCandidate', TypeInfo(ALAndroidWebRTCApi.JIceCandidate));
  TRegTypes.RegisterType('ALAndroidWebRTCApi.JSessionDescription_Type', TypeInfo(ALAndroidWebRTCApi.JSessionDescription_Type));
  TRegTypes.RegisterType('ALAndroidWebRTCApi.JSessionDescription', TypeInfo(ALAndroidWebRTCApi.JSessionDescription));
  TRegTypes.RegisterType('ALAndroidWebRTCApi.JALWebRTC_Listener', TypeInfo(ALAndroidWebRTCApi.JALWebRTC_Listener));
  TRegTypes.RegisterType('ALAndroidWebRTCApi.JALWebRTC_PeerConnectionParameters', TypeInfo(ALAndroidWebRTCApi.JALWebRTC_PeerConnectionParameters));
  TRegTypes.RegisterType('ALAndroidWebRTCApi.JALWebRTC', TypeInfo(ALAndroidWebRTCApi.JALWebRTC));
end;

initialization
  RegisterTypes;

end.
