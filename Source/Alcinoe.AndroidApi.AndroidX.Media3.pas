//
// Made from androidx.media3:media3-exoplayer:1.5.1
//
unit Alcinoe.AndroidApi.AndroidX.Media3;

interface

{$I Alcinoe.inc}

{$IFNDEF ALCompilerVersionSupported122}
  //Please run <Alcinoe>\Tools\NativeBridgeFileGenerator\NativeBridgeFileGeneratorAndroid.bat
  //with the library identifiers androidx.media3:media3-exoplayer:xx.xx.xx where xx.xx.xx
  //is the last version of the Media3 exoplayer (You can find this version at
  //https://maven.google.com/web/index.html#androidx.media3:media3-exoplayer) and give
  //also the path to <Alcinoe>\Source\Alcinoe.AndroidApi.AndroidX.Media3.pas to build the
  //compare source file. Then make a diff compare between the new generated
  //Alcinoe.AndroidApi.AndroidX.Media3.pas and this one to see if the api signature is still the same.
  // Note: If you encounter an error with NativeBridgeFileGenerator, refer to the workarounds at:
  // https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-3018
  {$MESSAGE WARN 'Check if the api signature of the last version of Media3 exoplayer (android) is still the same'}
{$ENDIF}

uses
  Androidapi.JNIBridge,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Os,
  Androidapi.JNI.GraphicsContentViewText;

type

  {******************}
  JPlayer = interface;
  JExoPlayer = interface;
  JExoPlayer_Builder = interface;
  JPlayer_Listener = interface;
  JPlayer_Events = interface;
  JPlayer_Commands = interface;
  JPlayer_PositionInfo = interface;
  JTransferListener = interface;
  JTimeline = interface;
  JPlaybackException = interface;
  JPlaybackParameters = interface;
  JVideoSize = interface;
  JTrackSelectionParameters = interface;
  JAudioAttributes = interface;
  JDeviceInfo = interface;
  JCueGroup = interface;
  JTracks = interface;
  JMediaMetadata = interface;
  JMetadata = interface;
  JMediaItem = interface;

  {****************************************************************************************************************}
  //https://github.com/androidx/media/blob/release/libraries/common/src/main/java/androidx/media3/common/Player.java
  JPlayerClass = interface(IJavaClass)
    ['{0D5D1A43-B6C5-4743-B083-AB82F7EFE36B}']
    {class} function _GetREPEAT_MODE_ALL: Integer; cdecl;
    {class} function _GetREPEAT_MODE_OFF: Integer; cdecl;
    {class} function _GetREPEAT_MODE_ONE: Integer; cdecl;
    {class} function _GetSTATE_BUFFERING: Integer; cdecl;
    {class} function _GetSTATE_ENDED: Integer; cdecl;
    {class} function _GetSTATE_IDLE: Integer; cdecl;
    {class} function _GetSTATE_READY: Integer; cdecl;
    {class} property REPEAT_MODE_ALL: Integer read _GetREPEAT_MODE_ALL;
    {class} property REPEAT_MODE_OFF: Integer read _GetREPEAT_MODE_OFF;
    {class} property REPEAT_MODE_ONE: Integer read _GetREPEAT_MODE_ONE;
    {class} property STATE_BUFFERING: Integer read _GetSTATE_BUFFERING;
    {class} property STATE_ENDED: Integer read _GetSTATE_ENDED;
    {class} property STATE_IDLE: Integer read _GetSTATE_IDLE;
    {class} property STATE_READY: Integer read _GetSTATE_READY;
  end;
  [JavaSignature('androidx/media3/common/Player')]
  JPlayer = interface(IJavaInstance)
    ['{B10E2FE7-CD9D-4C9C-9EEF-52D1ADC2AF57}']
    procedure addListener(listener: JPlayer_Listener); cdecl;
    procedure removeListener(listener: JPlayer_Listener); cdecl;
    procedure setPlayWhenReady(playWhenReady: Boolean); cdecl;
    function getPlayWhenReady: Boolean; cdecl;
    procedure setRepeatMode(repeatMode: Integer); cdecl;
    function getRepeatMode: Integer; cdecl;
    procedure seekTo(positionMs: Int64); cdecl;
    procedure setPlaybackParameters(playbackParameters: JPlaybackParameters); cdecl;
    function getPlaybackParameters: JPlaybackParameters; cdecl;
    procedure prepare; cdecl;
    procedure stop; cdecl;
    procedure release; cdecl;
    function getDuration: Int64; cdecl;
    function getCurrentPosition: Int64; cdecl;
    procedure clearVideoSurface; cdecl;
    procedure setVideoSurface(surface: JSurface); cdecl;
    procedure setVolume(volume: Single); cdecl;
    procedure setMediaItem(mediaItem: JMediaItem); cdecl; overload;
    procedure setMediaItem(mediaItem: JMediaItem; startPositionMs: Int64); cdecl; overload;
    procedure setMediaItem(mediaItem: JMediaItem; resetPosition: Boolean); cdecl; overload;
  end;
  TJPlayer = class(TJavaGenericImport<JPlayerClass, JPlayer>) end;

  {*************************************************************************************************************************}
  //https://github.com/androidx/media/blob/release/libraries/exoplayer/src/main/java/androidx/media3/exoplayer/ExoPlayer.java
  JExoPlayerClass = interface(JPlayerClass)
    ['{9649DCCB-24C6-4D9C-8FF1-24604611092F}']
  end;
  [JavaSignature('androidx/media3/exoplayer/ExoPlayer')]
  JExoPlayer = interface(JPlayer)
    ['{23F139A5-FEA3-450F-8FDC-039312AE95FF}']
  end;
  TJExoPlayer = class(TJavaGenericImport<JExoPlayerClass, JExoPlayer>) end;

  {***********************************************************************************************************************}
  //https://github.com/androidx/media/blob/release/libraries/exoplayer/src/main/java/androidx/media3/exoplayer/ExoPlayer.java
  JExoPlayer_BuilderClass = interface(JObjectClass)
    ['{AE382CD9-C438-41A6-A83B-3650BA28F850}']
    {class} function init(context: JContext): JExoPlayer_Builder; cdecl;
  end;
  [JavaSignature('androidx/media3/exoplayer/ExoPlayer$Builder')]
  JExoPlayer_Builder = interface(JObject)
    ['{BC387B76-5B9D-42D0-B7D8-B252089C708E}']
    function build: JExoPlayer; cdecl;
  end;
  TJExoPlayer_Builder = class(TJavaGenericImport<JExoPlayer_BuilderClass, JExoPlayer_Builder>) end;

  {****************************************************************************************************************}
  //https://github.com/androidx/media/blob/release/libraries/common/src/main/java/androidx/media3/common/Player.java
  JPlayer_ListenerClass = interface(IJavaClass)
    ['{5850B646-28C3-4143-9F28-E803AB2DB656}']
  end;
  [JavaSignature('androidx/media3/common/Player$Listener')]
  JPlayer_Listener = interface(IJavaInstance)
    ['{96103C70-D148-4255-96AC-7D0C0B638C35}']
    procedure onEvents(player: JPlayer; events: JPlayer_Events); cdecl;
    procedure onTimelineChanged(timeline: JTimeline; reason: Integer); cdecl;
    procedure onMediaItemTransition(mediaItem: JMediaItem; reason: Integer); cdecl;
    procedure onTracksChanged(tracks: JTracks); cdecl;
    procedure onMediaMetadataChanged(mediaMetadata: JMediaMetadata); cdecl;
    procedure onPlaylistMetadataChanged(mediaMetadata: JMediaMetadata); cdecl;
    procedure onIsLoadingChanged(isLoading: boolean); cdecl;
    procedure onLoadingChanged(isLoading: boolean); cdecl;
    procedure onAvailableCommandsChanged(availableCommands: JPlayer_Commands); cdecl;
    procedure onTrackSelectionParametersChanged(parameters: JTrackSelectionParameters); cdecl;
    procedure onPlayerStateChanged(playWhenReady: boolean; playbackState: Integer); cdecl;
    procedure onPlaybackStateChanged(playbackState: Integer); cdecl;
    procedure onPlayWhenReadyChanged(playWhenReady: boolean; reason: Integer); cdecl;
    procedure onPlaybackSuppressionReasonChanged(playbackSuppressionReason: Integer); cdecl;
    procedure onIsPlayingChanged(isPlaying: boolean); cdecl;
    procedure onRepeatModeChanged(repeatMode: Integer); cdecl;
    procedure onShuffleModeEnabledChanged(shuffleModeEnabled: boolean); cdecl;
    procedure onPlayerError(error: JPlaybackException); cdecl;
    procedure onPlayerErrorChanged(error: JPlaybackException); cdecl;
    procedure onPositionDiscontinuity(reason: Integer); overload; cdecl;
    procedure onPositionDiscontinuity(oldPosition: JPlayer_PositionInfo; newPosition: JPlayer_PositionInfo; reason: Integer); overload; cdecl;
    procedure onPlaybackParametersChanged(playbackParameters: JPlaybackParameters); cdecl;
    procedure onSeekBackIncrementChanged(seekBackIncrementMs: Int64); cdecl;
    procedure onSeekForwardIncrementChanged(seekForwardIncrementMs: Int64); cdecl;
    procedure onMaxSeekToPreviousPositionChanged(maxSeekToPreviousPositionMs: Int64); cdecl;
    procedure onAudioSessionIdChanged(audioSessionId: Integer); cdecl;
    procedure onAudioAttributesChanged(audioAttributes: JAudioAttributes); cdecl;
    procedure onVolumeChanged(volume: Single); cdecl;
    procedure onSkipSilenceEnabledChanged(skipSilenceEnabled: boolean); cdecl;
    procedure onDeviceInfoChanged(deviceInfo: JDeviceInfo); cdecl;
    procedure onDeviceVolumeChanged(volume: Integer; muted: boolean); cdecl;
    procedure onVideoSizeChanged(videoSize: JVideoSize); cdecl;
    procedure onSurfaceSizeChanged(width: Integer; height: Integer); cdecl;
    procedure onRenderedFirstFrame; cdecl;
    procedure onCues(cues: JList); overload; cdecl;
    procedure onCues(cueGroup: JCueGroup); overload; cdecl;
    procedure onMetadata(metadata: JMetadata); cdecl;
  end;
  TJPlayer_Listener = class(TJavaGenericImport<JPlayer_ListenerClass, JPlayer_Listener>) end;

  {****************************************************************************************************************}
  //https://github.com/androidx/media/blob/release/libraries/common/src/main/java/androidx/media3/common/Player.java
  JPlayer_EventsClass = interface(JObjectClass)
    ['{F8FE15BF-4EE9-4591-8C31-EAFA05E42EB6}']
  end;
  [JavaSignature('androidx/media3/common/Player$Events')]
  JPlayer_Events = interface(JObject)
    ['{102DE229-B107-4AFE-B526-64584F4D58F9}']
  end;
  TJPlayer_Events = class(TJavaGenericImport<JPlayer_EventsClass, JPlayer_Events>) end;

  {****************************************************************************************************************}
  //https://github.com/androidx/media/blob/release/libraries/common/src/main/java/androidx/media3/common/Player.java
  JPlayer_CommandsClass = interface(JObjectClass)
    ['{AA58B969-ACE7-4308-91D4-C9ADBF1FA11F}']
  end;
  [JavaSignature('androidx/media3/common/Player$Commands')]
  JPlayer_Commands = interface(JObject)
    ['{E927D3B4-3057-4239-85C3-FEC2483A0CDF}']
  end;
  TJPlayer_Commands = class(TJavaGenericImport<JPlayer_CommandsClass, JPlayer_Commands>) end;

  {****************************************************************************************************************}
  //https://github.com/androidx/media/blob/release/libraries/common/src/main/java/androidx/media3/common/Player.java
  JPlayer_PositionInfoClass = interface(JObjectClass)
    ['{CD3DBA2E-61C9-4EF5-A0EC-BC22569FB5B1}']
  end;
  [JavaSignature('androidx/media3/common/Player$PositionInfo')]
  JPlayer_PositionInfo = interface(JObject)
    ['{6427E33E-64C3-4EB7-B9A2-A55C7AA2CDF3}']
  end;
  TJPlayer_PositionInfo = class(TJavaGenericImport<JPlayer_PositionInfoClass, JPlayer_PositionInfo>) end;

  {***********************************************************************************************************************************}
  //https://github.com/androidx/media/blob/release/libraries/datasource/src/main/java/androidx/media3/datasource/TransferListener.java
  JTransferListenerClass = interface(IJavaClass)
    ['{9B26DEB2-564E-400A-B57D-71A56BAC7197}']
  end;
  [JavaSignature('androidx/media3/datasource/TransferListener')]
  JTransferListener = interface(IJavaInstance)
    ['{18E8ADE4-F6D9-4237-8D40-5B8CBF8D3256}']
  end;
  TJTransferListener = class(TJavaGenericImport<JTransferListenerClass, JTransferListener>) end;

  {******************************************************************************************************************}
  //https://github.com/androidx/media/blob/release/libraries/common/src/main/java/androidx/media3/common/Timeline.java
  JTimelineClass = interface(JObjectClass)
    ['{4256CEE0-95AE-4AFC-8283-CB769BA50F9F}']
  end;
  [JavaSignature('androidx/media3/common/Timeline')]
  JTimeline = interface(JObject)
    ['{979A535D-AC90-4001-9CB1-3B439BFE097B}']
  end;
  TJTimeline = class(TJavaGenericImport<JTimelineClass, JTimeline>) end;

  {***************************************************************************************************************************}
  //https://github.com/androidx/media/blob/release/libraries/common/src/main/java/androidx/media3/common/PlaybackException.java
  JPlaybackExceptionClass = interface(JExceptionClass)
    ['{CB0CA0E9-518A-44DD-8F99-9E884C2FD4E7}']
  end;
  [JavaSignature('androidx/media3/common/PlaybackException')]
  JPlaybackException = interface(JException)
    ['{678D2E2C-088A-4EB8-B72C-9B8EF657C483}']
  end;
  TJPlaybackException = class(TJavaGenericImport<JPlaybackExceptionClass, JPlaybackException>) end;

  {****************************************************************************************************************************}
  //https://github.com/androidx/media/blob/release/libraries/common/src/main/java/androidx/media3/common/PlaybackParameters.java
  JPlaybackParametersClass = interface(JObjectClass)
    ['{9081DF30-C4B5-4DB0-8FFF-08F64DF1AC03}']
    {class} function init(speed: Single; pitch: Single): JPlaybackParameters; cdecl;
  end;
  [JavaSignature('androidx/media3/common/PlaybackParameters')]
  JPlaybackParameters = interface(JObject)
    ['{1B3494CD-CC19-4A71-904D-6BEDCB31E2D9}']
  end;
  TJPlaybackParameters = class(TJavaGenericImport<JPlaybackParametersClass, JPlaybackParameters>) end;

  {*******************************************************************************************************************}
  //https://github.com/androidx/media/blob/release/libraries/common/src/main/java/androidx/media3/common/VideoSize.java
  JVideoSizeClass = interface(JObjectClass)
    ['{31E4CBE8-19B6-4CAD-8727-59C25612229C}']
  end;
  [JavaSignature('androidx/media3/common/VideoSize')]
  JVideoSize = interface(JObject)
    ['{4E874A98-8D59-434D-A390-72501A205A24}']
    function _Getheight: Integer; cdecl;
    function _Getwidth: Integer; cdecl;
    property height: Integer read _Getheight;
    property width: Integer read _Getwidth;
  end;
  TJVideoSize = class(TJavaGenericImport<JVideoSizeClass, JVideoSize>) end;

  {**********************************************************************************************************************************}
  //https://github.com/androidx/media/blob/release/libraries/common/src/main/java/androidx/media3/common/TrackSelectionParameters.java
  JTrackSelectionParametersClass = interface(JObjectClass)
    ['{EAD6046B-1C19-4C2D-9CF5-5F004F9592C1}']
  end;
  [JavaSignature('androidx/media3/common/TrackSelectionParameters')]
  JTrackSelectionParameters = interface(JObject)
    ['{4D9280D6-6561-4167-BB10-5958060EED4C}']
  end;
  TJTrackSelectionParameters = class(TJavaGenericImport<JTrackSelectionParametersClass, JTrackSelectionParameters>) end;

  {*************************************************************************************************************************}
  //https://github.com/androidx/media/blob/release/libraries/common/src/main/java/androidx/media3/common/AudioAttributes.java
  JAudioAttributesClass = interface(JObjectClass)
    ['{4E1F0BFC-6408-44E9-859B-0B360BA5C19F}']
  end;
  [JavaSignature('androidx/media3/common/AudioAttributes')]
  JAudioAttributes = interface(JObject)
    ['{CD2B93F4-0163-452A-A2FA-A8E530B88A52}']
  end;
  TJAudioAttributes = class(TJavaGenericImport<JAudioAttributesClass, JAudioAttributes>) end;

  {********************************************************************************************************************}
  //https://github.com/androidx/media/blob/release/libraries/common/src/main/java/androidx/media3/common/DeviceInfo.java
  JDeviceInfoClass = interface(JObjectClass)
    ['{C366CD94-DE9A-4E27-8F1F-FADEC3DA57AB}']
  end;
  [JavaSignature('androidx/media3/common/DeviceInfo')]
  JDeviceInfo = interface(JObject)
    ['{9323FB5B-EE90-4752-BA7C-D3070A4421D8}']
  end;
  TJDeviceInfo = class(TJavaGenericImport<JDeviceInfoClass, JDeviceInfo>) end;

  {***********************************************************************************************************************}
  //https://github.com/androidx/media/blob/release/libraries/common/src/main/java/androidx/media3/common/text/CueGroup.java
  JCueGroupClass = interface(JObjectClass)
    ['{A2304136-A15B-4AA0-A7E5-EEADF43C8E1A}']
  end;
  [JavaSignature('androidx/media3/common/text/CueGroup')]
  JCueGroup = interface(JObject)
    ['{D03E32CC-FCDA-465B-A9F6-20890FEF5227}']
  end;
  TJCueGroup = class(TJavaGenericImport<JCueGroupClass, JCueGroup>) end;

  {****************************************************************************************************************}
  //https://github.com/androidx/media/blob/release/libraries/common/src/main/java/androidx/media3/common/Tracks.java
  JTracksClass = interface(JObjectClass)
    ['{9BC81915-224E-4D28-A312-762911FA5E4B}']
  end;
  [JavaSignature('androidx/media3/common/Tracks')]
  JTracks = interface(JObject)
    ['{379DBB84-E60D-4C34-A51B-DDAD6C32A612}']
  end;
  TJTracks = class(TJavaGenericImport<JTracksClass, JTracks>) end;

  {***********************************************************************************************************************}
  //https://github.com/androidx/media/blob/release/libraries/common/src/main/java/androidx/media3/common/MediaMetadata.java
  JMediaMetadataClass = interface(JObjectClass)
    ['{AE9FA6AC-73CD-417B-8507-B199CCC8F5FA}']
  end;
  [JavaSignature('androidx/media3/common/MediaMetadata')]
  JMediaMetadata = interface(JObject)
    ['{26473517-35E2-4FE4-B9C6-9EB8AD5172A1}']
  end;
  TJMediaMetadata = class(TJavaGenericImport<JMediaMetadataClass, JMediaMetadata>) end;

  {******************************************************************************************************************}
  //https://github.com/androidx/media/blob/release/libraries/common/src/main/java/androidx/media3/common/Metadata.java
  JMetadataClass = interface(JParcelableClass)
    ['{B829B0F7-CDF4-4F06-9464-F9258095A76B}']
  end;
  [JavaSignature('androidx/media3/common/Metadata')]
  JMetadata = interface(JParcelable)
    ['{4753A269-75F1-46DD-88D8-106FAED90EBE}']
  end;
  TJMetadata = class(TJavaGenericImport<JMetadataClass, JMetadata>) end;

  {*******************************************************************************************************************}
  //https://github.com/androidx/media/blob/release/libraries/common/src/main/java/androidx/media3/common/MediaItem.java
  JMediaItemClass = interface(JObjectClass)
    ['{5C529D21-3854-4EB6-9221-89878D56240D}']
    {class} function fromUri(uri: JString): JMediaItem; cdecl;
  end;
  [JavaSignature('androidx/media3/common/MediaItem')]
  JMediaItem = interface(JObject)
    ['{127279C9-9678-4735-9597-57D1579CB7C3}']
  end;
  TJMediaItem = class(TJavaGenericImport<JMediaItemClass, JMediaItem>) end;

implementation

{**********************}
procedure RegisterTypes;
begin
  TRegTypes.RegisterType('Alcinoe.AndroidApi.AndroidX.Media3.JPlayer', TypeInfo(Alcinoe.AndroidApi.AndroidX.Media3.JPlayer));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.AndroidX.Media3.JExoPlayer', TypeInfo(Alcinoe.AndroidApi.AndroidX.Media3.JExoPlayer));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.AndroidX.Media3.JExoPlayer_Builder', TypeInfo(Alcinoe.AndroidApi.AndroidX.Media3.JExoPlayer_Builder));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.AndroidX.Media3.JPlayer_Listener', TypeInfo(Alcinoe.AndroidApi.AndroidX.Media3.JPlayer_Listener));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.AndroidX.Media3.JPlayer_Events', TypeInfo(Alcinoe.AndroidApi.AndroidX.Media3.JPlayer_Events));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.AndroidX.Media3.JPlayer_Commands', TypeInfo(Alcinoe.AndroidApi.AndroidX.Media3.JPlayer_Commands));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.AndroidX.Media3.JPlayer_PositionInfo', TypeInfo(Alcinoe.AndroidApi.AndroidX.Media3.JPlayer_PositionInfo));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.AndroidX.Media3.JTransferListener', TypeInfo(Alcinoe.AndroidApi.AndroidX.Media3.JTransferListener));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.AndroidX.Media3.JTimeline', TypeInfo(Alcinoe.AndroidApi.AndroidX.Media3.JTimeline));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.AndroidX.Media3.JPlaybackException', TypeInfo(Alcinoe.AndroidApi.AndroidX.Media3.JPlaybackException));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.AndroidX.Media3.JPlaybackParameters', TypeInfo(Alcinoe.AndroidApi.AndroidX.Media3.JPlaybackParameters));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.AndroidX.Media3.JVideoSize', TypeInfo(Alcinoe.AndroidApi.AndroidX.Media3.JVideoSize));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.AndroidX.Media3.JTrackSelectionParameters', TypeInfo(Alcinoe.AndroidApi.AndroidX.Media3.JTrackSelectionParameters));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.AndroidX.Media3.JAudioAttributes', TypeInfo(Alcinoe.AndroidApi.AndroidX.Media3.JAudioAttributes));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.AndroidX.Media3.JDeviceInfo', TypeInfo(Alcinoe.AndroidApi.AndroidX.Media3.JDeviceInfo));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.AndroidX.Media3.JCueGroup', TypeInfo(Alcinoe.AndroidApi.AndroidX.Media3.JCueGroup));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.AndroidX.Media3.JTracks', TypeInfo(Alcinoe.AndroidApi.AndroidX.Media3.JTracks));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.AndroidX.Media3.JMediaMetadata', TypeInfo(Alcinoe.AndroidApi.AndroidX.Media3.JMediaMetadata));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.AndroidX.Media3.JMetadata', TypeInfo(Alcinoe.AndroidApi.AndroidX.Media3.JMetadata));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.AndroidX.Media3.JMediaItem', TypeInfo(Alcinoe.AndroidApi.AndroidX.Media3.JMediaItem));
end;

initialization
  RegisterTypes;

end.
