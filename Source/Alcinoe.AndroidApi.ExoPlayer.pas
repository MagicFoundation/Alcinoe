//
// Made from com.google.android.exoplayer:exoplayer:2.19.1
//
unit Alcinoe.AndroidApi.ExoPlayer;

interface

{$I Alcinoe.inc}

{$IFNDEF ALCompilerVersionSupported122}
  //Please run <Alcinoe>\Tools\NativeBridgeFileGenerator\NativeBridgeFileGeneratorAndroid.bat
  //with the library identifiers com.google.android.exoplayer:exoplayer:xx.xx.xx where xx.xx.xx
  //is the last version of the exoplayer (You can find this version at
  //https://maven.google.com/web/index.html#com.google.android.exoplayer:exoplayer) and gave
  //also the path to <Alcinoe>\Source\Alcinoe.AndroidApi.ExoPlayer.pas to build the
  //compare source file. Then make a diff compare between the new generated
  //Alcinoe.AndroidApi.ExoPlayer.pas and this one to see if the api signature is still the same
  {$MESSAGE WARN 'Check if the api signature of the last version of exoplayer (android) is still the same'}
{$ENDIF}

uses
  Androidapi.JNIBridge,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Os,
  Androidapi.JNI.GraphicsContentViewText;

type

  {**********************}
  JBundleable = interface;
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
  JMediaSource = interface;
  JBaseMediaSource = interface;
  JProgressiveMediaSource = interface;
  JHlsMediaSource = interface;
  JMediaSource_Factory = interface;
  JMediaSourceFactory = interface;
  JProgressiveMediaSource_Factory = interface;
  JHlsMediaSource_Factory = interface;
  JDataSource_Factory = interface;
  JDefaultDataSource_Factory = interface;
  JMediaMetadata = interface;
  JMetadata = interface;
  JMediaItem = interface;

  {*********************************************************************************}
  //https://exoplayer.dev/doc/reference/com/google/android/exoplayer2/Bundleable.html
  //https://github.com/google/ExoPlayer/blob/release-v2/library/common/src/main/java/com/google/android/exoplayer2/Bundleable.java
  //public interface Bundleable
  JBundleableClass = interface(IJavaClass)
    ['{137B9409-7C77-4DDD-A55F-8B2C46DF3B25}']
  end;
  [JavaSignature('com/google/android/exoplayer2/Bundleable')]
  JBundleable = interface(IJavaInstance)
    ['{A982A642-F2CD-478B-BD7B-CB1ACFC04CBC}']
  end;
  TJBundleable = class(TJavaGenericImport<JBundleableClass, JBundleable>) end;

  {*****************************************************************************}
  //https://exoplayer.dev/doc/reference/com/google/android/exoplayer2/Player.html
  //https://github.com/google/ExoPlayer/blob/release-v2/library/common/src/main/java/com/google/android/exoplayer2/Player.java
  //public interface Player
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
  [JavaSignature('com/google/android/exoplayer2/Player')]
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
  end;
  TJPlayer = class(TJavaGenericImport<JPlayerClass, JPlayer>) end;

  {********************************************************************************}
  //https://exoplayer.dev/doc/reference/com/google/android/exoplayer2/ExoPlayer.html
  //https://github.com/google/ExoPlayer/blob/release-v2/library/core/src/main/java/com/google/android/exoplayer2/ExoPlayer.java
  //public interface ExoPlayer extends Player
  JExoPlayerClass = interface(JPlayerClass)
    ['{9649DCCB-24C6-4D9C-8FF1-24604611092F}']
  end;
  [JavaSignature('com/google/android/exoplayer2/ExoPlayer')]
  JExoPlayer = interface(JPlayer)
    ['{23F139A5-FEA3-450F-8FDC-039312AE95FF}']
    procedure setMediaSource(mediaSource: JMediaSource); cdecl;
  end;
  TJExoPlayer = class(TJavaGenericImport<JExoPlayerClass, JExoPlayer>) end;

  {****************************************************************************************}
  //https://exoplayer.dev/doc/reference/com/google/android/exoplayer2/ExoPlayer.Builder.html
  //https://github.com/google/ExoPlayer/blob/release-v2/library/core/src/main/java/com/google/android/exoplayer2/ExoPlayer.java
  //public interface ExoPlayer extends Player
  //  final class Builder
  JExoPlayer_BuilderClass = interface(JObjectClass)
    ['{AE382CD9-C438-41A6-A83B-3650BA28F850}']
    {class} function init(context: JContext): JExoPlayer_Builder; cdecl;
  end;
  [JavaSignature('com/google/android/exoplayer2/ExoPlayer$Builder')]
  JExoPlayer_Builder = interface(JObject)
    ['{BC387B76-5B9D-42D0-B7D8-B252089C708E}']
    function build: JExoPlayer;
  end;
  TJExoPlayer_Builder = class(TJavaGenericImport<JExoPlayer_BuilderClass, JExoPlayer_Builder>) end;

  {**************************************************************************************}
  //https://exoplayer.dev/doc/reference/com/google/android/exoplayer2/Player.Listener.html
  //https://github.com/google/ExoPlayer/blob/release-v2/library/common/src/main/java/com/google/android/exoplayer2/Player.java
  //public interface Player
  //  interface Listener
  JPlayer_ListenerClass = interface(IJavaClass)
    ['{5850B646-28C3-4143-9F28-E803AB2DB656}']
  end;
  [JavaSignature('com/google/android/exoplayer2/Player$Listener')]
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

  {************************************************************************************}
  //https://exoplayer.dev/doc/reference/com/google/android/exoplayer2/Player.Events.html
  //https://github.com/google/ExoPlayer/blob/release-v2/library/common/src/main/java/com/google/android/exoplayer2/Player.java
  //public interface Player
  //  final class Events
  JPlayer_EventsClass = interface(JObjectClass)
    ['{F8FE15BF-4EE9-4591-8C31-EAFA05E42EB6}']
  end;
  [JavaSignature('com/google/android/exoplayer2/Player$Events')]
  JPlayer_Events = interface(JObject)
    ['{102DE229-B107-4AFE-B526-64584F4D58F9}']
  end;
  TJPlayer_Events = class(TJavaGenericImport<JPlayer_EventsClass, JPlayer_Events>) end;

  {**************************************************************************************}
  //https://exoplayer.dev/doc/reference/com/google/android/exoplayer2/Player.Commands.html
  //https://github.com/google/ExoPlayer/blob/release-v2/library/common/src/main/java/com/google/android/exoplayer2/Player.java
  //public interface Player
  //  final class Commands implements Bundleable
  JPlayer_CommandsClass = interface(JBundleableClass)
    ['{AA58B969-ACE7-4308-91D4-C9ADBF1FA11F}']
  end;
  [JavaSignature('com/google/android/exoplayer2/Player$Commands')]
  JPlayer_Commands = interface(JBundleable)
    ['{E927D3B4-3057-4239-85C3-FEC2483A0CDF}']
  end;
  TJPlayer_Commands = class(TJavaGenericImport<JPlayer_CommandsClass, JPlayer_Commands>) end;

  {******************************************************************************************}
  //https://exoplayer.dev/doc/reference/com/google/android/exoplayer2/Player.PositionInfo.html
  //https://github.com/google/ExoPlayer/blob/release-v2/library/common/src/main/java/com/google/android/exoplayer2/Player.java
  //public interface Player
  //  final class PositionInfo implements Bundleable
  JPlayer_PositionInfoClass = interface(JBundleableClass)
    ['{CD3DBA2E-61C9-4EF5-A0EC-BC22569FB5B1}']
  end;
  [JavaSignature('com/google/android/exoplayer2/Player$PositionInfo')]
  JPlayer_PositionInfo = interface(JBundleable)
    ['{6427E33E-64C3-4EB7-B9A2-A55C7AA2CDF3}']
  end;
  TJPlayer_PositionInfo = class(TJavaGenericImport<JPlayer_PositionInfoClass, JPlayer_PositionInfo>) end;

  {************************************************************************************************}
  //https://exoplayer.dev/doc/reference/com/google/android/exoplayer2/upstream/TransferListener.html
  //https://github.com/google/ExoPlayer/blob/release-v2/library/datasource/src/main/java/com/google/android/exoplayer2/upstream/TransferListener.java
  //public interface TransferListener
  JTransferListenerClass = interface(IJavaClass)
    ['{9B26DEB2-564E-400A-B57D-71A56BAC7197}']
  end;
  [JavaSignature('com/google/android/exoplayer2/upstream/TransferListener')]
  JTransferListener = interface(IJavaInstance)
    ['{18E8ADE4-F6D9-4237-8D40-5B8CBF8D3256}']
  end;
  TJTransferListener = class(TJavaGenericImport<JTransferListenerClass, JTransferListener>) end;

  {*******************************************************************************}
  //https://exoplayer.dev/doc/reference/com/google/android/exoplayer2/Timeline.html
  //https://github.com/google/ExoPlayer/blob/release-v2/library/common/src/main/java/com/google/android/exoplayer2/Timeline.java
  //public abstract class Timeline implements Bundleable
  JTimelineClass = interface(JBundleableClass)
    ['{4256CEE0-95AE-4AFC-8283-CB769BA50F9F}']
  end;
  [JavaSignature('com/google/android/exoplayer2/Timeline')]
  JTimeline = interface(JBundleable)
    ['{979A535D-AC90-4001-9CB1-3B439BFE097B}']
  end;
  TJTimeline = class(TJavaGenericImport<JTimelineClass, JTimeline>) end;

  {****************************************************************************************}
  //https://exoplayer.dev/doc/reference/com/google/android/exoplayer2/PlaybackException.html
  //https://github.com/google/ExoPlayer/blob/release-v2/library/common/src/main/java/com/google/android/exoplayer2/PlaybackException.java
  //public class PlaybackException extends Exception implements Bundleable
  JPlaybackExceptionClass = interface(JExceptionClass)
    ['{CB0CA0E9-518A-44DD-8F99-9E884C2FD4E7}']
  end;
  [JavaSignature('com/google/android/exoplayer2/PlaybackException')]
  JPlaybackException = interface(JException)
    ['{678D2E2C-088A-4EB8-B72C-9B8EF657C483}']
  end;
  TJPlaybackException = class(TJavaGenericImport<JPlaybackExceptionClass, JPlaybackException>) end;

  {*****************************************************************************************}
  //https://exoplayer.dev/doc/reference/com/google/android/exoplayer2/PlaybackParameters.html
  //https://github.com/google/ExoPlayer/blob/release-v2/library/common/src/main/java/com/google/android/exoplayer2/PlaybackParameters.java
  //public final class PlaybackParameters implements Bundleable
  JPlaybackParametersClass = interface(JBundleableClass)
    ['{9081DF30-C4B5-4DB0-8FFF-08F64DF1AC03}']
    {class} function init(speed: Single; pitch: Single): JPlaybackParameters; cdecl;
  end;
  [JavaSignature('com/google/android/exoplayer2/PlaybackParameters')]
  JPlaybackParameters = interface(JBundleable)
    ['{1B3494CD-CC19-4A71-904D-6BEDCB31E2D9}']
  end;
  TJPlaybackParameters = class(TJavaGenericImport<JPlaybackParametersClass, JPlaybackParameters>) end;

  {**************************************************************************************}
  //https://exoplayer.dev/doc/reference/com/google/android/exoplayer2/video/VideoSize.html
  //https://github.com/google/ExoPlayer/blob/release-v2/library/common/src/main/java/com/google/android/exoplayer2/video/VideoSize.java
  //public final class VideoSize implements Bundleable
  JVideoSizeClass = interface(JBundleableClass)
    ['{31E4CBE8-19B6-4CAD-8727-59C25612229C}']
  end;
  [JavaSignature('com/google/android/exoplayer2/video/VideoSize')]
  JVideoSize = interface(JBundleable)
    ['{4E874A98-8D59-434D-A390-72501A205A24}']
    function _Getheight: Integer; cdecl;
    function _Getwidth: Integer; cdecl;
    property height: Integer read _Getheight;
    property width: Integer read _Getwidth;
  end;
  TJVideoSize = class(TJavaGenericImport<JVideoSizeClass, JVideoSize>) end;

  {**************************************************************************************************************}
  //https://exoplayer.dev/doc/reference/com/google/android/exoplayer2/trackselection/TrackSelectionParameters.html
  //https://github.com/google/ExoPlayer/blob/release-v2/library/common/src/main/java/com/google/android/exoplayer2/trackselection/TrackSelectionParameters.java
  //public class TrackSelectionParameters implements Bundleable
  JTrackSelectionParametersClass = interface(JBundleableClass)
    ['{EAD6046B-1C19-4C2D-9CF5-5F004F9592C1}']
  end;
  [JavaSignature('com/google/android/exoplayer2/trackselection/TrackSelectionParameters')]
  JTrackSelectionParameters = interface(JBundleable)
    ['{4D9280D6-6561-4167-BB10-5958060EED4C}']
  end;
  TJTrackSelectionParameters = class(TJavaGenericImport<JTrackSelectionParametersClass, JTrackSelectionParameters>) end;

  {********************************************************************************************}
  //https://exoplayer.dev/doc/reference/com/google/android/exoplayer2/audio/AudioAttributes.html
  //https://github.com/google/ExoPlayer/blob/release-v2/library/common/src/main/java/com/google/android/exoplayer2/audio/AudioAttributes.java
  //public final class AudioAttributes implements Bundleable
  JAudioAttributesClass = interface(JBundleableClass)
    ['{4E1F0BFC-6408-44E9-859B-0B360BA5C19F}']
  end;
  [JavaSignature('com/google/android/exoplayer2/audio/AudioAttributes')]
  JAudioAttributes = interface(JBundleable)
    ['{CD2B93F4-0163-452A-A2FA-A8E530B88A52}']
  end;
  TJAudioAttributes = class(TJavaGenericImport<JAudioAttributesClass, JAudioAttributes>) end;

  {*********************************************************************************}
  //https://exoplayer.dev/doc/reference/com/google/android/exoplayer2/DeviceInfo.html
  //https://github.com/google/ExoPlayer/blob/release-v2/library/common/src/main/java/com/google/android/exoplayer2/DeviceInfo.java
  //public final class DeviceInfo implements Bundleable
  JDeviceInfoClass = interface(JBundleableClass)
    ['{C366CD94-DE9A-4E27-8F1F-FADEC3DA57AB}']
  end;
  [JavaSignature('com/google/android/exoplayer2/DeviceInfo')]
  JDeviceInfo = interface(JBundleable)
    ['{9323FB5B-EE90-4752-BA7C-D3070A4421D8}']
  end;
  TJDeviceInfo = class(TJavaGenericImport<JDeviceInfoClass, JDeviceInfo>) end;

  {************************************************************************************}
  //https://exoplayer.dev/doc/reference/com/google/android/exoplayer2/text/CueGroup.html
  //https://github.com/google/ExoPlayer/blob/release-v2/library/common/src/main/java/com/google/android/exoplayer2/text/CueGroup.java
  //public final class CueGroup implements Bundleable
  JCueGroupClass = interface(JBundleableClass)
    ['{A2304136-A15B-4AA0-A7E5-EEADF43C8E1A}']
  end;
  [JavaSignature('com/google/android/exoplayer2/text/CueGroup')]
  JCueGroup = interface(JBundleable)
    ['{D03E32CC-FCDA-465B-A9F6-20890FEF5227}']
  end;
  TJCueGroup = class(TJavaGenericImport<JCueGroupClass, JCueGroup>) end;

  {*****************************************************************************}
  //https://exoplayer.dev/doc/reference/com/google/android/exoplayer2/Tracks.html
  //https://github.com/google/ExoPlayer/blob/release-v2/library/common/src/main/java/com/google/android/exoplayer2/Tracks.java
  //public final class Tracks implements Bundleable
  JTracksClass = interface(JBundleableClass)
    ['{9BC81915-224E-4D28-A312-762911FA5E4B}']
  end;
  [JavaSignature('com/google/android/exoplayer2/Tracks')]
  JTracks = interface(JBundleable)
    ['{379DBB84-E60D-4C34-A51B-DDAD6C32A612}']
  end;
  TJTracks = class(TJavaGenericImport<JTracksClass, JTracks>) end;

  {*****************************************************************************************}
  //https://exoplayer.dev/doc/reference/com/google/android/exoplayer2/source/MediaSource.html
  //https://github.com/google/ExoPlayer/blob/release-v2/library/core/src/main/java/com/google/android/exoplayer2/source/MediaSource.java
  //public interface MediaSource
  JMediaSourceClass = interface(IJavaClass)
    ['{3E592CEC-43F4-4657-BA6D-9FD9D64923D6}']
  end;
  [JavaSignature('com/google/android/exoplayer2/source/MediaSource')]
  JMediaSource = interface(IJavaInstance)
    ['{C2318719-6D42-4448-98C6-F2BD1A7615A8}']
  end;
  TJMediaSource = class(TJavaGenericImport<JMediaSourceClass, JMediaSource>) end;

  {*********************************************************************************************}
  //https://exoplayer.dev/doc/reference/com/google/android/exoplayer2/source/BaseMediaSource.html
  //https://github.com/google/ExoPlayer/blob/release-v2/library/core/src/main/java/com/google/android/exoplayer2/source/BaseMediaSource.java
  //public abstract class BaseMediaSource implements MediaSource
  JBaseMediaSourceClass = interface(JMediaSourceClass)
    ['{3CBB53CB-835B-48A1-92C9-771C00FC19F9}']
  end;
  [JavaSignature('com/google/android/exoplayer2/source/BaseMediaSource')]
  JBaseMediaSource = interface(JMediaSource)
    ['{D4306711-F75A-4E78-9C14-5ED31C96591F}']
  end;
  TJBaseMediaSource = class(TJavaGenericImport<JBaseMediaSourceClass, JBaseMediaSource>) end;

  {****************************************************************************************************}
  //https://exoplayer.dev/doc/reference/com/google/android/exoplayer2/source/ProgressiveMediaSource.html
  //https://github.com/google/ExoPlayer/blob/release-v2/library/core/src/main/java/com/google/android/exoplayer2/source/ProgressiveMediaSource.java
  //public final class ProgressiveMediaSource extends BaseMediaSource implements ProgressiveMediaPeriod.Listener
  JProgressiveMediaSourceClass = interface(JBaseMediaSourceClass)
    ['{549D61C7-B8CD-4604-931E-406C6E29FC4D}']
  end;
  [JavaSignature('com/google/android/exoplayer2/source/ProgressiveMediaSource')]
  JProgressiveMediaSource = interface(JBaseMediaSource)
    ['{7D591E12-649B-4E91-8494-E23B5B64D99B}']
  end;
  TJProgressiveMediaSource = class(TJavaGenericImport<JProgressiveMediaSourceClass, JProgressiveMediaSource>) end;

  {************************************************************************************************}
  //https://exoplayer.dev/doc/reference/com/google/android/exoplayer2/source/hls/HlsMediaSource.html
  //https://github.com/google/ExoPlayer/blob/release-v2/library/hls/src/main/java/com/google/android/exoplayer2/source/hls/HlsMediaSource.java
  //public final class HlsMediaSource extends BaseMediaSource implements HlsPlaylistTracker.PrimaryPlaylistListener
  JHlsMediaSourceClass = interface(JBaseMediaSourceClass)
    ['{9CB1E5D7-8111-401A-916B-AED6B81FFAD6}']
  end;
  [JavaSignature('com/google/android/exoplayer2/source/hls/HlsMediaSource')]
  JHlsMediaSource = interface(JBaseMediaSource)
    ['{4A68FCD9-06A1-4C28-A518-8079DDBCA395}']
  end;
  TJHlsMediaSource = class(TJavaGenericImport<JHlsMediaSourceClass, JHlsMediaSource>) end;

  {*************************************************************************************************}
  //https://exoplayer.dev/doc/reference/com/google/android/exoplayer2/source/MediaSource.Factory.html
  //https://github.com/google/ExoPlayer/blob/release-v2/library/core/src/main/java/com/google/android/exoplayer2/source/MediaSource.java
  //public interface MediaSource
  //  interface Factory
  JMediaSource_FactoryClass = interface(IJavaClass)
    ['{57AF7F71-FFC0-4CB6-B401-65DFC1F9253F}']
  end;
  [JavaSignature('com/google/android/exoplayer2/source/MediaSource$Factory')]
  JMediaSource_Factory = interface(IJavaInstance)
    ['{1EDC2493-E0FE-4FB4-98E3-C8430BEF56D9}']
  end;
  TJMediaSource_Factory = class(TJavaGenericImport<JMediaSource_FactoryClass, JMediaSource_Factory>) end;

  {************************************************************************************************}
  //https://exoplayer.dev/doc/reference/com/google/android/exoplayer2/source/MediaSourceFactory.html
  //https://github.com/google/ExoPlayer/blob/release-v2/library/core/src/main/java/com/google/android/exoplayer2/source/MediaSourceFactory.java
  //public interface MediaSourceFactory extends MediaSource.Factory
  JMediaSourceFactoryClass = interface(JMediaSource_FactoryClass)
    ['{B84352AF-4537-4173-BE96-441C97B4E798}']
  end;
  [JavaSignature('com/google/android/exoplayer2/source/MediaSourceFactory')]
  JMediaSourceFactory = interface(JMediaSource_Factory)
    ['{13E30EC4-5E63-4EA1-99F4-CABCACE46FEC}']
  end;
  TJMediaSourceFactory = class(TJavaGenericImport<JMediaSourceFactoryClass, JMediaSourceFactory>) end;

  {************************************************************************************************************}
  //https://exoplayer.dev/doc/reference/com/google/android/exoplayer2/source/ProgressiveMediaSource.Factory.html
  //https://github.com/google/ExoPlayer/blob/release-v2/library/core/src/main/java/com/google/android/exoplayer2/source/ProgressiveMediaSource.java
  //public final class ProgressiveMediaSource extends BaseMediaSource implements ProgressiveMediaPeriod.Listener
  //  public static final class Factory implements MediaSourceFactory
  JProgressiveMediaSource_FactoryClass = interface(JMediaSourceFactoryClass)
    ['{B61AB190-CADC-47B0-9650-EB3CADFB24ED}']
    {class} function init(dataSourceFactory: JDataSource_Factory): JProgressiveMediaSource_Factory; cdecl;
  end;
  [JavaSignature('com/google/android/exoplayer2/source/ProgressiveMediaSource$Factory')]
  JProgressiveMediaSource_Factory = interface(JMediaSourceFactory)
    ['{C01468D3-F52F-4486-BCB5-0187F99C0428}']
    function createMediaSource(mediaItem: JMediaItem): JProgressiveMediaSource; cdecl;
  end;
  TJProgressiveMediaSource_Factory = class(TJavaGenericImport<JProgressiveMediaSource_FactoryClass, JProgressiveMediaSource_Factory>) end;

  {********************************************************************************************************}
  //https://exoplayer.dev/doc/reference/com/google/android/exoplayer2/source/hls/HlsMediaSource.Factory.html
  //https://github.com/google/ExoPlayer/blob/release-v2/library/hls/src/main/java/com/google/android/exoplayer2/source/hls/HlsMediaSource.java
  //public final class HlsMediaSource extends BaseMediaSource implements HlsPlaylistTracker.PrimaryPlaylistListener
  //  public static final class Factory implements MediaSourceFactory
  JHlsMediaSource_FactoryClass = interface(JMediaSourceFactoryClass)
    ['{6CF34ADE-D3E9-4EE2-8B27-60C4838066BF}']
    {class} function init(dataSourceFactory: JDataSource_Factory): JHlsMediaSource_Factory; cdecl;
  end;
  [JavaSignature('com/google/android/exoplayer2/source/hls/HlsMediaSource$Factory')]
  JHlsMediaSource_Factory = interface(JMediaSourceFactory)
    ['{84A92CAC-3429-44EB-BBE6-8A04AAFD7DEC}']
    function createMediaSource(mediaItem: JMediaItem): JHlsMediaSource; cdecl;
  end;
  TJHlsMediaSource_Factory = class(TJavaGenericImport<JHlsMediaSource_FactoryClass, JHlsMediaSource_Factory>) end;

  {**************************************************************************************************}
  //https://exoplayer.dev/doc/reference/com/google/android/exoplayer2/upstream/DataSource.Factory.html
  //https://github.com/google/ExoPlayer/blob/release-v2/library/datasource/src/main/java/com/google/android/exoplayer2/upstream/DataSource.java
  //public interface DataSource extends DataReader
  //  interface Factory
  JDataSource_FactoryClass = interface(IJavaClass)
    ['{CF670334-A97D-48F1-918D-5BABB74532B0}']
  end;
  [JavaSignature('com/google/android/exoplayer2/upstream/DataSource$Factory')]
  JDataSource_Factory = interface(IJavaInstance)
    ['{2FF40D20-A1B1-461D-8CF9-4F1F3E27519C}']
  end;
  TJDataSource_Factory = class(TJavaGenericImport<JDataSource_FactoryClass, JDataSource_Factory>) end;

  {*********************************************************************************************************}
  //https://exoplayer.dev/doc/reference/com/google/android/exoplayer2/upstream/DefaultDataSource.Factory.html
  //https://github.com/google/ExoPlayer/blob/release-v2/library/datasource/src/main/java/com/google/android/exoplayer2/upstream/DefaultDataSource.java
  //public final class DefaultDataSource implements DataSource
  //  public static final class Factory implements DataSource.Factory
  JDefaultDataSource_FactoryClass = interface(JDataSource_FactoryClass)
    ['{F4CE7183-BCCA-4231-8492-F79B8ABE023D}']
    {class} function init(context: JContext): JDefaultDataSource_Factory; cdecl;
  end;
  [JavaSignature('com/google/android/exoplayer2/upstream/DefaultDataSource$Factory')]
  JDefaultDataSource_Factory = interface(JDataSource_Factory)
    ['{DAA8568E-C6A9-4F80-BD38-1EF8D6DB4A56}']
  end;
  TJDefaultDataSource_Factory = class(TJavaGenericImport<JDefaultDataSource_FactoryClass, JDefaultDataSource_Factory>) end;

  {************************************************************************************}
  //https://exoplayer.dev/doc/reference/com/google/android/exoplayer2/MediaMetadata.html
  //https://github.com/google/ExoPlayer/blob/release-v2/library/common/src/main/java/com/google/android/exoplayer2/MediaMetadata.java
  //public final class MediaMetadata implements Bundleable
  JMediaMetadataClass = interface(JBundleableClass)
    ['{AE9FA6AC-73CD-417B-8507-B199CCC8F5FA}']
  end;
  [JavaSignature('com/google/android/exoplayer2/MediaMetadata')]
  JMediaMetadata = interface(JBundleable)
    ['{26473517-35E2-4FE4-B9C6-9EB8AD5172A1}']
  end;
  TJMediaMetadata = class(TJavaGenericImport<JMediaMetadataClass, JMediaMetadata>) end;

  {****************************************************************************************}
  //https://exoplayer.dev/doc/reference/com/google/android/exoplayer2/metadata/Metadata.html
  //https://github.com/google/ExoPlayer/blob/release-v2/library/common/src/main/java/com/google/android/exoplayer2/metadata/Metadata.java
  //public final class Metadata implements Parcelable
  JMetadataClass = interface(JParcelableClass)
    ['{B829B0F7-CDF4-4F06-9464-F9258095A76B}']
  end;
  [JavaSignature('com/google/android/exoplayer2/metadata/Metadata')]
  JMetadata = interface(JParcelable)
    ['{4753A269-75F1-46DD-88D8-106FAED90EBE}']
  end;
  TJMetadata = class(TJavaGenericImport<JMetadataClass, JMetadata>) end;

  {********************************************************************************}
  //https://exoplayer.dev/doc/reference/com/google/android/exoplayer2/MediaItem.html
  //https://github.com/google/ExoPlayer/blob/release-v2/library/common/src/main/java/com/google/android/exoplayer2/MediaItem.java
  //public final class MediaItem implements Bundleable
  JMediaItemClass = interface(JBundleableClass)
    ['{5C529D21-3854-4EB6-9221-89878D56240D}']
    {class} function fromUri(uri: JString): JMediaItem; cdecl;
  end;
  [JavaSignature('com/google/android/exoplayer2/MediaItem')]
  JMediaItem = interface(JBundleable)
    ['{127279C9-9678-4735-9597-57D1579CB7C3}']
  end;
  TJMediaItem = class(TJavaGenericImport<JMediaItemClass, JMediaItem>) end;

implementation

{**********************}
procedure RegisterTypes;
begin
  TRegTypes.RegisterType('Alcinoe.AndroidApi.ExoPlayer.JBundleable', TypeInfo(Alcinoe.AndroidApi.ExoPlayer.JBundleable));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.ExoPlayer.JPlayer', TypeInfo(Alcinoe.AndroidApi.ExoPlayer.JPlayer));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.ExoPlayer.JExoPlayer', TypeInfo(Alcinoe.AndroidApi.ExoPlayer.JExoPlayer));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.ExoPlayer.JExoPlayer_Builder', TypeInfo(Alcinoe.AndroidApi.ExoPlayer.JExoPlayer_Builder));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.ExoPlayer.JPlayer_Listener', TypeInfo(Alcinoe.AndroidApi.ExoPlayer.JPlayer_Listener));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.ExoPlayer.JPlayer_Events', TypeInfo(Alcinoe.AndroidApi.ExoPlayer.JPlayer_Events));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.ExoPlayer.JPlayer_Commands', TypeInfo(Alcinoe.AndroidApi.ExoPlayer.JPlayer_Commands));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.ExoPlayer.JPlayer_PositionInfo', TypeInfo(Alcinoe.AndroidApi.ExoPlayer.JPlayer_PositionInfo));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.ExoPlayer.JTransferListener', TypeInfo(Alcinoe.AndroidApi.ExoPlayer.JTransferListener));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.ExoPlayer.JTimeline', TypeInfo(Alcinoe.AndroidApi.ExoPlayer.JTimeline));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.ExoPlayer.JPlaybackException', TypeInfo(Alcinoe.AndroidApi.ExoPlayer.JPlaybackException));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.ExoPlayer.JPlaybackParameters', TypeInfo(Alcinoe.AndroidApi.ExoPlayer.JPlaybackParameters));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.ExoPlayer.JVideoSize', TypeInfo(Alcinoe.AndroidApi.ExoPlayer.JVideoSize));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.ExoPlayer.JTrackSelectionParameters', TypeInfo(Alcinoe.AndroidApi.ExoPlayer.JTrackSelectionParameters));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.ExoPlayer.JAudioAttributes', TypeInfo(Alcinoe.AndroidApi.ExoPlayer.JAudioAttributes));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.ExoPlayer.JDeviceInfo', TypeInfo(Alcinoe.AndroidApi.ExoPlayer.JDeviceInfo));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.ExoPlayer.JCueGroup', TypeInfo(Alcinoe.AndroidApi.ExoPlayer.JCueGroup));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.ExoPlayer.JTracks', TypeInfo(Alcinoe.AndroidApi.ExoPlayer.JTracks));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.ExoPlayer.JMediaSource', TypeInfo(Alcinoe.AndroidApi.ExoPlayer.JMediaSource));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.ExoPlayer.JBaseMediaSource', TypeInfo(Alcinoe.AndroidApi.ExoPlayer.JBaseMediaSource));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.ExoPlayer.JProgressiveMediaSource', TypeInfo(Alcinoe.AndroidApi.ExoPlayer.JProgressiveMediaSource));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.ExoPlayer.JHlsMediaSource', TypeInfo(Alcinoe.AndroidApi.ExoPlayer.JHlsMediaSource));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.ExoPlayer.JMediaSource_Factory', TypeInfo(Alcinoe.AndroidApi.ExoPlayer.JMediaSource_Factory));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.ExoPlayer.JMediaSourceFactory', TypeInfo(Alcinoe.AndroidApi.ExoPlayer.JMediaSourceFactory));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.ExoPlayer.JProgressiveMediaSource_Factory', TypeInfo(Alcinoe.AndroidApi.ExoPlayer.JProgressiveMediaSource_Factory));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.ExoPlayer.JHlsMediaSource_Factory', TypeInfo(Alcinoe.AndroidApi.ExoPlayer.JHlsMediaSource_Factory));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.ExoPlayer.JDataSource_Factory', TypeInfo(Alcinoe.AndroidApi.ExoPlayer.JDataSource_Factory));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.ExoPlayer.JDefaultDataSource_Factory', TypeInfo(Alcinoe.AndroidApi.ExoPlayer.JDefaultDataSource_Factory));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.ExoPlayer.JMediaMetadata', TypeInfo(Alcinoe.AndroidApi.ExoPlayer.JMediaMetadata));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.ExoPlayer.JMetadata', TypeInfo(Alcinoe.AndroidApi.ExoPlayer.JMetadata));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.ExoPlayer.JMediaItem', TypeInfo(Alcinoe.AndroidApi.ExoPlayer.JMediaItem));
end;

initialization
  RegisterTypes;

end.
