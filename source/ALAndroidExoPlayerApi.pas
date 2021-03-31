unit ALAndroidExoPlayerApi;

interface

uses
  Androidapi.JNIBridge,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Net,
  Androidapi.JNI.Os,
  Androidapi.JNI.GraphicsContentViewText;

type

  {**************************}
  JBandwidthMeter = interface;
  JBandwidthMeter_EventListener = interface;
  JDefaultBandwidthMeter = interface;
  JTrackSelection = interface;
  JTrackSelection_Factory = interface;
  JBaseTrackSelection = interface;
  JAdaptiveTrackSelection = interface;
  JAdaptiveTrackSelection_Factory = interface;
  JTrackSelector = interface;
  JMappingTrackSelector = interface;
  JDefaultTrackSelector = interface;
  JPlayer = interface;
  JExoPlayer = interface;
  JSimpleExoPlayer = interface;
  JExoPlayerFactory = interface;
  JDataSource_Factory = interface;
  JDefaultDataSourceFactory = interface;
  JUtil = interface;
  JMediaSource = interface;
  JMediaSourceEventListener = interface;
  JAdsMediaSource_MediaSourceFactory = interface;
  JExtractorMediaSource = interface;
  JExtractorMediaSource_Factory = interface;
  JSimpleExoPlayer_VideoListener = interface;
  JTimeline = interface;
  JTrackGroupArray = interface;
  JTrackSelectionArray = interface;
  JExoPlaybackException = interface;
  JPlaybackParameters = interface;
  JPlayer_EventListener = interface;
  JHlsMediaSource = interface;
  JHlsMediaSource_Factory = interface;
  JTransferListener = interface;

  {******************************************}
  JBandwidthMeterClass = interface(IJavaClass)
    ['{0181196A-9765-4D2E-8FA6-350684794D3A}']
    {class} function _GetNO_ESTIMATE: Int64; cdecl;
    {class} property NO_ESTIMATE: Int64 read _GetNO_ESTIMATE;
  end;

  {**********************************************************************}
  [JavaSignature('com/google/android/exoplayer2/upstream/BandwidthMeter')]
  JBandwidthMeter = interface(IJavaInstance)
    ['{EEEC4661-F5F7-46EA-9144-44A9C0DD113C}']
    function getBitrateEstimate: Int64; cdecl;
  end;
  TJBandwidthMeter = class(TJavaGenericImport<JBandwidthMeterClass, JBandwidthMeter>) end;

  {********************************************************}
  JBandwidthMeter_EventListenerClass = interface(IJavaClass)
    ['{38C3F6FF-905D-43C6-8F67-8B090F9C4F33}']
  end;

  {************************************************************************************}
  [JavaSignature('com/google/android/exoplayer2/upstream/BandwidthMeter$EventListener')]
  JBandwidthMeter_EventListener = interface(IJavaInstance)
    ['{2058F446-77A3-4278-B42F-3137BF005479}']
    procedure onBandwidthSample(elapsedMs: Integer; bytes: Int64; bitrate: Int64); cdecl;
  end;
  TJBandwidthMeter_EventListener = class(TJavaGenericImport<JBandwidthMeter_EventListenerClass, JBandwidthMeter_EventListener>) end;

  {***********************************************************}
  JDefaultBandwidthMeterClass = interface(JBandwidthMeterClass)
    ['{2B9F65F5-8EAB-466A-9A2E-DDD9CA88F807}']
    {class} function _GetDEFAULT_MAX_WEIGHT: Integer; cdecl;
    {class} function init: JDefaultBandwidthMeter; cdecl; overload;
    {class} //function init(eventHandler: JHandler; eventListener: JBandwidthMeter_EventListener): JDefaultBandwidthMeter; cdecl; overload;
    {class} //function init(eventHandler: JHandler; eventListener: JBandwidthMeter_EventListener; maxWeight: Integer): JDefaultBandwidthMeter; cdecl; overload;
    {class} //function init(eventHandler: JHandler; eventListener: JBandwidthMeter_EventListener; maxWeight: Integer; clock: JClock): JDefaultBandwidthMeter; cdecl; overload;
    {class} property DEFAULT_MAX_WEIGHT: Integer read _GetDEFAULT_MAX_WEIGHT;
  end;

  {*****************************************************************************}
  [JavaSignature('com/google/android/exoplayer2/upstream/DefaultBandwidthMeter')]
  JDefaultBandwidthMeter = interface(JBandwidthMeter)
    ['{D6A32707-D8F6-458B-AAB6-926A523D1A95}']
    //procedure onBytesTransferred(source: JObject; bytes: Integer); cdecl;
    //procedure onTransferEnd(source: JObject); cdecl;
    //procedure onTransferStart(source: JObject; dataSpec: JDataSpec); cdecl;
  end;
  TJDefaultBandwidthMeter = class(TJavaGenericImport<JDefaultBandwidthMeterClass, JDefaultBandwidthMeter>) end;

  {******************************************}
  JTrackSelectionClass = interface(IJavaClass)
    ['{6F19CA83-1E76-44DE-A9A8-31A73207C738}']
  end;

  {****************************************************************************}
  [JavaSignature('com/google/android/exoplayer2/trackselection/TrackSelection')]
  JTrackSelection = interface(IJavaInstance)
    ['{E564B557-219C-4E4F-918A-FE116A1A1C2E}']
    procedure enable; cdecl;
    procedure disable; cdecl;
    //function getTrackGroup: JTrackGroup; cdecl;
    function length: Integer; cdecl;
    //function getFormat(index: Integer): Jexoplayer2_Format; cdecl;
    function getIndexInTrackGroup(index: Integer): Integer; cdecl;
    //function indexOf(format: Jexoplayer2_Format): Integer; cdecl; overload;
    //function indexOf(indexInTrackGroup: Integer): Integer; cdecl; overload;
    //function getSelectedFormat: Jexoplayer2_Format; cdecl;
    function getSelectedIndexInTrackGroup: Integer; cdecl;
    function getSelectedIndex: Integer; cdecl;
    function getSelectionReason: Integer; cdecl;
    //function getSelectionData: JObject; cdecl;
    procedure updateSelectedTrack(playbackPositionUs: Int64; bufferedDurationUs: Int64; availableDurationUs: Int64); cdecl;
    //function evaluateQueueSize(playbackPositionUs: Int64; queue: JList): Integer; cdecl;
    function blacklist(index: Integer; blacklistDurationMs: Int64): Boolean; cdecl;
  end;
  TJTrackSelection = class(TJavaGenericImport<JTrackSelectionClass, JTrackSelection>) end;

  {**************************************************}
  JTrackSelection_FactoryClass = interface(IJavaClass)
    ['{8CC30384-6615-48E8-9A57-08FEF2ADABAB}']
  end;

  {************************************************************************************}
  [JavaSignature('com/google/android/exoplayer2/trackselection/TrackSelection$Factory')]
  JTrackSelection_Factory = interface(IJavaInstance)
    ['{F78F69A8-7D8D-4131-9B51-2A141AC6A811}']
  end;
  TJTrackSelection_Factory = class(TJavaGenericImport<JTrackSelection_FactoryClass, JTrackSelection_Factory>) end;

  {********************************************************}
  JBaseTrackSelectionClass = interface(JTrackSelectionClass)
    ['{8C54E126-5762-4505-9FA6-A213FEFD5D14}']
  end;

  {********************************************************************************}
  [JavaSignature('com/google/android/exoplayer2/trackselection/BaseTrackSelection')]
  JBaseTrackSelection = interface(JTrackSelection)
    ['{D6C80B67-E804-4426-84EA-B494DFFD4C7B}']
  end;
  TJBaseTrackSelection = class(TJavaGenericImport<JBaseTrackSelectionClass, JBaseTrackSelection>) end;

    {**************************************************************}
  JAdaptiveTrackSelectionClass = interface(JBaseTrackSelectionClass)
    ['{15CD7BF5-B37B-4284-9787-6DB456E00230}']
    {class} function _GetDEFAULT_BANDWIDTH_FRACTION: Single; cdecl;
    {class} function _GetDEFAULT_BUFFERED_FRACTION_TO_LIVE_EDGE_FOR_QUALITY_INCREASE: Single; cdecl;
    {class} function _GetDEFAULT_MAX_DURATION_FOR_QUALITY_DECREASE_MS: Integer; cdecl;
    {class} function _GetDEFAULT_MAX_INITIAL_BITRATE: Integer; cdecl;
    {class} function _GetDEFAULT_MIN_DURATION_FOR_QUALITY_INCREASE_MS: Integer; cdecl;
    {class} function _GetDEFAULT_MIN_DURATION_TO_RETAIN_AFTER_DISCARD_MS: Integer; cdecl;
    {class} //function init(group: JTrackGroup; tracks: TJavaArray<Integer>; bandwidthMeter: JBandwidthMeter): JAdaptiveTrackSelection; cdecl; overload;
    {class} //function init(group: JTrackGroup;
            //              tracks: TJavaArray<Integer>;
            //              bandwidthMeter: JBandwidthMeter;
            //              maxInitialBitrate: Integer;
            //              minDurationForQualityIncreaseMs: Int64;
            //              maxDurationForQualityDecreaseMs: Int64;
            //              minDurationToRetainAfterDiscardMs: Int64;
            //              bandwidthFraction: Single;
            //              bufferedFractionToLiveEdgeForQualityIncrease: Single): JAdaptiveTrackSelection; cdecl; overload;
    {class} property DEFAULT_BANDWIDTH_FRACTION: Single read _GetDEFAULT_BANDWIDTH_FRACTION;
    {class} property DEFAULT_BUFFERED_FRACTION_TO_LIVE_EDGE_FOR_QUALITY_INCREASE: Single read _GetDEFAULT_BUFFERED_FRACTION_TO_LIVE_EDGE_FOR_QUALITY_INCREASE;
    {class} property DEFAULT_MAX_DURATION_FOR_QUALITY_DECREASE_MS: Integer read _GetDEFAULT_MAX_DURATION_FOR_QUALITY_DECREASE_MS;
    {class} property DEFAULT_MAX_INITIAL_BITRATE: Integer read _GetDEFAULT_MAX_INITIAL_BITRATE;
    {class} property DEFAULT_MIN_DURATION_FOR_QUALITY_INCREASE_MS: Integer read _GetDEFAULT_MIN_DURATION_FOR_QUALITY_INCREASE_MS;
    {class} property DEFAULT_MIN_DURATION_TO_RETAIN_AFTER_DISCARD_MS: Integer read _GetDEFAULT_MIN_DURATION_TO_RETAIN_AFTER_DISCARD_MS;
  end;

  {************************************************************************************}
  [JavaSignature('com/google/android/exoplayer2/trackselection/AdaptiveTrackSelection')]
  JAdaptiveTrackSelection = interface(JBaseTrackSelection)
    ['{4AC25A57-6ABB-4DE3-94A7-4E9EFC97B197}']
  end;
  TJAdaptiveTrackSelection = class(TJavaGenericImport<JAdaptiveTrackSelectionClass, JAdaptiveTrackSelection>) end;

  {****************************************************************************}
  JAdaptiveTrackSelection_FactoryClass = interface(JTrackSelection_FactoryClass)
    ['{E5993823-7009-488D-8529-62E1B20BA613}']
    {class} function init(bandwidthMeter: JBandwidthMeter): JAdaptiveTrackSelection_Factory; cdecl; overload;
    {class} function init(bandwidthMeter: JBandwidthMeter;
                          maxInitialBitrate: Integer;
                          minDurationForQualityIncreaseMs: Integer;
                          maxDurationForQualityDecreaseMs: Integer;
                          minDurationToRetainAfterDiscardMs: Integer;
                          bandwidthFraction: Single): JAdaptiveTrackSelection_Factory; cdecl; overload;
    {class} function init(bandwidthMeter: JBandwidthMeter;
                          maxInitialBitrate: Integer;
                          minDurationForQualityIncreaseMs: Integer;
                          maxDurationForQualityDecreaseMs: Integer;
                          minDurationToRetainAfterDiscardMs: Integer;
                          bandwidthFraction: Single;
                          bufferedFractionToLiveEdgeForQualityIncrease: Single): JAdaptiveTrackSelection_Factory; cdecl; overload;
  end;

  {********************************************************************************************}
  [JavaSignature('com/google/android/exoplayer2/trackselection/AdaptiveTrackSelection$Factory')]
  JAdaptiveTrackSelection_Factory = interface(JTrackSelection_Factory)
    ['{3FD6563D-3F3B-44B8-9CD2-49B136B7B8B9}']
  end;
  TJAdaptiveTrackSelection_Factory = class(TJavaGenericImport<JAdaptiveTrackSelection_FactoryClass, JAdaptiveTrackSelection_Factory>) end;

  {*******************************************}
  JTrackSelectorClass = interface(JObjectClass)
    ['{A445D5F0-132D-4659-B74D-CCF184A02931}']
    {class} //function init(listener: JInvalidationListener): JTrackSelector; cdecl; overload;
  end;

  {***************************************************************************}
  [JavaSignature('com/google/android/exoplayer2/trackselection/TrackSelector')]
  JTrackSelector = interface(JObject)
    ['{D898E52D-B2D4-4CE1-BA37-734E79BDF032}']
    procedure onSelectionActivated(info: JObject); cdecl;
    //function selectTracks(rendererCapabilities: TJavaObjectArray<JRendererCapabilities>; trackGroups: JTrackGroupArray): JTrackSelectorResult; cdecl;
  end;
  TJTrackSelector = class(TJavaGenericImport<JTrackSelectorClass, JTrackSelector>) end;

  {*********************************************************}
  JMappingTrackSelectorClass = interface(JTrackSelectorClass)
    ['{3DF053E7-E763-4181-8F6D-5C3576B08583}']
    {class} function init: JMappingTrackSelector; cdecl;
  end;

  {**********************************************************************************}
  [JavaSignature('com/google/android/exoplayer2/trackselection/MappingTrackSelector')]
  JMappingTrackSelector = interface(JTrackSelector)
    ['{A80D81D6-88DB-48D6-A5FA-DDFFA0011A3D}']
    //function getCurrentMappedTrackInfo: JMappingTrackSelector_MappedTrackInfo; cdecl;
    procedure setRendererDisabled(rendererIndex: Integer; disabled: Boolean); cdecl;
    function getRendererDisabled(rendererIndex: Integer): Boolean; cdecl;
    //procedure setSelectionOverride(rendererIndex: Integer; groups: JTrackGroupArray; &override: JMappingTrackSelector_SelectionOverride); cdecl;
    function hasSelectionOverride(rendererIndex: Integer; groups: JTrackGroupArray): Boolean; cdecl;
    //function getSelectionOverride(rendererIndex: Integer; groups: JTrackGroupArray): JMappingTrackSelector_SelectionOverride; cdecl;
    procedure clearSelectionOverride(rendererIndex: Integer; groups: JTrackGroupArray); cdecl;
    procedure clearSelectionOverrides; cdecl; overload;
    procedure clearSelectionOverrides(rendererIndex: Integer); cdecl; overload;
    procedure setTunnelingAudioSessionId(tunnelingAudioSessionId: Integer); cdecl;
  end;
  TJMappingTrackSelector = class(TJavaGenericImport<JMappingTrackSelectorClass, JMappingTrackSelector>) end;

  {****************************************************************}
  JDefaultTrackSelectorClass = interface(JMappingTrackSelectorClass)
    ['{ED31BC33-F0D7-4309-897C-DE3B7377F4B7}']
    {class} function init: JDefaultTrackSelector; cdecl; overload;
    {class} function init(adaptiveTrackSelectionFactory: JTrackSelection_Factory): JDefaultTrackSelector; cdecl; overload;
    {class} function init(bandwidthMeter: JBandwidthMeter): JDefaultTrackSelector; cdecl; overload;
  end;

  {**********************************************************************************}
  [JavaSignature('com/google/android/exoplayer2/trackselection/DefaultTrackSelector')]
  JDefaultTrackSelector = interface(JMappingTrackSelector)
    ['{E39CDA33-4B33-4B5D-98D7-5365E03C4927}']
    //function getParameters: JDefaultTrackSelector_Parameters; cdecl;
    //procedure setParameters(P1: JDefaultTrackSelector_Parameters); cdecl;
  end;
  TJDefaultTrackSelector = class(TJavaGenericImport<JDefaultTrackSelectorClass, JDefaultTrackSelector>) end;

  {**********************************}
  JPlayerClass = interface(IJavaClass)
    ['{0D5D1A43-B6C5-4743-B083-AB82F7EFE36B}']
    {class} function _GetDISCONTINUITY_REASON_INTERNAL: Integer; cdecl;
    {class} function _GetDISCONTINUITY_REASON_PERIOD_TRANSITION: Integer; cdecl;
    {class} function _GetDISCONTINUITY_REASON_SEEK: Integer; cdecl;
    {class} function _GetDISCONTINUITY_REASON_SEEK_ADJUSTMENT: Integer; cdecl;
    {class} function _GetREPEAT_MODE_ALL: Integer; cdecl;
    {class} function _GetREPEAT_MODE_OFF: Integer; cdecl;
    {class} function _GetREPEAT_MODE_ONE: Integer; cdecl;
    {class} function _GetSTATE_BUFFERING: Integer; cdecl;
    {class} function _GetSTATE_ENDED: Integer; cdecl;
    {class} function _GetSTATE_IDLE: Integer; cdecl;
    {class} function _GetSTATE_READY: Integer; cdecl;
    {class} property DISCONTINUITY_REASON_INTERNAL: Integer read _GetDISCONTINUITY_REASON_INTERNAL;
    {class} property DISCONTINUITY_REASON_PERIOD_TRANSITION: Integer read _GetDISCONTINUITY_REASON_PERIOD_TRANSITION;
    {class} property DISCONTINUITY_REASON_SEEK: Integer read _GetDISCONTINUITY_REASON_SEEK;
    {class} property DISCONTINUITY_REASON_SEEK_ADJUSTMENT: Integer read _GetDISCONTINUITY_REASON_SEEK_ADJUSTMENT;
    {class} property REPEAT_MODE_ALL: Integer read _GetREPEAT_MODE_ALL;
    {class} property REPEAT_MODE_OFF: Integer read _GetREPEAT_MODE_OFF;
    {class} property REPEAT_MODE_ONE: Integer read _GetREPEAT_MODE_ONE;
    {class} property STATE_BUFFERING: Integer read _GetSTATE_BUFFERING;
    {class} property STATE_ENDED: Integer read _GetSTATE_ENDED;
    {class} property STATE_IDLE: Integer read _GetSTATE_IDLE;
    {class} property STATE_READY: Integer read _GetSTATE_READY;
  end;

  {*****************************************************}
  [JavaSignature('com/google/android/exoplayer2/Player')]
  JPlayer = interface(IJavaInstance)
    ['{B10E2FE7-CD9D-4C9C-9EEF-52D1ADC2AF57}']
    procedure addListener(listener: JPlayer_EventListener); cdecl;
    procedure removeListener(listener: JPlayer_EventListener); cdecl;
    function getPlaybackState: Integer; cdecl;
    procedure setPlayWhenReady(playWhenReady: Boolean); cdecl;
    function getPlayWhenReady: Boolean; cdecl;
    procedure setRepeatMode(repeatMode: Integer); cdecl;
    function getRepeatMode: Integer; cdecl;
    procedure setShuffleModeEnabled(shuffleModeEnabled: Boolean); cdecl;
    function getShuffleModeEnabled: Boolean; cdecl;
    function isLoading: Boolean; cdecl;
    procedure seekToDefaultPosition; cdecl; overload;
    procedure seekToDefaultPosition(windowIndex: Integer); cdecl; overload;
    procedure seekTo(positionMs: Int64); cdecl; overload;
    procedure seekTo(windowIndex: Integer; positionMs: Int64); cdecl; overload;
    procedure setPlaybackParameters(playbackParameters: JPlaybackParameters); cdecl;
    function getPlaybackParameters: JPlaybackParameters; cdecl;
    procedure stop; cdecl;
    procedure release; cdecl;
    function getRendererCount: Integer; cdecl;
    function getRendererType(index: Integer): Integer; cdecl;
    function getCurrentTrackGroups: JTrackGroupArray; cdecl;
    function getCurrentTrackSelections: JTrackSelectionArray; cdecl;
    function getCurrentManifest: JObject; cdecl;
    function getCurrentTimeline: JTimeline; cdecl;
    function getCurrentPeriodIndex: Integer; cdecl;
    function getCurrentWindowIndex: Integer; cdecl;
    function getNextWindowIndex: Integer; cdecl;
    function getPreviousWindowIndex: Integer; cdecl;
    function getDuration: Int64; cdecl;
    function getCurrentPosition: Int64; cdecl;
    function getBufferedPosition: Int64; cdecl;
    function getBufferedPercentage: Integer; cdecl;
    function isCurrentWindowDynamic: Boolean; cdecl;
    function isCurrentWindowSeekable: Boolean; cdecl;
    function isPlayingAd: Boolean; cdecl;
    function getCurrentAdGroupIndex: Integer; cdecl;
    function getCurrentAdIndexInAdGroup: Integer; cdecl;
    function getContentPosition: Int64; cdecl;
  end;
  TJPlayer = class(TJavaGenericImport<JPlayerClass, JPlayer>) end;

  {***************************************}
  JExoPlayerClass = interface(JPlayerClass)
    ['{9649DCCB-24C6-4D9C-8FF1-24604611092F}']
  end;

  {********************************************************}
  [JavaSignature('com/google/android/exoplayer2/ExoPlayer')]
  JExoPlayer = interface(JPlayer)
    ['{23F139A5-FEA3-450F-8FDC-039312AE95FF}']
    //function getPlaybackLooper: JLooper; cdecl;
    procedure prepare(mediaSource: JMediaSource); cdecl; overload;
    procedure prepare(mediaSource: JMediaSource; resetPosition: Boolean; resetState: Boolean); cdecl; overload;
  end;
  TJExoPlayer = class(TJavaGenericImport<JExoPlayerClass, JExoPlayer>) end;

  {************************************************}
  JSimpleExoPlayerClass = interface(JExoPlayerClass)
    ['{3143E53D-A32A-4515-862F-186EF389FA9B}']
  end;

  {**************************************************************}
  [JavaSignature('com/google/android/exoplayer2/SimpleExoPlayer')]
  JSimpleExoPlayer = interface(JExoPlayer)
    ['{CD0CFB64-6D3A-48ED-BB3A-384AFE20A1BB}']
    procedure setVideoScalingMode(videoScalingMode: Integer); cdecl;
    function getVideoScalingMode: Integer; cdecl;
    procedure clearVideoSurface; cdecl; overload;
    procedure setVideoSurface(surface: JSurface); cdecl;
    procedure clearVideoSurface(surface: JSurface); cdecl; overload;
    procedure setVideoSurfaceHolder(surfaceHolder: JSurfaceHolder); cdecl;
    procedure clearVideoSurfaceHolder(surfaceHolder: JSurfaceHolder); cdecl;
    procedure setVideoSurfaceView(surfaceView: JSurfaceView); cdecl;
    procedure clearVideoSurfaceView(surfaceView: JSurfaceView); cdecl;
    //procedure setVideoTextureView(textureView: JTextureView); cdecl;
    //procedure clearVideoTextureView(textureView: JTextureView); cdecl;
    //procedure setAudioStreamType(streamType: Integer); cdecl; deprecated;
    //function getAudioStreamType: Integer; cdecl; deprecated;
    //procedure setAudioAttributes(audioAttributes: Jaudio_AudioAttributes); cdecl;
    //function getAudioAttributes: Jaudio_AudioAttributes; cdecl;
    procedure setVolume(audioVolume: Single); cdecl;
    function getVolume: Single; cdecl;
    //procedure setPlaybackParams(params: JPlaybackParams); cdecl; deprecated;
    //function getVideoFormat: Jexoplayer2_Format; cdecl;
    //function getAudioFormat: Jexoplayer2_Format; cdecl;
    function getAudioSessionId: Integer; cdecl;
    //function getVideoDecoderCounters: JDecoderCounters; cdecl;
    //function getAudioDecoderCounters: JDecoderCounters; cdecl;
    procedure addVideoListener(listener: JSimpleExoPlayer_VideoListener); cdecl;
    procedure removeVideoListener(listener: JSimpleExoPlayer_VideoListener); cdecl;
    //procedure setVideoListener(listener: JSimpleExoPlayer_VideoListener); cdecl; deprecated;
    //procedure clearVideoListener(listener: JSimpleExoPlayer_VideoListener); cdecl; deprecated;
    //procedure addTextOutput(listener: JTextOutput); cdecl;
    //procedure removeTextOutput(listener: JTextOutput); cdecl;
    //procedure setTextOutput(output: JTextOutput); cdecl; deprecated;
    //procedure clearTextOutput(output: JTextOutput); cdecl; deprecated;
    //procedure addMetadataOutput(listener: JMetadataOutput); cdecl;
    //procedure removeMetadataOutput(listener: JMetadataOutput); cdecl;
    //procedure setMetadataOutput(output: JMetadataOutput); cdecl; deprecated;
    //procedure clearMetadataOutput(output: JMetadataOutput); cdecl; deprecated;
    //procedure setVideoDebugListener(listener: JVideoRendererEventListener); cdecl; deprecated;
    //procedure addVideoDebugListener(listener: JVideoRendererEventListener); cdecl;
    //procedure removeVideoDebugListener(listener: JVideoRendererEventListener); cdecl;
    //procedure setAudioDebugListener(listener: JAudioRendererEventListener); cdecl; deprecated;
    //procedure addAudioDebugListener(listener: JAudioRendererEventListener); cdecl;
    //procedure removeAudioDebugListener(listener: JAudioRendererEventListener); cdecl;
  end;
  TJSimpleExoPlayer = class(TJavaGenericImport<JSimpleExoPlayerClass, JSimpleExoPlayer>) end;

  {**********************************************}
  JExoPlayerFactoryClass = interface(JObjectClass)
    ['{AE382CD9-C438-41A6-A83B-3650BA28F850}']
    {class} //function newInstance(renderers: TJavaObjectArray<JRenderer>; trackSelector: JTrackSelector): JExoPlayer; cdecl; overload;
    {class} //function newInstance(renderers: TJavaObjectArray<JRenderer>; trackSelector: JTrackSelector; loadControl: JLoadControl): JExoPlayer; cdecl; overload;
    {class} function newSimpleInstance(context: JContext; trackSelector: JTrackSelector): JSimpleExoPlayer; cdecl; overload;
    {class} //function newSimpleInstance(renderersFactory: JRenderersFactory; trackSelector: JTrackSelector): JSimpleExoPlayer; cdecl; overload;
    {class} //function newSimpleInstance(renderersFactory: JRenderersFactory; trackSelector: JTrackSelector; loadControl: JLoadControl): JSimpleExoPlayer; cdecl; overload;
    {class} //function newSimpleInstance(context: JContext; trackSelector: JTrackSelector; loadControl: JLoadControl): JSimpleExoPlayer; cdecl; overload; deprecated;
    {class} //function newSimpleInstance(context: JContext; trackSelector: JTrackSelector; loadControl: JLoadControl; drmSessionManager: JDrmSessionManager): JSimpleExoPlayer; cdecl; overload; deprecated;
    {class} //function newSimpleInstance(context: JContext; trackSelector: JTrackSelector; loadControl: JLoadControl; drmSessionManager: JDrmSessionManager; extensionRendererMode: Integer): JSimpleExoPlayer; cdecl; overload; deprecated;
    {class} //function newSimpleInstance(context: JContext; trackSelector: JTrackSelector; loadControl: JLoadControl; drmSessionManager: JDrmSessionManager; extensionRendererMode: Integer; allowedVideoJoiningTimeMs: Int64): JSimpleExoPlayer; cdecl; overload; deprecated;
  end;

  {***************************************************************}
  [JavaSignature('com/google/android/exoplayer2/ExoPlayerFactory')]
  JExoPlayerFactory = interface(JObject)
    ['{BC387B76-5B9D-42D0-B7D8-B252089C708E}']
  end;
  TJExoPlayerFactory = class(TJavaGenericImport<JExoPlayerFactoryClass, JExoPlayerFactory>) end;

  {**********************************************}
  JDataSource_FactoryClass = interface(IJavaClass)
    ['{F3647398-F5A3-43CE-992C-A8EFCBDD2E64}']
  end;

  {**************************************************************************}
  [JavaSignature('com/google/android/exoplayer2/upstream/DataSource$Factory')]
  JDataSource_Factory = interface(IJavaInstance)
    ['{C2E17D43-9101-4D1D-ACDC-B3DD5FC26671}']
    //function createDataSource: Jupstream_DataSource; cdecl;
  end;
  TJDataSource_Factory = class(TJavaGenericImport<JDataSource_FactoryClass, JDataSource_Factory>) end;

  {******************************************************************}
  JDefaultDataSourceFactoryClass = interface(JDataSource_FactoryClass)
    ['{2A8CA496-C561-45C8-AA7E-1601D5F01391}']
    {class} function init(context: JContext; userAgent: JString): JDefaultDataSourceFactory; cdecl; overload;
    {class} function init(context: JContext; userAgent: JString; listener: JTransferListener): JDefaultDataSourceFactory; cdecl; overload;
    {class} //function init(context: JContext; listener: JTransferListener; baseDataSourceFactory: JDataSource_Factory): JDefaultDataSourceFactory; cdecl; overload;
  end;

  {********************************************************************************}
  [JavaSignature('com/google/android/exoplayer2/upstream/DefaultDataSourceFactory')]
  JDefaultDataSourceFactory = interface(JDataSource_Factory)
    ['{722CE8B8-23F4-4C45-9B66-583C176940C1}']
  end;
  TJDefaultDataSourceFactory = class(TJavaGenericImport<JDefaultDataSourceFactoryClass, JDefaultDataSourceFactory>) end;

  {**********************************}
  JUtilClass = interface(JObjectClass)
    ['{CC0552B4-960B-4A20-9812-D2C13A7E42AB}']
    {class} function _GetDEVICE: JString; cdecl;
    {class} function _GetDEVICE_DEBUG_INFO: JString; cdecl;
    {class} function _GetMANUFACTURER: JString; cdecl;
    {class} function _GetMODEL: JString; cdecl;
    {class} function _GetSDK_INT: Integer; cdecl;
    {class} function toByteArray(inputStream: JInputStream): TJavaArray<Byte>; cdecl;
    {class} function isLocalFileUri(uri: Jnet_Uri): Boolean; cdecl;
    {class} function areEqual(o1: JObject; o2: JObject): Boolean; cdecl;
    {class} function &contains(items: TJavaObjectArray<JObject>; item: JObject): Boolean; cdecl;
    {class} function newSingleThreadExecutor(threadName: JString): JExecutorService; cdecl;
    {class} //procedure closeQuietly(dataSource: Jupstream_DataSource); cdecl; overload;
    {class} procedure closeQuietly(closeable: JCloseable); cdecl; overload;
    {class} function normalizeLanguageCode(language: JString): JString; cdecl;
    {class} function fromUtf8Bytes(bytes: TJavaArray<Byte>): JString; cdecl;
    {class} function getUtf8Bytes(value: JString): TJavaArray<Byte>; cdecl;
    {class} function isLinebreak(c: Integer): Boolean; cdecl;
    {class} function toLowerInvariant(text: JString): JString; cdecl;
    {class} function ceilDivide(numerator: Integer; denominator: Integer): Integer; cdecl; overload;
    {class} function ceilDivide(numerator: Int64; denominator: Int64): Int64; cdecl; overload;
    {class} function constrainValue(value: Integer; min: Integer; max: Integer): Integer; cdecl; overload;
    {class} function constrainValue(value: Single; min: Single; max: Single): Single; cdecl; overload;
    {class} function constrainValue(value: Int64; min: Int64; max: Int64): Int64; cdecl; overload;
    {class} function binarySearchFloor(&array: TJavaArray<Integer>; value: Integer; inclusive: Boolean; stayInBounds: Boolean): Integer; cdecl; overload;
    {class} function binarySearchFloor(&array: TJavaArray<Int64>; value: Int64; inclusive: Boolean; stayInBounds: Boolean): Integer; cdecl; overload;
    {class} //function binarySearchFloor(list: JList; value: JObject; inclusive: Boolean; stayInBounds: Boolean): Integer; cdecl; overload;
    {class} function binarySearchCeil(&array: TJavaArray<Int64>; value: Int64; inclusive: Boolean; stayInBounds: Boolean): Integer; cdecl; overload;
    {class} function binarySearchCeil(list: JList; value: JObject; inclusive: Boolean; stayInBounds: Boolean): Integer; cdecl; overload;
    {class} function parseXsDuration(value: JString): Int64; cdecl;
    {class} function parseXsDateTime(value: JString): Int64; cdecl;
    {class} function scaleLargeTimestamp(timestamp: Int64; multiplier: Int64; divisor: Int64): Int64; cdecl;
    {class} function scaleLargeTimestamps(timestamps: JList; multiplier: Int64; divisor: Int64): TJavaArray<Int64>; cdecl;
    {class} procedure scaleLargeTimestampsInPlace(timestamps: TJavaArray<Int64>; multiplier: Int64; divisor: Int64); cdecl;
    {class} function toArray(list: JList): TJavaArray<Integer>; cdecl;
    {class} function getIntegerCodeForString(&string: JString): Integer; cdecl;
    {class} function getBytesFromHexString(hexString: JString): TJavaArray<Byte>; cdecl;
    {class} function getCommaDelimitedSimpleClassNames(objects: TJavaObjectArray<JObject>): JString; cdecl;
    {class} function getUserAgent(context: JContext; applicationName: JString): JString; cdecl;
    {class} function getPcmEncoding(bitDepth: Integer): Integer; cdecl;
    {class} function getPcmFrameSize(pcmEncoding: Integer; channelCount: Integer): Integer; cdecl;
    {class} function getAudioUsageForStreamType(streamType: Integer): Integer; cdecl;
    {class} function getAudioContentTypeForStreamType(streamType: Integer): Integer; cdecl;
    {class} function getStreamTypeForAudioUsage(usage: Integer): Integer; cdecl;
    {class} function inferContentType(fileName: JString): Integer; cdecl; overload;
    {class} function inferContentType(uri: Jnet_Uri): Integer; cdecl; overload;
    {class} //function getStringForTime(builder: JStringBuilder; formatter: Jutil_Formatter; timeMs: Int64): JString; cdecl;
    {class} function getDefaultBufferSize(trackType: Integer): Integer; cdecl;
    {class} function escapeFileName(fileName: JString): JString; cdecl;
    {class} function unescapeFileName(fileName: JString): JString; cdecl;
    {class} procedure sneakyThrow(t: JThrowable); cdecl;
    {class} procedure recursiveDelete(fileOrDirectory: JFile); cdecl;
    {class} function createTempDirectory(context: JContext; prefix: JString): JFile; cdecl;
    {class} function createTempFile(context: JContext; prefix: JString): JFile; cdecl;
    {class} function crc(bytes: TJavaArray<Byte>; start: Integer; &end: Integer; initialValue: Integer): Integer; cdecl;
    {class} function getPhysicalDisplaySize(context: JContext): JPoint; cdecl; overload;
    {class} function getPhysicalDisplaySize(context: JContext; display: JDisplay): JPoint; cdecl; overload;
    {class} property DEVICE: JString read _GetDEVICE;
    {class} property DEVICE_DEBUG_INFO: JString read _GetDEVICE_DEBUG_INFO;
    {class} property MANUFACTURER: JString read _GetMANUFACTURER;
    {class} property MODEL: JString read _GetMODEL;
    {class} property SDK_INT: Integer read _GetSDK_INT;
  end;

  {********************************************************}
  [JavaSignature('com/google/android/exoplayer2/util/Util')]
  JUtil = interface(JObject)
    ['{F303252B-6A3B-4AE5-BED6-E3C0FE61E3D5}']
  end;
  TJUtil = class(TJavaGenericImport<JUtilClass, JUtil>) end;

  {***************************************}
  JMediaSourceClass = interface(IJavaClass)
    ['{6794DA8B-A9BA-4EE1-B053-084D62D09857}']
  end;

  {*****************************************************************}
  [JavaSignature('com/google/android/exoplayer2/source/MediaSource')]
  JMediaSource = interface(IJavaInstance)
    ['{D0C7B9A0-B83C-406F-9A8F-AF872A6C0BAA}']
    //procedure prepareSource(player: JExoPlayer; isTopLevelSource: Boolean; listener: JMediaSource_Listener); cdecl;
    //procedure maybeThrowSourceInfoRefreshError; cdecl;
    //function createPeriod(id: JMediaSource_MediaPeriodId; allocator: JAllocator): JMediaPeriod; cdecl;
    //procedure releasePeriod(mediaPeriod: JMediaPeriod); cdecl;
    //procedure releaseSource; cdecl;
  end;
  TJMediaSource = class(TJavaGenericImport<JMediaSourceClass, JMediaSource>) end;

  {****************************************************}
  JMediaSourceEventListenerClass = interface(IJavaClass)
    ['{E14B2651-5517-4C9B-A9B8-5AE30DD2276D}']
  end;

  {******************************************************************************}
  [JavaSignature('com/google/android/exoplayer2/source/MediaSourceEventListener')]
  JMediaSourceEventListener = interface(IJavaInstance)
    ['{34C1A75E-985F-4A9A-ADD7-D9B9F1F02273}']
    //procedure onLoadStarted(dataSpec: JDataSpec;
    //                        dataType: Integer;
    //                        trackType: Integer;
    //                        trackFormat: Jexoplayer2_Format;
    //                        trackSelectionReason: Integer;
    //                        trackSelectionData: JObject;
    //                        mediaStartTimeMs: Int64;
    //                        mediaEndTimeMs: Int64;
    //                        elapsedRealtimeMs: Int64); cdecl;
    //procedure onLoadCompleted(dataSpec: JDataSpec;
    //                          dataType: Integer;
    //                          trackType: Integer;
    //                          trackFormat: Jexoplayer2_Format;
    //                          trackSelectionReason: Integer;
    //                          trackSelectionData: JObject;
    //                          mediaStartTimeMs: Int64;
    //                          mediaEndTimeMs: Int64;
    //                          elapsedRealtimeMs: Int64;
    //                          loadDurationMs: Int64;
    //                          bytesLoaded: Int64); cdecl;
    //procedure onLoadCanceled(dataSpec: JDataSpec;
    //                         dataType: Integer;
    //                         trackType: Integer;
    //                         trackFormat: Jexoplayer2_Format;
    //                         trackSelectionReason: Integer;
    //                         trackSelectionData: JObject;
    //                         mediaStartTimeMs: Int64;
    //                         mediaEndTimeMs: Int64;
    //                         elapsedRealtimeMs: Int64;
    //                         loadDurationMs: Int64;
    //                         bytesLoaded: Int64); cdecl;
    //procedure onLoadError(dataSpec: JDataSpec;
    //                      dataType: Integer;
    //                      trackType: Integer;
    //                      trackFormat: Jexoplayer2_Format;
    //                      trackSelectionReason: Integer;
    //                      trackSelectionData: JObject;
    //                      mediaStartTimeMs: Int64;
    //                      mediaEndTimeMs: Int64;
    //                      elapsedRealtimeMs: Int64;
    //                      loadDurationMs: Int64;
    //                      bytesLoaded: Int64;
    //                      error: JIOException;
    //                      wasCanceled: Boolean); cdecl;
    //procedure onUpstreamDiscarded(trackType: Integer; mediaStartTimeMs: Int64; mediaEndTimeMs: Int64); cdecl;
    //procedure onDownstreamFormatChanged(trackType: Integer;
    //                                    trackFormat: Jexoplayer2_Format;
    //                                    trackSelectionReason: Integer;
    //                                    trackSelectionData: JObject;
    //                                    mediaTimeMs: Int64); cdecl;
  end;
  TJMediaSourceEventListener = class(TJavaGenericImport<JMediaSourceEventListenerClass, JMediaSourceEventListener>) end;

  {*************************************************************}
  JAdsMediaSource_MediaSourceFactoryClass = interface(IJavaClass)
    ['{1BFA10FD-7415-496E-BC82-D39386AB5977}']
  end;

  {*******************************************************************************************}
  [JavaSignature('com/google/android/exoplayer2/source/ads/AdsMediaSource$MediaSourceFactory')]
  JAdsMediaSource_MediaSourceFactory = interface(IJavaInstance)
    ['{8ECCE772-E38C-4E32-A9A4-17FA419DB612}']
    function createMediaSource(uri: Jnet_Uri; handler: JHandler; listener: JMediaSourceEventListener): JMediaSource; cdecl;
    function getSupportedTypes: TJavaArray<Integer>; cdecl;
  end;
  TJAdsMediaSource_MediaSourceFactory = class(TJavaGenericImport<JAdsMediaSource_MediaSourceFactoryClass, JAdsMediaSource_MediaSourceFactory>) end;

  {*******************************************************}
  JExtractorMediaSourceClass = interface(JMediaSourceClass)
    ['{0950AF2F-702C-4496-9FAF-1E3B9CE8FA6C}']
    {class} function _GetDEFAULT_LOADING_CHECK_INTERVAL_BYTES: Integer; cdecl;
    {class} function _GetDEFAULT_MIN_LOADABLE_RETRY_COUNT_LIVE: Integer; cdecl;
    {class} function _GetDEFAULT_MIN_LOADABLE_RETRY_COUNT_ON_DEMAND: Integer; cdecl;
    {class} function _GetMIN_RETRY_COUNT_DEFAULT_FOR_MEDIA: Integer; cdecl;
    {class} //function init(uri: Jnet_Uri; dataSourceFactory: JDataSource_Factory; extractorsFactory: JExtractorsFactory; eventHandler: JHandler; eventListener: JExtractorMediaSource_EventListener): JExtractorMediaSource; cdecl; overload; deprecated;
    {class} //function init(uri: Jnet_Uri; dataSourceFactory: JDataSource_Factory; extractorsFactory: JExtractorsFactory; eventHandler: JHandler; eventListener: JExtractorMediaSource_EventListener; customCacheKey: JString): JExtractorMediaSource; cdecl; overload; deprecated;
    {class} //function init(uri: Jnet_Uri;
            //              dataSourceFactory: JDataSource_Factory;
            //              extractorsFactory: JExtractorsFactory;
            //              minLoadableRetryCount: Integer;
            //              eventHandler: JHandler;
            //              eventListener: JExtractorMediaSource_EventListener;
            //              customCacheKey: JString;
            //              continueLoadingCheckIntervalBytes: Integer): JExtractorMediaSource; cdecl; overload;
    {class} property DEFAULT_LOADING_CHECK_INTERVAL_BYTES: Integer read _GetDEFAULT_LOADING_CHECK_INTERVAL_BYTES;
    {class} property DEFAULT_MIN_LOADABLE_RETRY_COUNT_LIVE: Integer read _GetDEFAULT_MIN_LOADABLE_RETRY_COUNT_LIVE;
    {class} property DEFAULT_MIN_LOADABLE_RETRY_COUNT_ON_DEMAND: Integer read _GetDEFAULT_MIN_LOADABLE_RETRY_COUNT_ON_DEMAND;
    {class} property MIN_RETRY_COUNT_DEFAULT_FOR_MEDIA: Integer read _GetMIN_RETRY_COUNT_DEFAULT_FOR_MEDIA;
  end;

  {**************************************************************************}
  [JavaSignature('com/google/android/exoplayer2/source/ExtractorMediaSource')]
  JExtractorMediaSource = interface(JMediaSource)
    ['{83091925-55B5-4E44-AACF-1AFEBBC756A9}']
    //procedure onSourceInfoRefreshed(durationUs: Int64; isSeekable: Boolean); cdecl;
  end;
  TJExtractorMediaSource = class(TJavaGenericImport<JExtractorMediaSourceClass, JExtractorMediaSource>) end;


  {*************************************************************************************}
  JExtractorMediaSource_FactoryClass = interface(JAdsMediaSource_MediaSourceFactoryClass)
    ['{4887DD78-39AB-4D63-8679-C0F8A5E13D06}']
    {class} function init(dataSourceFactory: JDataSource_Factory): JExtractorMediaSource_Factory; cdecl;
  end;

  {**********************************************************************************}
  [JavaSignature('com/google/android/exoplayer2/source/ExtractorMediaSource$Factory')]
  JExtractorMediaSource_Factory = interface(JAdsMediaSource_MediaSourceFactory)
    ['{14AD7BE1-8D95-46CD-A023-AFA6AA0B79CD}']
    //function setExtractorsFactory(extractorsFactory: JExtractorsFactory): JExtractorMediaSource_Factory; cdecl;
    function setCustomCacheKey(customCacheKey: JString): JExtractorMediaSource_Factory; cdecl;
    function setMinLoadableRetryCount(minLoadableRetryCount: Integer): JExtractorMediaSource_Factory; cdecl;
    function setContinueLoadingCheckIntervalBytes(continueLoadingCheckIntervalBytes: Integer): JExtractorMediaSource_Factory; cdecl;
    function createMediaSource(uri: Jnet_Uri): JExtractorMediaSource; cdecl; overload;
    function createMediaSource(uri: Jnet_Uri; Handler: JHandler; eventListener: JMediaSourceEventListener): JExtractorMediaSource; cdecl; overload;
  end;
  TJExtractorMediaSource_Factory = class(TJavaGenericImport<JExtractorMediaSource_FactoryClass, JExtractorMediaSource_Factory>) end;

  {*********************************************************}
  JSimpleExoPlayer_VideoListenerClass = interface(IJavaClass)
    ['{088F07EB-48EA-415A-A5E8-78D8058ACC4C}']
  end;

  {****************************************************************************}
  [JavaSignature('com/google/android/exoplayer2/SimpleExoPlayer$VideoListener')]
  JSimpleExoPlayer_VideoListener = interface(IJavaInstance)
    ['{39E0E4D8-03A0-4C9E-99DA-C616EE923158}']
    procedure onVideoSizeChanged(width: Integer; height: Integer; unappliedRotationDegrees: Integer; pixelWidthHeightRatio: Single); cdecl;
    procedure onRenderedFirstFrame; cdecl;
  end;
  TJSimpleExoPlayer_VideoListener = class(TJavaGenericImport<JSimpleExoPlayer_VideoListenerClass, JSimpleExoPlayer_VideoListener>) end;

  {**************************************}
  JTimelineClass = interface(JObjectClass)
    ['{4BA6283E-E1AE-4BA4-87D7-00A289D46A2B}']
    {class} function _GetEMPTY: JTimeline; cdecl;
    {class} function init: JTimeline; cdecl;
    {class} property EMPTY: JTimeline read _GetEMPTY;
  end;

  {*******************************************************}
  [JavaSignature('com/google/android/exoplayer2/Timeline')]
  JTimeline = interface(JObject)
    ['{E73F5194-2630-4616-ABD0-577436A1F06E}']
    function isEmpty: Boolean; cdecl;
    function getWindowCount: Integer; cdecl;
    function getNextWindowIndex(windowIndex: Integer; repeatMode: Integer; shuffleModeEnabled: Boolean): Integer; cdecl;
    function getPreviousWindowIndex(windowIndex: Integer; repeatMode: Integer; shuffleModeEnabled: Boolean): Integer; cdecl;
    function getLastWindowIndex(shuffleModeEnabled: Boolean): Integer; cdecl;
    function getFirstWindowIndex(shuffleModeEnabled: Boolean): Integer; cdecl;
    //function getWindow(windowIndex: Integer; window: JTimeline_Window): JTimeline_Window; cdecl; overload;
    //function getWindow(windowIndex: Integer; window: JTimeline_Window; setIds: Boolean): JTimeline_Window; cdecl; overload;
    //function getWindow(windowIndex: Integer; window: JTimeline_Window; setIds: Boolean; defaultPositionProjectionUs: Int64): JTimeline_Window; cdecl; overload;
    function getPeriodCount: Integer; cdecl;
    //function getNextPeriodIndex(periodIndex: Integer; period: JTimeline_Period; window: JTimeline_Window; repeatMode: Integer; shuffleModeEnabled: Boolean): Integer; cdecl;
    //function isLastPeriod(periodIndex: Integer; period: JTimeline_Period; window: JTimeline_Window; repeatMode: Integer; shuffleModeEnabled: Boolean): Boolean; cdecl;
    //function getPeriod(periodIndex: Integer; period: JTimeline_Period): JTimeline_Period; cdecl; overload;
    //function getPeriodPosition(window: JTimeline_Window; period: JTimeline_Period; windowIndex: Integer; windowPositionUs: Int64): JPair; cdecl; overload;
    //function getPeriodPosition(window: JTimeline_Window; period: JTimeline_Period; windowIndex: Integer; windowPositionUs: Int64; defaultPositionProjectionUs: Int64): JPair; cdecl; overload;
    //function getPeriod(periodIndex: Integer; period: JTimeline_Period; setIds: Boolean): JTimeline_Period; cdecl; overload;
    function getIndexOfPeriod(uid: JObject): Integer; cdecl;
  end;
  TJTimeline = class(TJavaGenericImport<JTimelineClass, JTimeline>) end;

  {************************************************}
  JPlayer_EventListenerClass = interface(IJavaClass)
    ['{5850B646-28C3-4143-9F28-E803AB2DB656}']
  end;

  {*********************************************}
  JTrackGroupArrayClass = interface(JObjectClass)
    ['{0F008A77-B922-481F-A9F7-30ABCD0DBFB2}']
    {class} function _GetEMPTY: JTrackGroupArray; cdecl;
    {class} property EMPTY: JTrackGroupArray read _GetEMPTY;
  end;

  {*********************************************************************}
  [JavaSignature('com/google/android/exoplayer2/source/TrackGroupArray')]
  JTrackGroupArray = interface(JObject)
    ['{E50EC7FB-9DA7-4227-8B82-D99591E626A3}']
    function _Getlength: Integer; cdecl;
    //function &get(index: Integer): JTrackGroup; cdecl;
    //function indexOf(group: JTrackGroup): Integer; cdecl;
    function isEmpty: Boolean; cdecl;
    property length: Integer read _Getlength;
  end;
  TJTrackGroupArray = class(TJavaGenericImport<JTrackGroupArrayClass, JTrackGroupArray>) end;

  {*************************************************}
  JTrackSelectionArrayClass = interface(JObjectClass)
    ['{E9194BEC-260C-44AC-85B3-E198AFE898FE}']
  end;

  {*********************************************************************************}
  [JavaSignature('com/google/android/exoplayer2/trackselection/TrackSelectionArray')]
  JTrackSelectionArray = interface(JObject)
    ['{D0C89CAC-E476-4299-991E-22BB73C44D77}']
    function _Getlength: Integer; cdecl;
    function &get(index: Integer): JTrackSelection; cdecl;
    function getAll: TJavaObjectArray<JTrackSelection>; cdecl;
    property length: Integer read _Getlength;
  end;
  TJTrackSelectionArray = class(TJavaGenericImport<JTrackSelectionArrayClass, JTrackSelectionArray>) end;

  {*****************************************************}
  JExoPlaybackExceptionClass = interface(JExceptionClass)
    ['{18A6697B-48A5-49BC-9E12-2327C07D4E03}']
    {class} function _GetTYPE_SOURCE: Integer; cdecl;
    {class} function _GetTYPE_RENDERER: Integer; cdecl;
    {class} function _GetTYPE_UNEXPECTED: Integer; cdecl;
    {class} function createForRenderer(cause: JException; rendererIndex: Integer): JExoPlaybackException; cdecl;
    {class} function createForSource(cause: JIOException): JExoPlaybackException; cdecl;
    {class} function createForUnexpected(cause: JRuntimeException): JExoPlaybackException; cdecl;
    {class} property TYPE_SOURCE: Integer read _GetTYPE_SOURCE;
    {class} property TYPE_RENDERER: Integer read _GetTYPE_RENDERER;
    {class} property TYPE_UNEXPECTED: Integer read _GetTYPE_UNEXPECTED;
  end;

  {*******************************************************************}
  [JavaSignature('com/google/android/exoplayer2/ExoPlaybackException')]
  JExoPlaybackException = interface(JException)
    ['{BF751CF9-7EBB-4A4B-9C75-F77D250FBC8F}']
    function _Gettype: Integer; cdecl;
    function _GetrendererIndex: Integer; cdecl;
    function getSourceException: JIOException; cdecl;
    function getRendererException: JException; cdecl;
    function getUnexpectedException: JRuntimeException; cdecl;
    property &type: Integer read _Gettype;
    property rendererIndex: Integer read _GetrendererIndex;
  end;
  TJExoPlaybackException = class(TJavaGenericImport<JExoPlaybackExceptionClass, JExoPlaybackException>) end;

  {************************************************}
  JPlaybackParametersClass = interface(JObjectClass)
    ['{5635D57F-7421-4C56-85B5-BA092A6FA1AF}']
    {class} function _GetDEFAULT: JPlaybackParameters; cdecl;
    {class} function init(speed: Single; pitch: Single): JPlaybackParameters; cdecl;
    {class} property DEFAULT: JPlaybackParameters read _GetDEFAULT;
  end;

  {*****************************************************************}
  [JavaSignature('com/google/android/exoplayer2/PlaybackParameters')]
  JPlaybackParameters = interface(JObject)
    ['{D130EDA4-A7BE-42C9-B1F5-07F63F98E73A}']
    function _Getspeed: Single; cdecl;
    function _Getpitch: Single; cdecl;
    function getSpeedAdjustedDurationUs(timeMs: Int64): Int64; cdecl;
    property speed: Single read _Getspeed;
    property pitch: Single read _Getpitch;
  end;
  TJPlaybackParameters = class(TJavaGenericImport<JPlaybackParametersClass, JPlaybackParameters>) end;

  {*******************************************************************}
  [JavaSignature('com/google/android/exoplayer2/Player$EventListener')]
  JPlayer_EventListener = interface(IJavaInstance)
    ['{96103C70-D148-4255-96AC-7D0C0B638C35}']
    procedure onTimelineChanged(timeline: JTimeline; manifest: JObject); cdecl;
    procedure onTracksChanged(trackGroups: JTrackGroupArray; trackSelections: JTrackSelectionArray); cdecl;
    procedure onLoadingChanged(isLoading: Boolean); cdecl;
    procedure onPlayerStateChanged(playWhenReady: Boolean; playbackState: Integer); cdecl;
    procedure onRepeatModeChanged(repeatMode: Integer); cdecl;
    procedure onShuffleModeEnabledChanged(shuffleModeEnabled: Boolean); cdecl;
    procedure onPlayerError(error: JExoPlaybackException); cdecl;
    procedure onPositionDiscontinuity(reason: Integer); cdecl;
    procedure onPlaybackParametersChanged(playbackParameters: JPlaybackParameters); cdecl;
    procedure onSeekProcessed; cdecl;
  end;
  TJPlayer_EventListener = class(TJavaGenericImport<JPlayer_EventListenerClass, JPlayer_EventListener>) end;

  {*************************************************}
  JHlsMediaSourceClass = interface(JMediaSourceClass)
    ['{90D27A39-B796-4810-99A0-7270EF3308E5}']
    {class} function _GetDEFAULT_MIN_LOADABLE_RETRY_COUNT: Integer; cdecl;
    {class} //function init(Uri: Jnet_Uri; dataSourceFactory: JDataSource_Factory; eventHandler: JHandler; eventListener: JMediaSourceEventListener): JHlsMediaSource; cdecl; overload; deprecated;
    {class} //function init(Uri: Jnet_Uri; dataSourceFactory: JDataSource_Factory; minLoadableRetryCount: Integer; eventHandler: JHandler; eventListener: JMediaSourceEventListener): JHlsMediaSource; cdecl; overload; deprecated;
    {class} //function init(Uri: Jnet_Uri; dataSourceFactory: JHlsDataSourceFactory; extractorFactory: JHlsExtractorFactory; minLoadableRetryCount: Integer; eventHandler: JHandler; eventListener: JMediaSourceEventListener; playlistParser: JParsingLoadable_Parser): JHlsMediaSource; cdecl; overload; deprecated;
    {class} property DEFAULT_MIN_LOADABLE_RETRY_COUNT: Integer read _GetDEFAULT_MIN_LOADABLE_RETRY_COUNT;
  end;

  {************************************************************************}
  [JavaSignature('com/google/android/exoplayer2/source/hls/HlsMediaSource')]
  JHlsMediaSource = interface(JMediaSource)
    ['{E51E0FDE-C0A6-4C34-98AC-DAA86B7B397B}']
  end;
  TJHlsMediaSource = class(TJavaGenericImport<JHlsMediaSourceClass, JHlsMediaSource>) end;

  {*******************************************************************************}
  JHlsMediaSource_FactoryClass = interface(JAdsMediaSource_MediaSourceFactoryClass)
    ['{9B289592-C697-434D-AAE8-9259C2231C73}']
    {class} //function init(hlsDataSourceFactory: JHlsDataSourceFactory): JHlsMediaSource_Factory; cdecl; overload;
    {class} function init(dataSourceFactory: JDataSource_Factory): JHlsMediaSource_Factory; cdecl; overload;
  end;

  {********************************************************************************}
  [JavaSignature('com/google/android/exoplayer2/source/hls/HlsMediaSource$Factory')]
  JHlsMediaSource_Factory = interface(JAdsMediaSource_MediaSourceFactory)
    ['{7991C8C2-A81A-4970-AB18-F1C856B42D2F}']
    //function setExtractorFactory(extractorFactory: JHlsExtractorFactory): JHlsMediaSource_Factory; cdecl;
    function setMinLoadableRetryCount(minLoadableRetryCount: Integer): JHlsMediaSource_Factory; cdecl;
    //function setPlaylistParser(playlistParser: JParsingLoadable_Parser): JHlsMediaSource_Factory; cdecl;
    function createMediaSource(playlistUri: Jnet_Uri): JHlsMediaSource; cdecl; overload;
    function createMediaSource(playlistUri: Jnet_Uri; eventHandler: JHandler; eventListener: JMediaSourceEventListener): JHlsMediaSource; cdecl; overload;
  end;
  TJHlsMediaSource_Factory = class(TJavaGenericImport<JHlsMediaSource_FactoryClass, JHlsMediaSource_Factory>) end;

  {********************************************}
  JTransferListenerClass = interface(IJavaClass)
    ['{AFC443DD-A2E6-405D-B212-073E05E23DD5}']
  end;

  {************************************************************************}
  [JavaSignature('com/google/android/exoplayer2/upstream/TransferListener')]
  JTransferListener = interface(IJavaInstance)
    ['{BD871527-8064-4FC1-82CE-7FDDEB4672AC}']
    //procedure onTransferStart(source: JObject; dataSpec: JDataSpec); cdecl;
    procedure onBytesTransferred(source: JObject; bytesTransferred: Integer); cdecl;
    procedure onTransferEnd(source: JObject); cdecl;
  end;
  TJTransferListener = class(TJavaGenericImport<JTransferListenerClass, JTransferListener>) end;

implementation

procedure RegisterTypes;
begin
  TRegTypes.RegisterType('ALAndroidExoPlayerApi.JBandwidthMeter', TypeInfo(ALAndroidExoPlayerApi.JBandwidthMeter));
  TRegTypes.RegisterType('ALAndroidExoPlayerApi.JBandwidthMeter_EventListener', TypeInfo(ALAndroidExoPlayerApi.JBandwidthMeter_EventListener));
  TRegTypes.RegisterType('ALAndroidExoPlayerApi.JDefaultBandwidthMeter', TypeInfo(ALAndroidExoPlayerApi.JDefaultBandwidthMeter));
  TRegTypes.RegisterType('ALAndroidExoPlayerApi.JTrackSelection', TypeInfo(ALAndroidExoPlayerApi.JTrackSelection));
  TRegTypes.RegisterType('ALAndroidExoPlayerApi.JTrackSelection_Factory', TypeInfo(ALAndroidExoPlayerApi.JTrackSelection_Factory));
  TRegTypes.RegisterType('ALAndroidExoPlayerApi.JBaseTrackSelection', TypeInfo(ALAndroidExoPlayerApi.JBaseTrackSelection));
  TRegTypes.RegisterType('ALAndroidExoPlayerApi.JAdaptiveTrackSelection', TypeInfo(ALAndroidExoPlayerApi.JAdaptiveTrackSelection));
  TRegTypes.RegisterType('ALAndroidExoPlayerApi.JAdaptiveTrackSelection_Factory', TypeInfo(ALAndroidExoPlayerApi.JAdaptiveTrackSelection_Factory));
  TRegTypes.RegisterType('ALAndroidExoPlayerApi.JTrackSelector', TypeInfo(ALAndroidExoPlayerApi.JTrackSelector));
  TRegTypes.RegisterType('ALAndroidExoPlayerApi.JMappingTrackSelector', TypeInfo(ALAndroidExoPlayerApi.JMappingTrackSelector));
  TRegTypes.RegisterType('ALAndroidExoPlayerApi.JDefaultTrackSelector', TypeInfo(ALAndroidExoPlayerApi.JDefaultTrackSelector));
  TRegTypes.RegisterType('ALAndroidExoPlayerApi.JPlayer', TypeInfo(ALAndroidExoPlayerApi.JPlayer));
  TRegTypes.RegisterType('ALAndroidExoPlayerApi.JExoPlayer', TypeInfo(ALAndroidExoPlayerApi.JExoPlayer));
  TRegTypes.RegisterType('ALAndroidExoPlayerApi.JSimpleExoPlayer', TypeInfo(ALAndroidExoPlayerApi.JSimpleExoPlayer));
  TRegTypes.RegisterType('ALAndroidExoPlayerApi.JExoPlayerFactory', TypeInfo(ALAndroidExoPlayerApi.JExoPlayerFactory));
  TRegTypes.RegisterType('ALAndroidExoPlayerApi.JDataSource_Factory', TypeInfo(ALAndroidExoPlayerApi.JDataSource_Factory));
  TRegTypes.RegisterType('ALAndroidExoPlayerApi.JDefaultDataSourceFactory', TypeInfo(ALAndroidExoPlayerApi.JDefaultDataSourceFactory));
  TRegTypes.RegisterType('ALAndroidExoPlayerApi.JUtil', TypeInfo(ALAndroidExoPlayerApi.JUtil));
  TRegTypes.RegisterType('ALAndroidExoPlayerApi.JMediaSource', TypeInfo(ALAndroidExoPlayerApi.JMediaSource));
  TRegTypes.RegisterType('ALAndroidExoPlayerApi.JMediaSourceEventListener', TypeInfo(ALAndroidExoPlayerApi.JMediaSourceEventListener));
  TRegTypes.RegisterType('ALAndroidExoPlayerApi.JAdsMediaSource_MediaSourceFactory', TypeInfo(ALAndroidExoPlayerApi.JAdsMediaSource_MediaSourceFactory));
  TRegTypes.RegisterType('ALAndroidExoPlayerApi.JExtractorMediaSource', TypeInfo(ALAndroidExoPlayerApi.JExtractorMediaSource));
  TRegTypes.RegisterType('ALAndroidExoPlayerApi.JExtractorMediaSource_Factory', TypeInfo(ALAndroidExoPlayerApi.JExtractorMediaSource_Factory));
  TRegTypes.RegisterType('ALAndroidExoPlayerApi.JSimpleExoPlayer_VideoListener', TypeInfo(ALAndroidExoPlayerApi.JSimpleExoPlayer_VideoListener));
  TRegTypes.RegisterType('ALAndroidExoPlayerApi.JTimeline', TypeInfo(ALAndroidExoPlayerApi.JTimeline));
  TRegTypes.RegisterType('ALAndroidExoPlayerApi.JTrackGroupArray', TypeInfo(ALAndroidExoPlayerApi.JTrackGroupArray));
  TRegTypes.RegisterType('ALAndroidExoPlayerApi.JTrackSelectionArray', TypeInfo(ALAndroidExoPlayerApi.JTrackSelectionArray));
  TRegTypes.RegisterType('ALAndroidExoPlayerApi.JExoPlaybackException', TypeInfo(ALAndroidExoPlayerApi.JExoPlaybackException));
  TRegTypes.RegisterType('ALAndroidExoPlayerApi.JPlaybackParameters', TypeInfo(ALAndroidExoPlayerApi.JPlaybackParameters));
  TRegTypes.RegisterType('ALAndroidExoPlayerApi.JPlayer_EventListener', TypeInfo(ALAndroidExoPlayerApi.JPlayer_EventListener));
  TRegTypes.RegisterType('ALAndroidExoPlayerApi.JHlsMediaSource', TypeInfo(ALAndroidExoPlayerApi.JHlsMediaSource));
  TRegTypes.RegisterType('ALAndroidExoPlayerApi.JHlsMediaSource_Factory', TypeInfo(ALAndroidExoPlayerApi.JHlsMediaSource_Factory));
  TRegTypes.RegisterType('ALAndroidExoPlayerApi.JTransferListener', TypeInfo(ALAndroidExoPlayerApi.JTransferListener));
end;

initialization
  RegisterTypes;

end.
