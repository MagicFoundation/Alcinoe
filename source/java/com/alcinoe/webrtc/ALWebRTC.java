package com.alcinoe.webrtc;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.io.File;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import android.os.Handler;
import android.content.Context;
import android.os.Environment;
import android.os.ParcelFileDescriptor;
import android.util.Log;
import android.support.annotation.Nullable;
import org.webrtc.AudioSource;
import org.webrtc.AudioTrack;
import org.webrtc.CameraVideoCapturer;
import org.webrtc.DataChannel;
import org.webrtc.DefaultVideoDecoderFactory;
import org.webrtc.DefaultVideoEncoderFactory;
import org.webrtc.EglBase;
import org.webrtc.IceCandidate;
import org.webrtc.MediaConstraints;
import org.webrtc.MediaStream;
import org.webrtc.MediaStreamTrack;
import org.webrtc.PeerConnection;
import org.webrtc.PeerConnectionFactory;
import org.webrtc.RtpParameters;
import org.webrtc.RtpReceiver;
import org.webrtc.RtpSender;
import org.webrtc.RtpTransceiver;
import org.webrtc.SdpObserver;
import org.webrtc.SessionDescription;
import org.webrtc.SoftwareVideoDecoderFactory;
import org.webrtc.SoftwareVideoEncoderFactory;
import org.webrtc.SurfaceTextureHelper;
import org.webrtc.VideoCapturer;
import org.webrtc.VideoDecoderFactory;
import org.webrtc.VideoEncoderFactory;
import org.webrtc.VideoSink;
import org.webrtc.VideoSource;
import org.webrtc.VideoTrack;
import org.webrtc.audio.AudioDeviceModule;
import org.webrtc.audio.JavaAudioDeviceModule;
import org.webrtc.audio.JavaAudioDeviceModule.AudioRecordErrorCallback;
import org.webrtc.audio.JavaAudioDeviceModule.AudioTrackErrorCallback;
import org.webrtc.Camera1Enumerator;
import org.webrtc.Camera2Enumerator;
import org.webrtc.CameraEnumerator;
import org.webrtc.VideoFrame;

public class ALWebRTC {

  public static final int ERROR_CREATE_SDP = 1;
  public static final int ERROR_SET_SDP = 2;
  public static final int ERROR_AUDIO_RECORD_INIT = 3;
  public static final int ERROR_AUDIO_RECORD_START = 4;
  public static final int ERROR_AUDIO_RECORD = 5;
  public static final int ERROR_AUDIO_TRACK_INIT = 6;
  public static final int ERROR_AUDIO_TRACK_START = 7;
  public static final int ERROR_AUDIO_TRACK = 8;
  public static final String VIDEO_TRACK_ID = "ARDAMSv0";
  public static final String AUDIO_TRACK_ID = "ARDAMSa0";
  public static final String VIDEO_TRACK_TYPE = "video";
  private static final String TAG = "ALWebRTC";
  private static final String VIDEO_CODEC_VP8 = "VP8";
  private static final String VIDEO_CODEC_VP9 = "VP9";
  private static final String VIDEO_CODEC_H264 = "H264";
  private static final String VIDEO_CODEC_H264_BASELINE = "H264 Baseline";
  private static final String VIDEO_CODEC_H264_HIGH = "H264 High";
  private static final String AUDIO_CODEC_OPUS = "opus";
  private static final String AUDIO_CODEC_ISAC = "ISAC";
  private static final String VIDEO_FLEXFEC_FIELDTRIAL = "WebRTC-FlexFEC-03-Advertised/Enabled/WebRTC-FlexFEC-03/Enabled/";
  private static final String VIDEO_VP8_INTEL_HW_ENCODER_FIELDTRIAL = "WebRTC-IntelVP8/Enabled/";
  private static final String DISABLE_WEBRTC_AGC_FIELDTRIAL = "WebRTC-Audio-MinimizeResamplingOnMobile/Enabled/";
  private static final String AUDIO_ECHO_CANCELLATION_CONSTRAINT = "googEchoCancellation";
  private static final String AUDIO_AUTO_GAIN_CONTROL_CONSTRAINT = "googAutoGainControl";
  private static final String AUDIO_HIGH_PASS_FILTER_CONSTRAINT = "googHighpassFilter";
  private static final String AUDIO_NOISE_SUPPRESSION_CONSTRAINT = "googNoiseSuppression";
  private static final String VIDEO_CODEC_PARAM_START_BITRATE = "x-google-start-bitrate";
  private static final String AUDIO_CODEC_PARAM_BITRATE = "maxaveragebitrate";
  private static final int BPS_IN_KBPS = 1000;
  
  private Context mAppContext;
  private Handler mHandler;
  private PeerConnectionParameters mPeerConnectionParameters;
  private PeerConnectionFactory mPeerConnectionFactory = null;
  private boolean mPreferIsac;
  private EglBase mEglBase;
  List<PeerConnection.IceServer> mIceServers;
  private Listener mListener = null;
  private AudioSource mAudioSource = null;
  private VideoSource mVideoSource = null;
  private AudioTrack mLocalAudioTrack = null;
  private VideoTrack mLocalVideoTrack = null;
  private VideoTrack mRemoteVideoTrack = null;
  private SurfaceTextureHelper mSurfaceTextureHelper = null;
  private VideoCapturer mVideoCapturer = null;
  private List<IceCandidate> mQueuedRemoteCandidates = null;
  private PeerConnection mPeerConnection = null;
  private DataChannel mDataChannel = null;
  private SessionDescription mLocalSdp = null; // either offer or answer SDP
  private boolean mIsInitiator = false;
  private final PeerConnectionObserver mPeerConnectionObserver = new PeerConnectionObserver();
  private final SDPObserver mSDPObserver = new SDPObserver();
  private final RemoteProxyVideoSink mRemoteProxyVideoSink = new RemoteProxyVideoSink();
  private final LocalProxyVideoSink mLocalProxyVideoSink = new LocalProxyVideoSink();
  
  public static class PeerConnectionParameters {
    public boolean videoCallEnabled = true; /* Enable video in a call. */
    public int videoWidth = 1920;
    public int videoHeight = 1080;
    public int videoFps = 0; /* Camera fps. */
    public int videoMaxBitrate = 0; /* Video encoder maximum bitrate in kbps. */ 
    public String videoCodec = VIDEO_CODEC_VP8; /* Default video codec. VP8 / VP9 / H264 Baseline / H264 High */
    public boolean videoCodecHwAcceleration = true; /* Use hardware accelerated video codec (if available). */
    public int audioStartBitrate = 0; /* Audio codec bitrate in kbps. */
    public String audioCodec = AUDIO_CODEC_OPUS; /* Default audio codec. OPUS / ISAC */
    public boolean noAudioProcessing = false; /* Disable audio processing pipeline. */
    public boolean aecDump = false; /* Enable diagnostic audio recordings. */
    public boolean disableBuiltInAEC = false; /* Disable hardware AEC. */
    public boolean disableBuiltInNS = false; /* Disable hardware NS. */
    public boolean dataChannelEnabled = false;  /* Enable datachannel. */
    public boolean dataChannelOrdered = true; /* Order messages. */
    public int dataChannelMaxRetransmitTimeMs = -1; /* Max delay to retransmit. */
    public int dataChannelMaxRetransmits = -1; /* Max attempts to retransmit. */
    public String dataChannelProtocol = ""; /* Subprotocol. */
    public boolean dataChannelNegotiated = false; /* Negotiated. */
    public int dataChannelId = -1; /* data channel id. */
  }
  
  public static void initializeLibrary(Context appContext) {
    
    // Initialize PeerConnectionFactory globals.
    PeerConnectionFactory.initialize(
      PeerConnectionFactory.InitializationOptions.builder(appContext)
        //Field trials allow webrtc clients (such as Chrome) to turn on feature code
        //in binaries out in the field and gather information with that.
        .setFieldTrials(getFieldTrials())
        //.setEnableInternalTracer(true)
        //.setNativeLibraryLoader()
        //.setNativeLibraryName()
        //.setInjectableLogger()
        .createInitializationOptions());
    
  }
  
  public static void finalizeLibrary() {    
    //PeerConnectionFactory.shutdownInternalTracer();    
  }
  
  private static String getFieldTrials() {
    String fieldTrials = "";
    fieldTrials += VIDEO_VP8_INTEL_HW_ENCODER_FIELDTRIAL;
    //fieldTrials += VIDEO_FLEXFEC_FIELDTRIAL;
    //fieldTrials += DISABLE_WEBRTC_AGC_FIELDTRIAL;
    return fieldTrials;
  }

  public interface Listener {
    
    // Callback fired once one local frame is captured
    void onLocalFrameAvailable(int textureId, int width, int height, int rotation);
    
    // Callback fired once one remote frame is captured
    void onRemoteFrameAvailable(int textureId, int width, int height, int rotation);

    // Callback fired once local SDP is created and set.
    void onLocalDescription(final SessionDescription sdp);

    // Callback fired once local Ice candidate is generated.
    void onIceCandidate(final IceCandidate candidate);

    // Callback fired once local ICE candidates are removed.
    void onIceCandidatesRemoved(final IceCandidate[] candidates);

    // Callback fired once connection is established or closed (IceConnectionState is CONNECTED or DISCONNECTED).
    void onIceConnectionChange(final PeerConnection.IceConnectionState newState);

    // Callback fired once peer connection error happened.
    void onError(int code, final String description);

  }
                                                                                    
  private class LocalProxyVideoSink implements VideoSink {

    @Override
    synchronized public void onFrame(VideoFrame frame) {
    
      // Implementations should call frame.retain() if they need to hold a reference to the frame after
      // this function returns. Each call to retain() should be followed by a call to frame.release()
      // when the reference is no longer needed.
    
      VideoFrame.TextureBuffer textureBuffer = (VideoFrame.TextureBuffer) frame.getBuffer();
      final int textureId = textureBuffer.getTextureId();
      final int width = textureBuffer.getWidth();
      final int height = textureBuffer.getHeight();
      final int rotation = frame.getRotation();
      mHandler.post(new Runnable() {
        @Override
        public void run() {
          if (mListener != null) mListener.onLocalFrameAvailable(textureId, 
                                                                 width, 
                                                                 height,
                                                                 rotation);             
        }
      });
      
    }
    
  }
  
  private class RemoteProxyVideoSink implements VideoSink {
    
    @Override
    synchronized public void onFrame(VideoFrame frame) {
    
      // Implementations should call frame.retain() if they need to hold a reference to the frame after
      // this function returns. Each call to retain() should be followed by a call to frame.release()
      // when the reference is no longer needed.
    
      VideoFrame.TextureBuffer textureBuffer = (VideoFrame.TextureBuffer) frame.getBuffer();
      final int textureId = textureBuffer.getTextureId();
      final int width = textureBuffer.getWidth();
      final int height = textureBuffer.getHeight();
      final int rotation = frame.getRotation();
      mHandler.post(new Runnable() {
        @Override
        public void run() {
          if (mListener != null) mListener.onRemoteFrameAvailable(textureId, 
                                                                  width, 
                                                                  height,
                                                                  rotation);             
        }
      });
      
    }
        
  }
                                                                                                   
  public ALWebRTC(Context appContext, long eglContext, List<PeerConnection.IceServer> iceServers, PeerConnectionParameters peerConnectionParameters) throws ClassNotFoundException, InstantiationException, IllegalAccessException, NoSuchMethodException, InvocationTargetException {
    
    /* init local vars */
    mAppContext = appContext;
    mHandler = new Handler(mAppContext.getMainLooper());
    mPeerConnectionParameters = peerConnectionParameters;
    mPreferIsac = (mPeerConnectionParameters.audioCodec != null) && 
                  (mPeerConnectionParameters.audioCodec.equals(AUDIO_CODEC_ISAC));
    mEglBase = EglBase.createEgl14(createSharedEGLContextObj(eglContext), EglBase.CONFIG_PLAIN);   
    mIceServers = iceServers;
                                    
  }
    
  private String getSdpVideoCodecName() {
    switch (mPeerConnectionParameters.videoCodec) {
      case VIDEO_CODEC_VP8: return VIDEO_CODEC_VP8;
      case VIDEO_CODEC_VP9: return VIDEO_CODEC_VP9;
      case VIDEO_CODEC_H264_HIGH:
      case VIDEO_CODEC_H264_BASELINE: return VIDEO_CODEC_H264;
      default: return VIDEO_CODEC_VP8;
    }
  }
                                                                                        
  private PeerConnectionFactory createPeerConnectionFactory(PeerConnectionFactory.Options options) {

    final AudioDeviceModule audioDeviceModule = createJavaAudioDevice();

    final boolean enableH264HighProfile = VIDEO_CODEC_H264_HIGH.equals(mPeerConnectionParameters.videoCodec);
    final VideoEncoderFactory encoderFactory;
    final VideoDecoderFactory decoderFactory;

    if (mPeerConnectionParameters.videoCodecHwAcceleration) {
      encoderFactory = new DefaultVideoEncoderFactory(mEglBase.getEglBaseContext(), true /* enableIntelVp8Encoder */, enableH264HighProfile);
      decoderFactory = new DefaultVideoDecoderFactory(mEglBase.getEglBaseContext());
    } 
    else {
      encoderFactory = new SoftwareVideoEncoderFactory();
      decoderFactory = new SoftwareVideoDecoderFactory();
    }

    PeerConnectionFactory peerConnectionFactory = PeerConnectionFactory.builder()
                                                    .setOptions(options)
                                                    .setAudioDeviceModule(audioDeviceModule)
                                                    //.setAudioEncoderFactoryFactory(AudioEncoderFactoryFactory audioEncoderFactoryFactory)
                                                    //.setAudioDecoderFactoryFactory(AudioDecoderFactoryFactory audioDecoderFactoryFactory)
                                                    .setVideoEncoderFactory(encoderFactory)
                                                    .setVideoDecoderFactory(decoderFactory)
                                                    //.setAudioProcessingFactory(AudioProcessingFactory audioProcessingFactory)
                                                    //.setFecControllerFactoryFactoryInterface(FecControllerFactoryFactoryInterface fecControllerFactoryFactory)
                                                    //.setMediaTransportFactoryFactory(MediaTransportFactoryFactory mediaTransportFactoryFactory)
                                                    .createPeerConnectionFactory();
    audioDeviceModule.release();
    return peerConnectionFactory;
    
  }

  private AudioDeviceModule createJavaAudioDevice() {
      
    // Set audio record error callbacks.
    AudioRecordErrorCallback audioRecordErrorCallback = new AudioRecordErrorCallback() {
      
      @Override
      public void onWebRtcAudioRecordInitError(String errorMessage) {
        mHandler.post(new Runnable() {
          @Override
          public void run() {
            if (mListener != null) mListener.onError(ERROR_AUDIO_RECORD_INIT, errorMessage);        
          }
        }); 
      }

      @Override
      public void onWebRtcAudioRecordStartError(JavaAudioDeviceModule.AudioRecordStartErrorCode errorCode, String errorMessage) {
        mHandler.post(new Runnable() {
          @Override
          public void run() {
            if (mListener != null) mListener.onError(ERROR_AUDIO_RECORD_START, errorCode + ". " + errorMessage);
          }
        });
      }

      @Override
      public void onWebRtcAudioRecordError(String errorMessage) {
        mHandler.post(new Runnable() {
          @Override
          public void run() {
            if (mListener != null) mListener.onError(ERROR_AUDIO_RECORD, errorMessage); 
          }
        });
      }
      
    };

    // Set audio track error callbacks.
    AudioTrackErrorCallback audioTrackErrorCallback = new AudioTrackErrorCallback() {
      
      @Override
      public void onWebRtcAudioTrackInitError(String errorMessage) {
        mHandler.post(new Runnable() {
          @Override
          public void run() {
            if (mListener != null) mListener.onError(ERROR_AUDIO_TRACK_INIT, errorMessage);   
          }
        }); 
      }

      @Override
      public void onWebRtcAudioTrackStartError(JavaAudioDeviceModule.AudioTrackStartErrorCode errorCode, String errorMessage) {
        mHandler.post(new Runnable() {
          @Override
          public void run() {
            if (mListener != null) mListener.onError(ERROR_AUDIO_TRACK_START, errorCode + ". " + errorMessage); 
          }
        });
      }

      @Override
      public void onWebRtcAudioTrackError(String errorMessage) {
        mHandler.post(new Runnable() {
          @Override
          public void run() {
            if (mListener != null) mListener.onError(ERROR_AUDIO_TRACK, errorMessage); 
          }
        });
      }
      
    };

    return JavaAudioDeviceModule.builder(mAppContext)
        
      //Call this method if the default handling of querying the native sample rate shall be
      //overridden. Can be useful on some devices where the available Android APIs are known to
      //return invalid results.        
      //.setSampleRate(int sampleRate)

      //Call this to change the audio source. The argument should be one of the values from
      //android.media.MediaRecorder.AudioSource. The default is AudioSource.VOICE_COMMUNICATION.
      //.setAudioSource(int audioSource)       
      
      //Control if stereo input should be used or not. The default is mono.
      //.setUseStereoInput(boolean useStereoInput)
      
      //Control if stereo output should be used or not. The default is mono.
      //.setUseStereoOutput(boolean useStereoOutput)
      
      //Set a callback to listen to the raw audio input from the AudioRecord.
      .setSamplesReadyCallback(null/*saveRecordedAudioToFile*/)
      
      //Control if the built-in HW acoustic echo canceler should be used or not. The default is on if
      //it is supported. It is possible to query support by calling isBuiltInAcousticEchoCancelerSupported().
      .setUseHardwareAcousticEchoCanceler(!mPeerConnectionParameters.disableBuiltInAEC)
      
      //Control if the built-in HW noise suppressor should be used or not. The default is on if it is
      //supported. It is possible to query support by calling isBuiltInNoiseSuppressorSupported().
      .setUseHardwareNoiseSuppressor(!mPeerConnectionParameters.disableBuiltInNS)
      
      //Set a callback to retrieve errors from the AudioRecord.
      .setAudioRecordErrorCallback(audioRecordErrorCallback)
      
      //Set a callback to retrieve errors from the AudioTrack.
      .setAudioTrackErrorCallback(audioTrackErrorCallback)
      
      //Construct an AudioDeviceModule based on the supplied arguments. The caller takes ownership
      //and is responsible for calling release().
      .createAudioDeviceModule();
        
  }
          
  public void setListener(Listener listener) {
    mListener = listener;
  } 
          
  private android.opengl.EGLContext createSharedEGLContextObj(long handle) throws ClassNotFoundException, InstantiationException, IllegalAccessException, NoSuchMethodException, InvocationTargetException {
    Class<?> classType =Class.forName("android.opengl.EGLContext"); 
    Class<?>[] types = new Class[] { long.class };
    Constructor constructor=classType.getDeclaredConstructor(types);
    constructor.setAccessible(true);     
    Object object=constructor.newInstance(handle);
    return (android.opengl.EGLContext) object;
  }

  private @Nullable VideoCapturer createVideoCapturer() {
    final VideoCapturer videoCapturer;
    if (Camera2Enumerator.isSupported(mAppContext)) {
      Log.v(TAG, "Creating capturer using camera2 API.");
      videoCapturer = createCameraCapturer(new Camera2Enumerator(mAppContext));
    } 
    else {
      Log.v(TAG, "Creating capturer using camera1 API.");
      videoCapturer = createCameraCapturer(new Camera1Enumerator(true/*captureToTexture*/));
    }
    return videoCapturer;
  }
                                                                              
  private @Nullable VideoCapturer createCameraCapturer(CameraEnumerator enumerator) {
    final String[] deviceNames = enumerator.getDeviceNames();

    // First, try to find front facing camera
    for (String deviceName : deviceNames) {
      if (enumerator.isFrontFacing(deviceName)) {
        Log.v(TAG, "Creating front facing camera capturer.");
        VideoCapturer videoCapturer = enumerator.createCapturer(deviceName, null);
        if (videoCapturer != null) { return videoCapturer; }
      }
    }

    // Front facing camera not found, try something else
    for (String deviceName : deviceNames) {
      if (!enumerator.isFrontFacing(deviceName)) {
        Log.v(TAG, "Creating other camera capturer.");
        VideoCapturer videoCapturer = enumerator.createCapturer(deviceName, null);
        if (videoCapturer != null) { return videoCapturer; }
      }
    }

    return null;
  }
                                                                                
  public boolean start() throws ClassNotFoundException, InstantiationException, IllegalAccessException, NoSuchMethodException, InvocationTargetException {
    
    /* create mPeerConnectionFactory */
    mPeerConnectionFactory = createPeerConnectionFactory(new PeerConnectionFactory.Options());

    //create mLocalVideoTrack
    if (mPeerConnectionParameters.videoCallEnabled) {

      // create a Capturer which takes the image/video from the device’s camera.      
      mVideoCapturer = createVideoCapturer();
      if (mVideoCapturer == null) { return false; }
            
      // Construct a new SurfaceTextureHelper sharing OpenGL resources with |sharedContext|. A dedicated
      // thread and handler is created for handling the SurfaceTexture. May return null if EGL fails to
      // initialize a pixel buffer surface and make it current.
      // At the begining I was thinking to force the SurfaceTextureHelper to use Main UI thread handler, so in this
      // way the onframe event will be called in the main UI thread, but this make this bug :
      // https://stackoverflow.com/questions/53564583/how-to-draw-the-opengl-texture-retrieved-by-remotevideotrack
      // on the remote part (not on the local one)
      mSurfaceTextureHelper = SurfaceTextureHelper.create("CaptureThread", mEglBase.getEglBaseContext());

      //create the mVideoSource
      mVideoSource = mPeerConnectionFactory.createVideoSource(mVideoCapturer.isScreencast());

      // This function is used to initialize the camera thread, the android application context, and the
      // capture observer. It will be called only once and before any startCapture() request. The
      // camera thread is guaranteed to be valid until dispose() is called. If the VideoCapturer wants
      // to deliver texture frames, it should do this by rendering on the SurfaceTexture in
      // {@code surfaceTextureHelper}, register itself as a listener, and forward the frames to
      // CapturerObserver.onFrameCaptured(). The caller still has ownership of {@code
      // surfaceTextureHelper} and is responsible for making sure surfaceTextureHelper.dispose() is
      // called. This also means that the caller can reuse the SurfaceTextureHelper to initialize a new
      // VideoCapturer once the previous VideoCapturer has been disposed.
      mVideoCapturer.initialize(mSurfaceTextureHelper, mAppContext, mVideoSource.getCapturerObserver());
   
      //Start capturing frames in a format that is as close as possible to {@code width x height} and {@code framerate}.
      mVideoCapturer.startCapture(mPeerConnectionParameters.videoWidth, /*width*/ 
                                  mPeerConnectionParameters.videoHeight, /*height*/ 
                                  mPeerConnectionParameters.videoFps); /*framerate*/
      
      mLocalVideoTrack = mPeerConnectionFactory.createVideoTrack(VIDEO_TRACK_ID, mVideoSource);
      mLocalVideoTrack.setEnabled(true);
      mLocalVideoTrack.addSink(mLocalProxyVideoSink);
 
    }

    // Create mLocalAudioTrack
    MediaConstraints audioConstraints = new MediaConstraints();
    if (mPeerConnectionParameters.noAudioProcessing) {
      audioConstraints.mandatory.add(new MediaConstraints.KeyValuePair(AUDIO_ECHO_CANCELLATION_CONSTRAINT, "false"));
      audioConstraints.mandatory.add(new MediaConstraints.KeyValuePair(AUDIO_AUTO_GAIN_CONTROL_CONSTRAINT, "false"));
      audioConstraints.mandatory.add(new MediaConstraints.KeyValuePair(AUDIO_HIGH_PASS_FILTER_CONSTRAINT, "false"));
      audioConstraints.mandatory.add(new MediaConstraints.KeyValuePair(AUDIO_NOISE_SUPPRESSION_CONSTRAINT, "false"));
    }
    mAudioSource = mPeerConnectionFactory.createAudioSource(audioConstraints);    
    mLocalAudioTrack = mPeerConnectionFactory.createAudioTrack(AUDIO_TRACK_ID, mAudioSource);
    mLocalAudioTrack.setEnabled(true);

    // create mQueuedRemoteCandidates
    mQueuedRemoteCandidates = new ArrayList<>();

    //create mPeerConnection
    PeerConnection.RTCConfiguration rtcConfig = new PeerConnection.RTCConfiguration(mIceServers);
    rtcConfig.tcpCandidatePolicy = PeerConnection.TcpCandidatePolicy.DISABLED; // TCP candidates are only useful when connecting to a server that supports ICE-TCP.
    rtcConfig.bundlePolicy = PeerConnection.BundlePolicy.MAXBUNDLE;
    rtcConfig.rtcpMuxPolicy = PeerConnection.RtcpMuxPolicy.REQUIRE;
    rtcConfig.continualGatheringPolicy = PeerConnection.ContinualGatheringPolicy.GATHER_CONTINUALLY;
    rtcConfig.keyType = PeerConnection.KeyType.ECDSA; // Use ECDSA encryption.
    rtcConfig.enableDtlsSrtp = true; // Enable DTLS for normal calls and disable for loopback calls.
    rtcConfig.sdpSemantics = PeerConnection.SdpSemantics.UNIFIED_PLAN;
    mPeerConnection = mPeerConnectionFactory.createPeerConnection(rtcConfig, mPeerConnectionObserver);

    //create mDataChannel
    if (mPeerConnectionParameters.dataChannelEnabled) {
      DataChannel.Init init = new DataChannel.Init();
      init.ordered = mPeerConnectionParameters.dataChannelOrdered;
      init.negotiated = mPeerConnectionParameters.dataChannelNegotiated;
      init.maxRetransmits = mPeerConnectionParameters.dataChannelMaxRetransmits;
      init.maxRetransmitTimeMs = mPeerConnectionParameters.dataChannelMaxRetransmitTimeMs;
      init.id = mPeerConnectionParameters.dataChannelId;
      init.protocol = mPeerConnectionParameters.dataChannelProtocol;
      mDataChannel = mPeerConnection.createDataChannel("ALWebRTC data", init);
    }

    // Enable diagnostic logging for messages of |severity| to the platform debug
    // output. On Android, the output will be directed to Logcat.
    // Note: this function starts collecting the output of the RTC_LOG() macros.
    // TODO(bugs.webrtc.org/8491): Remove NoSynchronizedMethodCheck suppression.
    // NOTE: this _must_ happen while |factory| is alive!
    // Logging.enableLogToDebugOutput(Logging.Severity.LS_INFO);

    List<String> mediaStreamLabels = Collections.singletonList("ARDAMS");
    if (mPeerConnectionParameters.videoCallEnabled) {
      mPeerConnection.addTrack(mLocalVideoTrack, mediaStreamLabels);
      // We can add the renderers right away because we don't need to wait for an
      // answer to get the remote track.
      mRemoteVideoTrack = getRemoteVideoTrack();
      mRemoteVideoTrack.setEnabled(true);
      mRemoteVideoTrack.addSink(mRemoteProxyVideoSink);
    }
    mPeerConnection.addTrack(mLocalAudioTrack, mediaStreamLabels);

    if (mPeerConnectionParameters.aecDump) {
      try {
        ParcelFileDescriptor aecDumpFileDescriptor = ParcelFileDescriptor.open(
                                                       new File(Environment.getExternalStorageDirectory().getPath() + File.separator + "Download/audio.aecdump"),
                                                       ParcelFileDescriptor.MODE_READ_WRITE | ParcelFileDescriptor.MODE_CREATE | ParcelFileDescriptor.MODE_TRUNCATE);
        mPeerConnectionFactory.startAecDump(aecDumpFileDescriptor.detachFd(), -1);
      } catch (IOException e) {
        Log.e(TAG, "Can not open aecdump file", e);
      }
    }

    //return
    return true;
    
  }   
                                                                                
  public void stop() {
    mListener = null;
    if (mPeerConnectionParameters.aecDump) { mPeerConnectionFactory.stopAecDump(); }
    if (mDataChannel != null) {
      mDataChannel.dispose();
      mDataChannel = null;
    }
    if (mPeerConnection != null) {
      mPeerConnection.dispose();
      mPeerConnection = null;
    }
    if (mAudioSource != null) {
      mAudioSource.dispose();
      mAudioSource = null;
    }
    if (mVideoCapturer != null) {
      try {
        mVideoCapturer.stopCapture();
      } catch (InterruptedException e) {
        throw new RuntimeException(e);
      }
      mVideoCapturer.dispose();
      mVideoCapturer = null;
    }
    if (mVideoSource != null) {
      mVideoSource.dispose();
      mVideoSource = null;
    }
    if (mSurfaceTextureHelper != null) {
      mSurfaceTextureHelper.dispose();
      mSurfaceTextureHelper = null;
    }
    mPeerConnectionFactory.dispose();
    mPeerConnectionFactory = null;
    mEglBase.release();
  }
  
  public void setAudioEnabled(final boolean enable) {
    if (mLocalAudioTrack != null) { mLocalAudioTrack.setEnabled(enable); }
  }

  public void setVideoEnabled(final boolean enable) {
    if (mLocalVideoTrack != null) { mLocalVideoTrack.setEnabled(enable); }
    if (mRemoteVideoTrack != null) { mRemoteVideoTrack.setEnabled(enable); }
  }
    
  private RtpSender findVideoSender() {
    for (RtpSender rtpSender : mPeerConnection.getSenders()) {
      if (rtpSender.track() != null) {
        String trackType = rtpSender.track().kind();
        if (trackType.equals(VIDEO_TRACK_TYPE)) {
          return rtpSender;
        }
      }
    }
    return null;
  }

  public boolean setVideoMaxBitrate(@Nullable final Integer maxBitrateKbps) {
    RtpSender rtpSender = findVideoSender();
    if (rtpSender == null) { 
      Log.w(TAG, "Sender is not ready.");
      return false; 
    }

    RtpParameters parameters = rtpSender.getParameters();
    if (parameters.encodings.size() == 0) {
      Log.w(TAG, "RtpParameters are not ready.");
      return false;
    }

    for (RtpParameters.Encoding encoding : parameters.encodings) {
      // Null value means no limit.
      encoding.maxBitrateBps = maxBitrateKbps == null ? null : maxBitrateKbps * BPS_IN_KBPS;
    }
    if (!rtpSender.setParameters(parameters)) {
      Log.e(TAG, "RtpSender.setParameters failed.");
      return false; 
    }
    
    return true;
  }
    
  // Returns the remote VideoTrack, assuming there is only one.
  private @Nullable VideoTrack getRemoteVideoTrack() {
    for (RtpTransceiver transceiver : mPeerConnection.getTransceivers()) {
      MediaStreamTrack track = transceiver.getReceiver().track();
      if (track instanceof VideoTrack) {
        return (VideoTrack) track;
      }
    }
    return null;
  }    
                                                                                  
  // Implementation detail: observe ICE & stream changes and react accordingly.
  private class PeerConnectionObserver implements PeerConnection.Observer {
    
    @Override
    public void onIceCandidate(final IceCandidate candidate) {
      mHandler.post(new Runnable() {
        @Override
        public void run() {
          if (mListener != null) mListener.onIceCandidate(candidate);             
        }
      });
    }

    @Override
    public void onIceCandidatesRemoved(final IceCandidate[] candidates) {
      mHandler.post(new Runnable() {
        @Override
        public void run() {
          if (mListener != null) mListener.onIceCandidatesRemoved(candidates);             
        }
      });
    }

    @Override
    public void onSignalingChange(PeerConnection.SignalingState newState) {
      Log.v(TAG, "SignalingState: " + newState);
    }

    @Override
    public void onIceConnectionChange(final PeerConnection.IceConnectionState newState) {
      mHandler.post(new Runnable() {
        @Override
        public void run() {
          if (mListener != null) mListener.onIceConnectionChange(newState);             
        }
      });
    }

    @Override
    public void onIceGatheringChange(PeerConnection.IceGatheringState newState) {
      Log.v(TAG, "IceGatheringState: " + newState);
    }

    @Override
    public void onIceConnectionReceivingChange(boolean receiving) {
      Log.v(TAG, "IceConnectionReceiving changed to " + receiving);
    }

    @Override
    public void onAddStream(final MediaStream stream) {
      Log.v(TAG, "Add Stream");  
    }

    @Override
    public void onRemoveStream(final MediaStream stream) {
      Log.v(TAG, "Remove Stream");    
    }

    @Override
    public void onDataChannel(final DataChannel dc) {
      Log.d(TAG, "New Data channel " + dc.label());

      if (!mPeerConnectionParameters.dataChannelEnabled) return;

      dc.registerObserver(new DataChannel.Observer() {
        
        @Override
        public void onBufferedAmountChange(long previousAmount) {
          Log.d(TAG, "Data channel buffered amount changed: " + dc.label() + ": " + dc.state());
        }

        @Override
        public void onStateChange() {
          Log.d(TAG, "Data channel state changed: " + dc.label() + ": " + dc.state());
        }

        @Override
        public void onMessage(final DataChannel.Buffer buffer) {
          if (buffer.binary) {
            Log.d(TAG, "Received binary msg over " + dc);
            return;
          }
          ByteBuffer data = buffer.data;
          final byte[] bytes = new byte[data.capacity()];
          data.get(bytes);
          String strData = new String(bytes, Charset.forName("UTF-8"));
          Log.d(TAG, "Got msg: " + strData + " over " + dc);
        }
        
      });
    }

    @Override
    public void onRenegotiationNeeded() {
      Log.v(TAG, "Renegotiation Needed");    
      // No need to do anything; AppRTC follows a pre-agreed-upon
      // signaling/negotiation protocol.
    }

    @Override
    public void onAddTrack(final RtpReceiver receiver, final MediaStream[] mediaStreams) {
      Log.v(TAG, "Add Track");    
    }
    
  }

  public void createOffer() {
    
    //The createOffer() method of the RTCPeerConnection interface initiates the creation 
    //of an SDP offer for the purpose of starting a new WebRTC connection to a remote peer. 
    //The SDP offer includes information about any MediaStreamTracks already attached to the 
    //WebRTC session, codec, and options supported by the browser, and any candidates already 
    //gathered by the ICE agent, for the purpose of being sent over the signaling channel to 
    //a potential peer to request a connection or to update the configuration of an existing connection.   
    mIsInitiator = true;
    mPeerConnection.createOffer(mSDPObserver, createSdpMediaConstraints());
    
  }

  public void createAnswer() {
    
    //The createAnswer() method on the RTCPeerConnection interface creates an SDP answer to an 
    //offer received from a remote peer during the offer/answer negotiation of a WebRTC connection. 
    //The answer contains information about any media already attached to the session, codecs and 
    //options supported by the browser, and any ICE candidates already gathered. The answer is delivered 
    //to the returned Promise, and should then be sent to the source of the offer to continue the 
    //negotiation process.
    mIsInitiator = false;
    mPeerConnection.createAnswer(mSDPObserver, createSdpMediaConstraints());
    
  }

  private MediaConstraints createSdpMediaConstraints(){
    MediaConstraints sdpMediaConstraints = new MediaConstraints();
    sdpMediaConstraints.mandatory.add(new MediaConstraints.KeyValuePair("OfferToReceiveAudio", "true"));
    sdpMediaConstraints.mandatory.add(new MediaConstraints.KeyValuePair("OfferToReceiveVideo", Boolean.toString(mPeerConnectionParameters.videoCallEnabled)));   
    return sdpMediaConstraints;
  }
                                                                                
  // Implementation detail: handle offer creation/signaling and answer setting,
  // as well as adding remote ICE candidates once the answer SDP is set.
  private class SDPObserver implements SdpObserver {
    
    /** Called on success of Create{Offer,Answer}(). */
    @Override
    public void onCreateSuccess(SessionDescription sdp) {
    
     //check mLocalSdp is null
     if (mLocalSdp != null) {
        mHandler.post(new Runnable() {
          @Override
          public void run() {
            if (mListener != null) mListener.onError(ERROR_CREATE_SDP, "Multiple SDP create.");   
          }
        });
        return;
      }
      
      //init mLocalSdp
      String sdpDescription = sdp.description;
      if (mPreferIsac) { sdpDescription = preferCodec(sdpDescription, AUDIO_CODEC_ISAC, true); }
      if (mPeerConnectionParameters.videoCallEnabled) { sdpDescription = preferCodec(sdpDescription, getSdpVideoCodecName(), false); }
      mLocalSdp = new SessionDescription(sdp.type, sdpDescription);
        
      //The RTCPeerConnection.setLocalDescription() method changes the local description associated with 
      //the connection. This description specifies the properties of the local end of the connection, 
      //including the media format. The method takes a single parameter—the session description—and it 
      //returns a Promise which is fulfilled once the description has been changed, asynchronously.
      //If setLocalDescription() is called while a connection is already in place, it means renegotiation 
      //is underway (possibly to adapt to changing network conditions). Because descriptions will be 
      //exchanged until the two peers agree on a configuration, the description submitted by calling 
      //setLocalDescription() does not immediately take effect. Instead, the current connection configuration 
      //remains in place until negotiation is complete. Only then does the agreed-upon configuration take effect.         
      mPeerConnection.setLocalDescription(mSDPObserver, mLocalSdp);
        
    }

    /** Called on success of Set{Local,Remote}Description(). */
    @Override
    public void onSetSuccess() {
   
      if (mIsInitiator) {
        // For offering peer connection we first create offer and set
        // local SDP, then after receiving answer set remote SDP.
        if (mPeerConnection.getRemoteDescription() == null) {
          // We've just set our local SDP so time to send it.
          Log.d(TAG, "Local SDP set succesfully");
          mHandler.post(new Runnable() {
            @Override
            public void run() {
              if (mListener != null) mListener.onLocalDescription(mLocalSdp);    
              mLocalSdp = null;        
            }
          }); 
        } 
        else {
          // We've just set remote description, so drain remote
          // and send local ICE candidates.
          Log.d(TAG, "Remote SDP set succesfully");
          mHandler.post(new Runnable() {
            @Override
            public void run() {
              drainCandidates();        
            }
          }); 
        }
      } 
      else {
        // For answering peer connection we set remote SDP and then
        // create answer and set local SDP.
        if (mPeerConnection.getLocalDescription() != null) {
          // We've just set our local SDP so time to send it, drain
          // remote and send local ICE candidates.
          Log.d(TAG, "Local SDP set succesfully");
          mHandler.post(new Runnable() {
            @Override
            public void run() {
              if (mListener != null) mListener.onLocalDescription(mLocalSdp); 
              mLocalSdp = null;         
              drainCandidates();
            }
          }); 
        } 
        else {
          // We've just set remote SDP - do nothing for now -
          // answer will be created soon.
          Log.d(TAG, "Remote SDP set succesfully");
        }
      }
    }

    /** Called on error of Create{Offer,Answer}(). */
    @Override
    public void onCreateFailure(String error) {
      mHandler.post(new Runnable() {
        @Override
        public void run() {
          if (mListener != null) mListener.onError(ERROR_CREATE_SDP, error);
        }
      });
    }

    /** Called on error of Set{Local,Remote}Description(). */
    @Override
    public void onSetFailure(String error) {
       mHandler.post(new Runnable() {
        @Override
        public void run() {
          if (mListener != null) mListener.onError(ERROR_SET_SDP, error);
        }
      });
    }
  
  }
                                                                                
  private void drainCandidates() {
    if (mQueuedRemoteCandidates != null) {
      for (IceCandidate candidate : mQueuedRemoteCandidates) {
        mPeerConnection.addIceCandidate(candidate);
      }
      mQueuedRemoteCandidates = null;
    }
  }
                                                                              
  private static String preferCodec(String sdpDescription, String codec, boolean isAudio) {
    final String[] lines = sdpDescription.split("\r\n");
    final int mLineIndex = findMediaDescriptionLine(isAudio, lines);
    if (mLineIndex == -1) {
      Log.w(TAG, "No mediaDescription line, so can't prefer " + codec);
      return sdpDescription;
    }
    // A list with all the payload types with name |codec|. The payload types are integers in the
    // range 96-127, but they are stored as strings here.
    final List<String> codecPayloadTypes = new ArrayList<>();
    // a=rtpmap:<payload type> <encoding name>/<clock rate> [/<encoding parameters>]
    final Pattern codecPattern = Pattern.compile("^a=rtpmap:(\\d+) " + codec + "(/\\d+)+[\r]?$");
    for (String line : lines) {
      Matcher codecMatcher = codecPattern.matcher(line);
      if (codecMatcher.matches()) { codecPayloadTypes.add(codecMatcher.group(1)); }
    }
    if (codecPayloadTypes.isEmpty()) {
      Log.w(TAG, "No payload types with name " + codec);
      return sdpDescription;
    }
    final String newMLine = movePayloadTypesToFront(codecPayloadTypes, lines[mLineIndex]);
    if (newMLine == null) { return sdpDescription; }
    lines[mLineIndex] = newMLine;
    return joinString(Arrays.asList(lines), "\r\n", true /* delimiterAtEnd */);
  }
                                                                              
  private static @Nullable String movePayloadTypesToFront(List<String> preferredPayloadTypes, String mLine) {
    // The format of the media description line should be: m=<media> <port> <proto> <fmt> ...
    final List<String> origLineParts = Arrays.asList(mLine.split(" "));
    if (origLineParts.size() <= 3) {
      Log.e(TAG, "Wrong SDP media description format: " + mLine);
      return null;
    }
    final List<String> header = origLineParts.subList(0, 3);
    final List<String> unpreferredPayloadTypes = new ArrayList<>(origLineParts.subList(3, origLineParts.size()));
    unpreferredPayloadTypes.removeAll(preferredPayloadTypes);
    // Reconstruct the line with |preferredPayloadTypes| moved to the beginning of the payload types.
    final List<String> newLineParts = new ArrayList<>();
    newLineParts.addAll(header);
    newLineParts.addAll(preferredPayloadTypes);
    newLineParts.addAll(unpreferredPayloadTypes);
    return joinString(newLineParts, " ", false /* delimiterAtEnd */);
  }
                                                                              
  private static String joinString(Iterable<? extends CharSequence> s, String delimiter, boolean delimiterAtEnd) {
    Iterator<? extends CharSequence> iter = s.iterator();
    if (!iter.hasNext()) { return ""; }
    StringBuilder buffer = new StringBuilder(iter.next());
    while (iter.hasNext()) {
      buffer.append(delimiter).append(iter.next());
    }
    if (delimiterAtEnd) { buffer.append(delimiter); }
    return buffer.toString();
  }
                                                                              
  /** Returns the line number containing "m=audio|video", or -1 if no such line exists. */
  private static int findMediaDescriptionLine(boolean isAudio, String[] sdpLines) {
    final String mediaDescription = isAudio ? "m=audio " : "m=video ";
    for (int i = 0; i < sdpLines.length; ++i) {
      if (sdpLines[i].startsWith(mediaDescription)) { return i; }
    }
    return -1;
  }
                                                                              
  @SuppressWarnings("StringSplitter")
  private static String setStartBitrate(String codec, boolean isVideoCodec, String sdpDescription, int bitrateKbps) {
   
    String[] lines = sdpDescription.split("\r\n");
    int rtpmapLineIndex = -1;
    boolean sdpFormatUpdated = false;
    String codecRtpMap = null;
    // Search for codec rtpmap in format
    // a=rtpmap:<payload type> <encoding name>/<clock rate> [/<encoding parameters>]
    String regex = "^a=rtpmap:(\\d+) " + codec + "(/\\d+)+[\r]?$";
    Pattern codecPattern = Pattern.compile(regex);
    for (int i = 0; i < lines.length; i++) {
      Matcher codecMatcher = codecPattern.matcher(lines[i]);
      if (codecMatcher.matches()) {
        codecRtpMap = codecMatcher.group(1);
        rtpmapLineIndex = i;
        break;
      }
    }
    if (codecRtpMap == null) {
      Log.w(TAG, "No rtpmap for " + codec + " codec");
      return sdpDescription;
    }

    // Check if a=fmtp string already exist in remote SDP for this codec and
    // update it with new bitrate parameter.
    regex = "^a=fmtp:" + codecRtpMap + " \\w+=\\d+.*[\r]?$";
    codecPattern = Pattern.compile(regex);
    for (int i = 0; i < lines.length; i++) {
      Matcher codecMatcher = codecPattern.matcher(lines[i]);
      if (codecMatcher.matches()) {
        if (isVideoCodec) { lines[i] += "; " + VIDEO_CODEC_PARAM_START_BITRATE + "=" + bitrateKbps; } 
        else { lines[i] += "; " + AUDIO_CODEC_PARAM_BITRATE + "=" + (bitrateKbps * 1000); }
        sdpFormatUpdated = true;
        break;
      }
    }

    StringBuilder newSdpDescription = new StringBuilder();
    for (int i = 0; i < lines.length; i++) {
      newSdpDescription.append(lines[i]).append("\r\n");
      // Append new a=fmtp line if no such line exist for a codec.
      if (!sdpFormatUpdated && i == rtpmapLineIndex) {
        String bitrateSet;
        if (isVideoCodec) { bitrateSet = "a=fmtp:" + codecRtpMap + " " + VIDEO_CODEC_PARAM_START_BITRATE + "=" + bitrateKbps; } 
        else { bitrateSet = "a=fmtp:" + codecRtpMap + " " + AUDIO_CODEC_PARAM_BITRATE + "=" + (bitrateKbps * 1000); }
        newSdpDescription.append(bitrateSet).append("\r\n");
      }
    }
    return newSdpDescription.toString();    
  
  }
                                                                                                                                                                
  public void setRemoteDescription(SessionDescription.Type sdpType, String sdpDescription) {
    if (mPreferIsac) { sdpDescription = preferCodec(sdpDescription, AUDIO_CODEC_ISAC, true); }
    if (mPeerConnectionParameters.videoCallEnabled) { sdpDescription = preferCodec(sdpDescription, getSdpVideoCodecName(), false); }
    if (mPeerConnectionParameters.audioStartBitrate > 0) { sdpDescription = setStartBitrate(AUDIO_CODEC_OPUS, false, sdpDescription, mPeerConnectionParameters.audioStartBitrate); }
    SessionDescription sdpRemote = new SessionDescription(sdpType, sdpDescription);
    mPeerConnection.setRemoteDescription(mSDPObserver, sdpRemote);
  }
                                                                                
  public void addRemoteIceCandidate(String sdpMid, int sdpMLineIndex, String sdp) {
    IceCandidate iceCandidate = new IceCandidate(sdpMid, sdpMLineIndex, sdp);
    if (mQueuedRemoteCandidates != null) { mQueuedRemoteCandidates.add(iceCandidate); } 
    else { mPeerConnection.addIceCandidate(iceCandidate); }
  }
  
  public void removeRemoteIceCandidates(final IceCandidate[] candidates) {
    
    // Drain the queued remote candidates if there is any so that
    // they are processed in the proper order.
    drainCandidates();
    mPeerConnection.removeIceCandidates(candidates);
    
  }
  
  public void switchCamera() {
    if (mVideoCapturer instanceof CameraVideoCapturer) {
      CameraVideoCapturer cameraVideoCapturer = (CameraVideoCapturer) mVideoCapturer;
      cameraVideoCapturer.switchCamera(null);
    } 
    else { Log.e(TAG, "Will not switch camera, video caputurer is not a camera"); }
  }

  public void changeCaptureFormat(int width, int height, int framerate) {
    mVideoSource.adaptOutputFormat(width, height, framerate);
  }
  
}