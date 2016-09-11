package android.support.v4.media;

import android.app.PendingIntent;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.media.AudioManager;
import android.media.AudioManager.OnAudioFocusChangeListener;
import android.media.RemoteControlClient;
import android.media.RemoteControlClient.OnGetPlaybackPositionListener;
import android.media.RemoteControlClient.OnPlaybackPositionUpdateListener;
import android.util.Log;
import android.view.KeyEvent;
import android.view.View;
import android.view.ViewTreeObserver.OnWindowAttachListener;
import android.view.ViewTreeObserver.OnWindowFocusChangeListener;
import com.google.android.gms.drive.DriveFile;

class TransportMediatorJellybeanMR2 implements OnGetPlaybackPositionListener, OnPlaybackPositionUpdateListener {
    OnAudioFocusChangeListener mAudioFocusChangeListener;
    boolean mAudioFocused;
    final AudioManager mAudioManager;
    final Context mContext;
    boolean mFocused;
    final Intent mIntent;
    final BroadcastReceiver mMediaButtonReceiver;
    PendingIntent mPendingIntent;
    int mPlayState;
    final String mReceiverAction;
    final IntentFilter mReceiverFilter;
    RemoteControlClient mRemoteControl;
    final View mTargetView;
    final TransportMediatorCallback mTransportCallback;
    final OnWindowAttachListener mWindowAttachListener;
    final OnWindowFocusChangeListener mWindowFocusListener;

    public TransportMediatorJellybeanMR2(Context context, AudioManager audioManager, View view, TransportMediatorCallback transportCallback) {
        this.mWindowAttachListener = new OnWindowAttachListener() {
            public void onWindowAttached() {
                TransportMediatorJellybeanMR2.this.windowAttached();
            }

            public void onWindowDetached() {
                TransportMediatorJellybeanMR2.this.windowDetached();
            }
        };
        this.mWindowFocusListener = new OnWindowFocusChangeListener() {
            public void onWindowFocusChanged(boolean hasFocus) {
                if (hasFocus) {
                    TransportMediatorJellybeanMR2.this.gainFocus();
                } else {
                    TransportMediatorJellybeanMR2.this.loseFocus();
                }
            }
        };
        this.mMediaButtonReceiver = new BroadcastReceiver() {
            public void onReceive(Context context, Intent intent) {
                try {
                    TransportMediatorJellybeanMR2.this.mTransportCallback.handleKey((KeyEvent) intent.getParcelableExtra("android.intent.extra.KEY_EVENT"));
                } catch (ClassCastException e) {
                    Log.w("TransportController", e);
                }
            }
        };
        this.mAudioFocusChangeListener = new OnAudioFocusChangeListener() {
            public void onAudioFocusChange(int focusChange) {
                TransportMediatorJellybeanMR2.this.mTransportCallback.handleAudioFocusChange(focusChange);
            }
        };
        this.mPlayState = 0;
        this.mContext = context;
        this.mAudioManager = audioManager;
        this.mTargetView = view;
        this.mTransportCallback = transportCallback;
        this.mReceiverAction = context.getPackageName() + ":transport:" + System.identityHashCode(this);
        this.mIntent = new Intent(this.mReceiverAction);
        this.mIntent.setPackage(context.getPackageName());
        this.mReceiverFilter = new IntentFilter();
        this.mReceiverFilter.addAction(this.mReceiverAction);
        this.mTargetView.getViewTreeObserver().addOnWindowAttachListener(this.mWindowAttachListener);
        this.mTargetView.getViewTreeObserver().addOnWindowFocusChangeListener(this.mWindowFocusListener);
    }

    public Object getRemoteControlClient() {
        return this.mRemoteControl;
    }

    public void destroy() {
        windowDetached();
        this.mTargetView.getViewTreeObserver().removeOnWindowAttachListener(this.mWindowAttachListener);
        this.mTargetView.getViewTreeObserver().removeOnWindowFocusChangeListener(this.mWindowFocusListener);
    }

    void windowAttached() {
        this.mContext.registerReceiver(this.mMediaButtonReceiver, this.mReceiverFilter);
        this.mPendingIntent = PendingIntent.getBroadcast(this.mContext, 0, this.mIntent, DriveFile.MODE_READ_ONLY);
        this.mRemoteControl = new RemoteControlClient(this.mPendingIntent);
        this.mRemoteControl.setOnGetPlaybackPositionListener(this);
        this.mRemoteControl.setPlaybackPositionUpdateListener(this);
    }

    void gainFocus() {
        if (!this.mFocused) {
            this.mFocused = true;
            this.mAudioManager.registerMediaButtonEventReceiver(this.mPendingIntent);
            this.mAudioManager.registerRemoteControlClient(this.mRemoteControl);
            if (this.mPlayState == 3) {
                takeAudioFocus();
            }
        }
    }

    void takeAudioFocus() {
        if (!this.mAudioFocused) {
            this.mAudioFocused = true;
            this.mAudioManager.requestAudioFocus(this.mAudioFocusChangeListener, 3, 1);
        }
    }

    public void startPlaying() {
        if (this.mPlayState != 3) {
            this.mPlayState = 3;
            this.mRemoteControl.setPlaybackState(3);
        }
        if (this.mFocused) {
            takeAudioFocus();
        }
    }

    public long onGetPlaybackPosition() {
        return this.mTransportCallback.getPlaybackPosition();
    }

    public void onPlaybackPositionUpdate(long newPositionMs) {
        this.mTransportCallback.playbackPositionUpdate(newPositionMs);
    }

    public void refreshState(boolean playing, long position, int transportControls) {
        if (this.mRemoteControl != null) {
            this.mRemoteControl.setPlaybackState(playing ? 3 : 1, position, playing ? 1.0f : 0.0f);
            this.mRemoteControl.setTransportControlFlags(transportControls);
        }
    }

    public void pausePlaying() {
        if (this.mPlayState == 3) {
            this.mPlayState = 2;
            this.mRemoteControl.setPlaybackState(2);
        }
        dropAudioFocus();
    }

    public void stopPlaying() {
        if (this.mPlayState != 1) {
            this.mPlayState = 1;
            this.mRemoteControl.setPlaybackState(1);
        }
        dropAudioFocus();
    }

    void dropAudioFocus() {
        if (this.mAudioFocused) {
            this.mAudioFocused = false;
            this.mAudioManager.abandonAudioFocus(this.mAudioFocusChangeListener);
        }
    }

    void loseFocus() {
        dropAudioFocus();
        if (this.mFocused) {
            this.mFocused = false;
            this.mAudioManager.unregisterRemoteControlClient(this.mRemoteControl);
            this.mAudioManager.unregisterMediaButtonEventReceiver(this.mPendingIntent);
        }
    }

    void windowDetached() {
        loseFocus();
        if (this.mPendingIntent != null) {
            this.mContext.unregisterReceiver(this.mMediaButtonReceiver);
            this.mPendingIntent.cancel();
            this.mPendingIntent = null;
            this.mRemoteControl = null;
        }
    }
}
