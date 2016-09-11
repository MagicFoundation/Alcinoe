package com.embarcadero.firemonkey;

import android.annotation.SuppressLint;
import android.app.Dialog;
import android.app.NativeActivity;
import android.content.Intent;
import android.graphics.Point;
import android.os.Build;
import android.os.Build.VERSION;
import android.os.Bundle;
import android.os.Process;
import android.provider.Settings.Secure;
import android.support.v4.view.MotionEventCompat;
import android.support.v4.view.accessibility.AccessibilityNodeInfoCompat;
import android.util.DisplayMetrics;
import android.util.Log;
import android.view.Display;
import android.view.KeyEvent;
import android.view.SurfaceHolder;
import android.view.SurfaceHolder.Callback2;
import android.view.View;
import android.view.ViewGroup;
import android.view.ViewGroup.LayoutParams;
import com.embarcadero.firemonkey.medialibrary.FMXMediaLibrary;
import com.embarcadero.firemonkey.medialibrary.FMXMediaLibraryListener;
import com.embarcadero.firemonkey.text.FMXTextEditorProxy;
import com.embarcadero.firemonkey.text.VKStateChangeListener;
import com.embarcadero.rtl.notifications.NotificationInfo;
import com.embarcadero.rtl.notifications.NotificationPublisher;
import com.google.android.gms.games.multiplayer.turnbased.TurnBasedMatch;
import com.google.android.gms.gcm.GoogleCloudMessaging;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.vending.licensing.APKExpansionPolicy;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.List;

public class FMXNativeActivity extends NativeActivity implements FMXMediaLibraryListener {
    private static int BUFFER_SIZE = 0;
    private static final boolean CLASSES_DEX_DEBUGGER_SUPPORT = true;
    private static final String TAG = "FMXNativeActivity";
    private FullScreenFSM mFullScreenState;
    private boolean mIsSurfaceCreated;
    private KeyEvent mLastEvent;
    private OnActivityListener mListener;
    private FMXMediaLibrary mMediaLibrary;
    private Callback2 mNativeCallback;
    private boolean mNeedPerformTakingPhoto;
    private boolean mPaused;
    private int mPhotoRequestCode;
    private int mPhotoResultCode;
    private Dialog mPickerDialog;
    private int mPickerDialogId;
    private List<String> mRegisteredIntentActions;
    private Bundle mStartupGCM;
    private FMXTextEditorProxy mTextView;
    private ViewGroup mViewGroup;
    private ViewStack mViewStack;

    static class FullScreenFSM {
        public static final int[][] FSM;
        private static final int NSTATES = 4;
        public static final int STATE_NAV = 1;
        public static final int STATE_STAT_NAV = 0;
        public static final int STATE_Z1 = 2;
        public static final int STATE_Z2 = 3;
        public static final int TRANS_HIDE_STATUSBAR = 0;
        public static final int TRANS_HIDE_SYSTEM_UI = 2;
        public static final int TRANS_SHOW_STATUSBAR = 1;
        public static final int TRANS_SHOW_SYSTEM_UI = 3;
        private Callback[] mCallbacks;
        private int mState;

        public static abstract class Callback {
            abstract void stateChanged(int i, int i2);
        }

        FullScreenFSM() {
            this.mState = TRANS_HIDE_STATUSBAR;
            this.mCallbacks = new Callback[NSTATES];
        }

        static {
            int[][] iArr = new int[NSTATES][];
            iArr[TRANS_HIDE_STATUSBAR] = new int[]{TRANS_SHOW_STATUSBAR, TRANS_HIDE_STATUSBAR, TRANS_SHOW_SYSTEM_UI, TRANS_HIDE_STATUSBAR};
            iArr[TRANS_SHOW_STATUSBAR] = new int[]{TRANS_SHOW_STATUSBAR, TRANS_HIDE_STATUSBAR, TRANS_HIDE_SYSTEM_UI, TRANS_SHOW_STATUSBAR};
            iArr[TRANS_HIDE_SYSTEM_UI] = new int[]{TRANS_HIDE_SYSTEM_UI, TRANS_SHOW_SYSTEM_UI, TRANS_HIDE_SYSTEM_UI, TRANS_SHOW_STATUSBAR};
            iArr[TRANS_SHOW_SYSTEM_UI] = new int[]{TRANS_HIDE_SYSTEM_UI, TRANS_SHOW_SYSTEM_UI, TRANS_SHOW_SYSTEM_UI, TRANS_HIDE_STATUSBAR};
            FSM = iArr;
        }

        public void setStateCallback(int state, Callback callback) {
            this.mCallbacks[state] = callback;
        }

        public int changeState(int transition) {
            int newState = FSM[this.mState][transition];
            if (!(newState == this.mState || this.mCallbacks[newState] == null)) {
                this.mCallbacks[newState].stateChanged(this.mState, newState);
            }
            this.mState = newState;
            return this.mState;
        }

        public void callback() {
            if (this.mCallbacks[this.mState] != null) {
                this.mCallbacks[this.mState].stateChanged(this.mState, this.mState);
            }
        }

        public int getState() {
            return this.mState;
        }
    }

    public FMXNativeActivity() {
        this.mMediaLibrary = new FMXMediaLibrary(this);
        this.mPaused = false;
        this.mPickerDialogId = 1;
        this.mPhotoRequestCode = 0;
        this.mPhotoResultCode = 0;
        this.mNeedPerformTakingPhoto = false;
        this.mIsSurfaceCreated = false;
        this.mFullScreenState = new FullScreenFSM();
    }

    private boolean hasNeon() {
        boolean result = false;
        File cpuInfo = new File("/proc/cpuinfo");
        if (cpuInfo.exists()) {
            try {
                String info;
                BufferedReader reader = new BufferedReader(new FileReader(cpuInfo));
                do {
                    info = reader.readLine();
                    if (info == null) {
                        break;
                    }
                } while (!info.contains("Features"));
                if (info.contains("neon")) {
                    result = CLASSES_DEX_DEBUGGER_SUPPORT;
                } else {
                    Log.e(TAG, "NEON support not detected in CPU Features");
                }
                reader.close();
            } catch (IOException e) {
            }
        }
        return result;
    }

    @SuppressLint({"NewApi"})
    protected void onCreate(Bundle savedInstanceState) {
        if (!Build.CPU_ABI.equals("armeabi-v7a") || hasNeon()) {
            Bundle aExtra;
            int portno = -1;
            Intent aIntent = getIntent();
            if (aIntent != null) {
                aExtra = aIntent.getExtras();
                if (aExtra != null) {
                    portno = aExtra.getInt("port", -1);
                }
            }
            if (portno == 0) {
                portno = findAvailablePort();
            }
            if (portno > 0) {
                startGdbServer(portno);
            }
            this.mRegisteredIntentActions = new ArrayList();
            this.mRegisteredIntentActions.add(NotificationInfo.ACTION_NOTIFICATION);
            this.mRegisteredIntentActions.add(NotificationPublisher.ACTION_GCM_NOTIFICATION);
            super.onCreate(savedInstanceState);
            aExtra = getIntent().getExtras();
            if (aExtra != null) {
                this.mStartupGCM = aExtra.getBundle(GoogleCloudMessaging.MESSAGE_TYPE_MESSAGE);
            }
            this.mTextView = new FMXTextEditorProxy(this);
            this.mTextView.setFocusable(false);
            this.mViewGroup = (ViewGroup) findViewById(16908290);
            this.mViewGroup.addView(this.mTextView);
            this.mViewGroup.setTag("mViewGroup");
            this.mViewStack = new ViewStack(this);
            this.mMediaLibrary.setListener(this);
            this.mTextView.addOnVKStateChangeListener(new VKStateChangeListener() {
                public void onVirtualKeyboardShown() {
                }

                public void onVirtualKeyboardHidden() {
                    FMXNativeActivity.this.runOnUiThread(new Runnable() {
                        public void run() {
                            FMXNativeActivity.this.mFullScreenState.callback();
                        }
                    });
                }
            });
            return;
        }
        Log.e(TAG, "Unsupported CPU architecture");
        throw new RuntimeException("Application does not support this device");
    }

    public ViewStack getViewStack() {
        return this.mViewStack;
    }

    public void onResume() {
        super.onResume();
        this.mPaused = false;
    }

    public void onPause() {
        super.onPause();
        this.mPaused = CLASSES_DEX_DEBUGGER_SUPPORT;
    }

    public ViewGroup getViewGroup() {
        return this.mViewGroup;
    }

    public int addView(View view, LayoutParams params) {
        this.mViewGroup.addView(view, params);
        return this.mViewGroup.indexOfChild(view);
    }

    public void removeView(View view) {
        this.mViewGroup.removeView(view);
    }

    public void removeViewAt(int index) {
        this.mViewGroup.removeViewAt(index);
    }

    public void setNativeCallback(Callback2 callback) {
        this.mNativeCallback = callback;
    }

    public void embSetOrientation(int orientationMask) {
        switch (orientationMask) {
            case TurnBasedMatch.MATCH_VARIANT_DEFAULT /*-1*/:
                setRequestedOrientation(2);
            case APKExpansionPolicy.MAIN_FILE_URL_INDEX /*0*/:
                setRequestedOrientation(1);
            case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                setRequestedOrientation(0);
            case DetectedActivity.ON_FOOT /*2*/:
                setRequestedOrientation(9);
            case DetectedActivity.STILL /*3*/:
                setRequestedOrientation(8);
            default:
        }
    }

    public void setStatusBarVisibility(boolean visible) {
        this.mFullScreenState.changeState(visible ? 1 : 0);
    }

    public void setSystemUIVisibility(boolean visible) {
        this.mFullScreenState.changeState(visible ? 3 : 2);
    }

    public boolean getSystemUIVisibility() {
        return (this.mFullScreenState.getState() == 1 || this.mFullScreenState.getState() == 0) ? CLASSES_DEX_DEBUGGER_SUPPORT : false;
    }

    public boolean getStatusBarVisibility() {
        return this.mFullScreenState.getState() == 0 ? CLASSES_DEX_DEBUGGER_SUPPORT : false;
    }

    public void onWindowFocusChanged(boolean hasFocus) {
        if (hasFocus) {
            this.mFullScreenState.callback();
        }
        super.onWindowFocusChanged(hasFocus);
    }

    public KeyEvent getLastEvent() {
        return this.mLastEvent;
    }

    public void setListener(OnActivityListener listener) {
        this.mListener = listener;
    }

    public void receiveGCM(Bundle bundle) {
        if (this.mPaused) {
            new NotificationPublisher(this).publishGCM(bundle);
        } else if (hasListener()) {
            Intent intent = new Intent(NotificationPublisher.ACTION_GCM_NOTIFICATION);
            intent.putExtra(GoogleCloudMessaging.MESSAGE_TYPE_MESSAGE, bundle);
            this.mListener.onReceiveNotification(intent);
        }
    }

    public Bundle getStartupGCM() {
        return this.mStartupGCM;
    }

    public boolean dispatchKeyEvent(KeyEvent event) {
        this.mLastEvent = event;
        return super.dispatchKeyEvent(event);
    }

    public FMXMediaLibrary getFMXMediaLibrary() {
        return this.mMediaLibrary;
    }

    public void showDialog(int id, Dialog dialog) {
        this.mPickerDialog = dialog;
        showDialog(id);
    }

    @Deprecated
    protected Dialog onCreateDialog(int id) {
        Dialog tempDialog = this.mPickerDialog;
        this.mPickerDialog = null;
        return tempDialog;
    }

    public int getNextPickerDialogId() {
        int i = this.mPickerDialogId;
        this.mPickerDialogId = i + 1;
        return i;
    }

    @SuppressLint({"NewApi"})
    public Point getRawDisplaySize() {
        Display display = getWindowManager().getDefaultDisplay();
        if (VERSION.SDK_INT >= 17) {
            DisplayMetrics metrics = new DisplayMetrics();
            display.getRealMetrics(metrics);
            return new Point(metrics.widthPixels, metrics.heightPixels);
        }
        try {
            return new Point(((Integer) Display.class.getMethod("getRawWidth", new Class[0]).invoke(display, new Object[0])).intValue(), ((Integer) Display.class.getMethod("getRawHeight", new Class[0]).invoke(display, new Object[0])).intValue());
        } catch (Throwable th) {
            return null;
        }
    }

    protected boolean hasListener() {
        return this.mListener != null ? CLASSES_DEX_DEBUGGER_SUPPORT : false;
    }

    protected boolean canInvokeListenerCallbacks() {
        return this.mIsSurfaceCreated;
    }

    protected void storeActivityResult(int requestCode, int resultCode) {
        this.mPhotoResultCode = resultCode;
        this.mPhotoRequestCode = requestCode;
    }

    protected void onActivityResult(int requestCode, int resultCode, Intent data) {
        super.onActivityResult(requestCode, resultCode, data);
        if (hasListener() && FMXMediaLibrary.isRequestForTakingImage(requestCode)) {
            storeActivityResult(requestCode, resultCode);
            if (resultCode == -1) {
                this.mMediaLibrary.handleTakingPhotoRequest(data, requestCode);
            } else if (canInvokeListenerCallbacks()) {
                this.mListener.onCancelReceiveImage(requestCode);
            } else {
                this.mNeedPerformTakingPhoto = CLASSES_DEX_DEBUGGER_SUPPORT;
            }
        } else if (hasListener()) {
            Log.i(TAG, "Request code not from FMXMediaLibrary, calling generic handler.");
            this.mListener.onReceiveResult(requestCode, resultCode, data);
        }
    }

    public void surfaceCreated(SurfaceHolder holder) {
        super.surfaceCreated(holder);
        if (this.mNativeCallback != null) {
            this.mNativeCallback.surfaceCreated(holder);
        }
        this.mIsSurfaceCreated = CLASSES_DEX_DEBUGGER_SUPPORT;
        if (this.mNeedPerformTakingPhoto && hasListener()) {
            try {
                if (this.mPhotoResultCode == -1) {
                    this.mListener.onReceiveImagePath(this.mPhotoRequestCode, this.mMediaLibrary.getLastPhotoName());
                } else {
                    this.mListener.onCancelReceiveImage(this.mPhotoRequestCode);
                }
                this.mPhotoRequestCode = 0;
                this.mNeedPerformTakingPhoto = false;
            } catch (Throwable th) {
                this.mPhotoRequestCode = 0;
                this.mNeedPerformTakingPhoto = false;
            }
        }
    }

    public void surfaceDestroyed(SurfaceHolder holder) {
        super.surfaceDestroyed(holder);
        if (this.mNativeCallback != null) {
            this.mNativeCallback.surfaceDestroyed(holder);
        }
        this.mIsSurfaceCreated = false;
    }

    public void surfaceChanged(SurfaceHolder holder, int format, int w, int h) {
        super.surfaceChanged(holder, format, w, h);
        if (this.mNativeCallback != null) {
            this.mNativeCallback.surfaceChanged(holder, format, w, h);
        }
    }

    public void registerIntentAction(String action) {
        this.mRegisteredIntentActions.add(action);
    }

    protected void onNewIntent(Intent intent) {
        super.onNewIntent(intent);
        if (hasListener() && this.mRegisteredIntentActions != null && this.mRegisteredIntentActions.contains(intent.getAction())) {
            this.mListener.onReceiveNotification(intent);
        }
    }

    protected void onSaveInstanceState(Bundle outState) {
        super.onSaveInstanceState(outState);
        this.mMediaLibrary.onSaveInstanceState(outState);
    }

    protected void onRestoreInstanceState(Bundle savedInstanceState) {
        super.onRestoreInstanceState(savedInstanceState);
        this.mMediaLibrary.onRestoreInstanceState(savedInstanceState);
    }

    public FMXTextEditorProxy getTextEditorProxy() {
        return this.mTextView;
    }

    public void showStatusBar() {
        getWindow().clearFlags(AccessibilityNodeInfoCompat.ACTION_NEXT_HTML_ELEMENT);
        getWindow().addFlags(AccessibilityNodeInfoCompat.ACTION_PREVIOUS_HTML_ELEMENT);
    }

    public void hideStatusBar() {
        getWindow().clearFlags(AccessibilityNodeInfoCompat.ACTION_PREVIOUS_HTML_ELEMENT);
        getWindow().addFlags(AccessibilityNodeInfoCompat.ACTION_NEXT_HTML_ELEMENT);
    }

    @SuppressLint({"NewApi"})
    public void hideSystemUI() {
        if (VERSION.SDK_INT >= 19) {
            getWindow().getDecorView().setSystemUiVisibility(5894);
        }
    }

    @SuppressLint({"NewApi"})
    public void showSystemUI() {
        if (VERSION.SDK_INT >= 19) {
            getWindow().getDecorView().setSystemUiVisibility(0);
        }
    }

    public void applicationActivated() {
        initFullScreenFSM();
        runOnUiThread(new Runnable() {
            public void run() {
                FMXNativeActivity.this.mFullScreenState.callback();
            }
        });
    }

    public void applicationDeactivated() {
        unInitFullScreenFSM();
    }

    private void unInitFullScreenFSM() {
        this.mFullScreenState.setStateCallback(0, null);
        this.mFullScreenState.setStateCallback(1, null);
        this.mFullScreenState.setStateCallback(2, null);
        this.mFullScreenState.setStateCallback(3, null);
    }

    private void initFullScreenFSM() {
        this.mFullScreenState.setStateCallback(0, new Callback() {
            public void stateChanged(int oldState, int newState) {
                FMXNativeActivity.this.showStatusBar();
                FMXNativeActivity.this.showSystemUI();
            }
        });
        this.mFullScreenState.setStateCallback(1, new Callback() {
            public void stateChanged(int oldState, int newState) {
                FMXNativeActivity.this.hideStatusBar();
                FMXNativeActivity.this.showSystemUI();
            }
        });
        Callback hideEverything = new Callback() {
            public void stateChanged(int oldState, int newState) {
                FMXNativeActivity.this.hideStatusBar();
                FMXNativeActivity.this.hideSystemUI();
            }
        };
        this.mFullScreenState.setStateCallback(2, hideEverything);
        this.mFullScreenState.setStateCallback(3, hideEverything);
    }

    public String getDeviceID() {
        String deviceID = Secure.getString(getContentResolver(), "android_id");
        try {
            MessageDigest digest = MessageDigest.getInstance("MD5");
            digest.update(deviceID.getBytes());
            byte[] messageDigest = digest.digest();
            StringBuffer hexString = new StringBuffer(32);
            for (byte b : messageDigest) {
                String h = Integer.toHexString(b & MotionEventCompat.ACTION_MASK);
                if (h.length() == 1) {
                    hexString.append("0");
                }
                hexString.append(h);
            }
            return hexString.toString().toUpperCase();
        } catch (NoSuchAlgorithmException e) {
            return "";
        }
    }

    private int findAvailablePort() {
        return -1;
    }

    private int getPid(String processName) {
        int pid = -1;
        BufferedReader bufferedReader;
        try {
            String line;
            bufferedReader = new BufferedReader(new InputStreamReader(new ProcessBuilder(new String[0]).command(new String[]{"ps"}).start().getInputStream()));
            processName = processName.toLowerCase();
            do {
                line = bufferedReader.readLine();
                if (line != null) {
                }
                break;
            } while (!line.toLowerCase().contains(processName));
            pid = Integer.parseInt(line.split("[ ]+")[1]);
            break;
            bufferedReader.close();
        } catch (Exception e) {
            Log.w(TAG, "Exception failed to start ps command: " + e.getMessage());
        } catch (Throwable th) {
            bufferedReader.close();
        }
        return pid;
    }

    private int invokeGdbServer(String gdbServerName, int portno) {
        int pid = getPid(gdbServerName);
        if (pid != -1) {
            Process.killProcess(pid);
        }
        try {
            if (!new File(gdbServerName).canExecute()) {
                return -1;
            }
            Process process = new ProcessBuilder(new String[0]).command(new String[]{gdbServerName, "tcp:" + String.valueOf(portno), "--attach", "" + Process.myPid()}).redirectErrorStream(CLASSES_DEX_DEBUGGER_SUPPORT).start();
            return getPid(gdbServerName);
        } catch (Exception e) {
            Log.w(TAG, "Exception failed to start " + gdbServerName);
            return -1;
        }
    }

    static {
        BUFFER_SIZE = AccessibilityNodeInfoCompat.ACTION_NEXT_HTML_ELEMENT;
    }

    private void startGdbServer(int portno) {
        String libGdbServerPath = getFilesDir().getParent() + "/lib/gdbserver";
        if (invokeGdbServer(libGdbServerPath, portno) == -1) {
            File cacheGdbserver = new File(getFilesDir().getParent() + "/cache/gdbserver");
            if (!cacheGdbserver.exists()) {
                try {
                    File cacheDir = cacheGdbserver.getParentFile();
                    if (!cacheDir.exists()) {
                        cacheDir.mkdir();
                    }
                    InputStream src = new FileInputStream(new File(libGdbServerPath));
                    OutputStream dst = new FileOutputStream(cacheGdbserver);
                    byte[] buf = new byte[BUFFER_SIZE];
                    while (true) {
                        int len = src.read(buf);
                        if (len <= 0) {
                            break;
                        }
                        dst.write(buf, 0, len);
                    }
                    src.close();
                    dst.close();
                    cacheGdbserver.setExecutable(CLASSES_DEX_DEBUGGER_SUPPORT, false);
                    cacheGdbserver.setReadable(CLASSES_DEX_DEBUGGER_SUPPORT, false);
                } catch (Exception e) {
                    Log.w(TAG, "Exception failed to copy gdbserver");
                }
            }
            invokeGdbServer(cacheGdbserver.getPath(), portno);
        }
    }

    public void OnMediaLibraryCancel() {
        this.mNeedPerformTakingPhoto = CLASSES_DEX_DEBUGGER_SUPPORT;
        storeActivityResult(this.mPhotoRequestCode, 0);
        if (canInvokeListenerCallbacks()) {
            this.mNeedPerformTakingPhoto = false;
            this.mListener.onCancelReceiveImage(this.mPhotoRequestCode);
        }
    }

    public void OnMediaLibraryAccept() {
        this.mNeedPerformTakingPhoto = CLASSES_DEX_DEBUGGER_SUPPORT;
        storeActivityResult(this.mPhotoRequestCode, -1);
        if (canInvokeListenerCallbacks()) {
            this.mNeedPerformTakingPhoto = false;
            this.mListener.onReceiveImagePath(this.mPhotoRequestCode, this.mMediaLibrary.getLastPhotoName());
        }
    }
}
