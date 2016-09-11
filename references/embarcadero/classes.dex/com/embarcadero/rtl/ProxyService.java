package com.embarcadero.rtl;

import android.content.Intent;
import android.content.res.Configuration;
import android.os.IBinder;
import android.os.Message;

public class ProxyService {
    private static native long getDelphiService(Object obj, String str);

    private static native IBinder onBindNative(Object obj, String str, Intent intent);

    private static native void onConfigurationChangedNative(Object obj, String str, Configuration configuration);

    private static native void onCreateNative(Object obj, String str);

    private static native void onDestroyNative(Object obj, String str);

    private static native void onHandleIntentNative(Object obj, String str, Intent intent);

    private static native boolean onHandleMessageNative(Object obj, String str, Message message);

    private static native void onLowMemoryNative(Object obj, String str);

    private static native void onRebindNative(Object obj, String str, Intent intent);

    private static native int onStartCommandNative(Object obj, String str, Intent intent, int i, int i2);

    private static native void onTaskRemovedNative(Object obj, String str, Intent intent);

    private static native void onTrimMemoryNative(Object obj, String str, int i);

    private static native boolean onUnbindNative(Object obj, String str, Intent intent);

    static {
        System.loadLibrary("ProxyAndroidService");
    }

    public static void onCreate(Object service, String libraryName) {
        onCreateNative(service, libraryName);
    }

    public static void onDestroy(Object service, String libraryName) {
        onDestroyNative(service, libraryName);
    }

    public static int onStartCommand(Object service, String libraryName, Intent intent, int flags, int startId) {
        return onStartCommandNative(service, libraryName, intent, flags, startId);
    }

    public static IBinder onBind(Object service, String libraryName, Intent intent) {
        return onBindNative(service, libraryName, intent);
    }

    public static void onRebind(Object service, String libraryName, Intent intent) {
        onRebindNative(service, libraryName, intent);
    }

    public static boolean onUnbind(Object service, String libraryName, Intent intent) {
        return onUnbindNative(service, libraryName, intent);
    }

    public static void onConfigurationChanged(Object service, String libraryName, Configuration newConfig) {
        onConfigurationChangedNative(service, libraryName, newConfig);
    }

    public static void onLowMemory(Object service, String libraryName) {
        onLowMemoryNative(service, libraryName);
    }

    public static void onTaskRemoved(Object service, String libraryName, Intent rootIntent) {
        onTaskRemovedNative(service, libraryName, rootIntent);
    }

    public static void onTrimMemory(Object service, String libraryName, int level) {
        onTrimMemoryNative(service, libraryName, level);
    }

    public static long getService(Object service, String libraryName) {
        return getDelphiService(service, libraryName);
    }

    public static void onHandleIntent(Object service, String libraryName, Intent intent) {
        onHandleIntentNative(service, libraryName, intent);
    }

    public static boolean onHandleMessage(Object service, String libraryName, Message msg) {
        return onHandleMessageNative(service, libraryName, msg);
    }
}
