package android.bluetooth.le;

import android.annotation.SuppressLint;

@SuppressLint({"NewApi"})
public class RTLAdvertiseCallback extends AdvertiseCallback {
    private RTLAdvertiseListener mListener;

    public RTLAdvertiseCallback(RTLAdvertiseListener listener) {
        this.mListener = listener;
    }

    public void onStartFailure(int errorCode) {
        this.mListener.onStartFailure(errorCode);
    }

    public void onStartSuccess(AdvertiseSettings settingsInEffect) {
        this.mListener.onStartSuccess(settingsInEffect);
    }
}
