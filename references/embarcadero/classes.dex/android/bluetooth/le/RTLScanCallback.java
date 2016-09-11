package android.bluetooth.le;

import android.annotation.SuppressLint;
import java.util.List;

@SuppressLint({"NewApi"})
public class RTLScanCallback extends ScanCallback {
    private RTLScanListener mListener;

    public RTLScanCallback(RTLScanListener listener) {
        this.mListener = listener;
    }

    public void onBatchScanResults(List results) {
        this.mListener.onBatchScanResults(results);
    }

    public void onScanFailed(int errorCode) {
        this.mListener.onScanFailed(errorCode);
    }

    public void onScanResult(int callbackType, ScanResult result) {
        this.mListener.onScanResult(callbackType, result);
    }
}
