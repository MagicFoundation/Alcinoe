package android.bluetooth.le;

import java.util.List;

public interface RTLScanListener {
    void onBatchScanResults(List list);

    void onScanFailed(int i);

    void onScanResult(int i, ScanResult scanResult);
}
