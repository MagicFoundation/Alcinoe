package android.bluetooth.le;

public interface RTLAdvertiseListener {
    void onStartFailure(int i);

    void onStartSuccess(AdvertiseSettings advertiseSettings);
}
