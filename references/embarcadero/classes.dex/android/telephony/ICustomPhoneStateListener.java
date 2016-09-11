package android.telephony;

import java.util.List;

public interface ICustomPhoneStateListener {
    void onCallForwardingIndicatorChanged(boolean z);

    void onCallStateChanged(int i, String str);

    void onCellInfoChanged(List<CellInfo> list);

    void onCellLocationChanged(CellLocation cellLocation);

    void onDataActivity(int i);

    void onDataConnectionStateChanged(int i);

    void onDataConnectionStateChanged(int i, int i2);

    void onMessageWaitingIndicatorChanged(boolean z);

    void onServiceStateChanged(ServiceState serviceState);

    void onSignalStrengthChanged(int i);

    void onSignalStrengthsChanged(SignalStrength signalStrength);
}
