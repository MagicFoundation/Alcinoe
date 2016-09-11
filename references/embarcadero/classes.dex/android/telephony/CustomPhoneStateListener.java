package android.telephony;

import java.util.List;

public class CustomPhoneStateListener extends PhoneStateListener {
    ICustomPhoneStateListener mListener;

    public CustomPhoneStateListener(ICustomPhoneStateListener listener) {
        this.mListener = listener;
    }

    public void onServiceStateChanged(ServiceState serviceState) {
        this.mListener.onServiceStateChanged(serviceState);
    }

    public void onSignalStrengthChanged(int asu) {
        this.mListener.onSignalStrengthChanged(asu);
    }

    public void onMessageWaitingIndicatorChanged(boolean mwi) {
        this.mListener.onMessageWaitingIndicatorChanged(mwi);
    }

    public void onCallForwardingIndicatorChanged(boolean cfi) {
        this.mListener.onCallForwardingIndicatorChanged(cfi);
    }

    public void onCellLocationChanged(CellLocation location) {
        this.mListener.onCellLocationChanged(location);
    }

    public void onCallStateChanged(int state, String incomingNumber) {
        this.mListener.onCallStateChanged(state, incomingNumber);
    }

    public void onDataConnectionStateChanged(int state) {
        this.mListener.onDataConnectionStateChanged(state);
    }

    public void onDataConnectionStateChanged(int state, int networkType) {
        this.mListener.onDataConnectionStateChanged(state, networkType);
    }

    public void onDataActivity(int direction) {
        this.mListener.onDataActivity(direction);
    }

    public void onSignalStrengthsChanged(SignalStrength signalStrength) {
        this.mListener.onSignalStrengthsChanged(signalStrength);
    }

    public void onCellInfoChanged(List<CellInfo> cellInfo) {
        this.mListener.onCellInfoChanged(cellInfo);
    }
}
