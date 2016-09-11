package com.embarcadero.firemonkey.broadcast;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;

public class FMXBroadcastReceiver extends BroadcastReceiver {
    FMXBroadcastReceiverListener mListener;

    public FMXBroadcastReceiver(FMXBroadcastReceiverListener listener) {
        this.mListener = listener;
    }

    public void onReceive(Context context, Intent intent) {
        this.mListener.onReceive(context, intent);
    }
}
