package com.embarcadero.firemonkey.broadcast;

import android.content.Context;
import android.content.Intent;

public interface FMXBroadcastReceiverListener {
    void onReceive(Context context, Intent intent);
}
