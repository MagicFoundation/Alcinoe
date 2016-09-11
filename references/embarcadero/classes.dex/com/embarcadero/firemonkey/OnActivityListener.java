package com.embarcadero.firemonkey;

import android.content.Intent;

public interface OnActivityListener {
    void onCancelReceiveImage(int i);

    void onReceiveImagePath(int i, String str);

    void onReceiveNotification(Intent intent);

    void onReceiveResult(int i, int i2, Intent intent);
}
