package com.embarcadero.gcm.notifications;

import android.os.Bundle;

public interface GCMNativeListener {
    void OnNotificationReceived(Bundle bundle);
}
