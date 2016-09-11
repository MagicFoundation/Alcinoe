package com.google.android.gms.drive.internal;

import android.os.Handler;
import android.os.Looper;
import android.os.Message;
import android.os.RemoteException;
import android.util.Log;
import android.util.Pair;
import com.google.android.gms.drive.events.DriveEvent;
import com.google.android.gms.drive.events.DriveEvent.Listener;
import com.google.android.gms.internal.er;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.vending.licensing.APKExpansionPolicy;

public class s<C extends DriveEvent> extends com.google.android.gms.drive.internal.w.a {
    private final Listener<C> DR;
    private final a<C> DS;
    private final int Dm;

    private static class a<E extends DriveEvent> extends Handler {
        private a(Looper looper) {
            super(looper);
        }

        public void a(Listener<E> listener, E e) {
            sendMessage(obtainMessage(1, new Pair(listener, e)));
        }

        public void handleMessage(Message msg) {
            switch (msg.what) {
                case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                    Pair pair = (Pair) msg.obj;
                    ((Listener) pair.first).onEvent((DriveEvent) pair.second);
                default:
                    Log.wtf("EventCallback", "Don't know how to handle this event");
            }
        }
    }

    public s(Looper looper, int i, Listener<C> listener) {
        this.Dm = i;
        this.DR = listener;
        this.DS = new a(null);
    }

    public void a(OnEventResponse onEventResponse) throws RemoteException {
        er.v(this.Dm == onEventResponse.getEventType());
        switch (onEventResponse.getEventType()) {
            case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                this.DS.a(this.DR, onEventResponse.fa());
            case DetectedActivity.ON_FOOT /*2*/:
                this.DS.a(this.DR, onEventResponse.fb());
            default:
                Log.w("EventCallback", "Unexpected event type:" + onEventResponse.getEventType());
        }
    }
}
