package com.google.android.gms.internal;

import android.os.ParcelFileDescriptor;
import android.os.ParcelFileDescriptor.AutoCloseInputStream;
import android.os.ParcelFileDescriptor.AutoCloseOutputStream;
import com.google.android.gms.games.multiplayer.realtime.RealTimeSocket;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

public final class gc implements RealTimeSocket {
    private static final String TAG;
    private final ParcelFileDescriptor AC;
    private final OutputStream HA;
    private final InputStream Hz;

    static {
        TAG = gc.class.getSimpleName();
    }

    gc(ParcelFileDescriptor parcelFileDescriptor) {
        this.AC = parcelFileDescriptor;
        this.Hz = new AutoCloseInputStream(parcelFileDescriptor);
        this.HA = new AutoCloseOutputStream(parcelFileDescriptor);
    }

    public void close() throws IOException {
        this.AC.close();
    }

    public InputStream getInputStream() throws IOException {
        return this.Hz;
    }

    public OutputStream getOutputStream() throws IOException {
        return this.HA;
    }

    public ParcelFileDescriptor getParcelFileDescriptor() throws IOException {
        return this.AC;
    }

    public boolean isClosed() {
        try {
            this.Hz.available();
            return false;
        } catch (IOException e) {
            return true;
        }
    }
}
