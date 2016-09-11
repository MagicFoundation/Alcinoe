package com.google.android.gms.maps.model;

import android.support.v4.view.accessibility.AccessibilityNodeInfoCompat;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.URL;

public abstract class UrlTileProvider implements TileProvider {
    private final int v;
    private final int w;

    public UrlTileProvider(int width, int height) {
        this.w = width;
        this.v = height;
    }

    private static long a(InputStream inputStream, OutputStream outputStream) throws IOException {
        byte[] bArr = new byte[AccessibilityNodeInfoCompat.ACTION_SCROLL_FORWARD];
        long j = 0;
        while (true) {
            int read = inputStream.read(bArr);
            if (read == -1) {
                return j;
            }
            outputStream.write(bArr, 0, read);
            j += (long) read;
        }
    }

    private static byte[] a(InputStream inputStream) throws IOException {
        OutputStream byteArrayOutputStream = new ByteArrayOutputStream();
        a(inputStream, byteArrayOutputStream);
        return byteArrayOutputStream.toByteArray();
    }

    public final Tile getTile(int x, int y, int zoom) {
        URL tileUrl = getTileUrl(x, y, zoom);
        if (tileUrl == null) {
            return NO_TILE;
        }
        try {
            return new Tile(this.w, this.v, a(tileUrl.openStream()));
        } catch (IOException e) {
            return null;
        }
    }

    public abstract URL getTileUrl(int i, int i2, int i3);
}
