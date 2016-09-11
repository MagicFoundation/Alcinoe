package com.embarcadero.firemonkey.medialibrary;

import android.os.Environment;
import android.util.Log;
import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

public class Utils {
    private static final int BUFFER_SIZE = 8192;
    private static final String TAG = "FMX_UTILS";

    public static int copyStream(InputStream input, OutputStream output) throws Exception, IOException {
        byte[] buffer = new byte[BUFFER_SIZE];
        BufferedInputStream in = new BufferedInputStream(input, BUFFER_SIZE);
        BufferedOutputStream out = new BufferedOutputStream(output, BUFFER_SIZE);
        int count = 0;
        while (true) {
            try {
                int n = in.read(buffer, 0, BUFFER_SIZE);
                if (n == -1) {
                    break;
                }
                out.write(buffer, 0, n);
                count += n;
            } finally {
                try {
                    out.close();
                } catch (IOException e) {
                    Log.e(TAG, e.getMessage(), e);
                }
                try {
                    in.close();
                } catch (IOException e2) {
                    Log.e(TAG, e2.getMessage(), e2);
                }
            }
        }
        out.flush();
        try {
            in.close();
        } catch (IOException e22) {
            Log.e(TAG, e22.getMessage(), e22);
        }
        return count;
    }

    public static File getPhotosDir() {
        return getAlbumDir("Camera");
    }

    public static File getAlbumDir(String albumName) {
        File storageDir = null;
        if ("mounted".equals(Environment.getExternalStorageState())) {
            storageDir = new File(Environment.getExternalStoragePublicDirectory(Environment.DIRECTORY_DCIM), albumName);
            if (!(storageDir == null || storageDir.mkdirs() || storageDir.exists())) {
                return null;
            }
        }
        Log.v(TAG, "External storage is not mounted READ/WRITE.");
        return storageDir;
    }
}
