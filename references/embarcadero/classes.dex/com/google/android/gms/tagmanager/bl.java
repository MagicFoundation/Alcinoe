package com.google.android.gms.tagmanager;

import java.io.IOException;
import java.io.InputStream;

interface bl {
    InputStream bo(String str) throws IOException;

    void close();
}
