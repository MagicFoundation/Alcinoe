package android.support.v4.util;

import android.support.v4.media.TransportMediator;
import android.util.Log;
import java.io.Writer;

public class LogWriter extends Writer {
    private StringBuilder mBuilder;
    private final String mTag;

    public LogWriter(String tag) {
        this.mBuilder = new StringBuilder(TransportMediator.FLAG_KEY_MEDIA_NEXT);
        this.mTag = tag;
    }

    public void close() {
        flushBuilder();
    }

    public void flush() {
        flushBuilder();
    }

    public void write(char[] buf, int offset, int count) {
        for (int i = 0; i < count; i++) {
            char c = buf[offset + i];
            if (c == '\n') {
                flushBuilder();
            } else {
                this.mBuilder.append(c);
            }
        }
    }

    private void flushBuilder() {
        if (this.mBuilder.length() > 0) {
            Log.d(this.mTag, this.mBuilder.toString());
            this.mBuilder.delete(0, this.mBuilder.length());
        }
    }
}
