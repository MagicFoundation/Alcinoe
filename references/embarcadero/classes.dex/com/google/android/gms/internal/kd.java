package com.google.android.gms.internal;

import java.io.IOException;

public class kd extends IOException {
    public kd(String str) {
        super(str);
    }

    static kd kO() {
        return new kd("While parsing a protocol message, the input ended unexpectedly in the middle of a field.  This could mean either than the input has been truncated or that an embedded message misreported its own length.");
    }

    static kd kP() {
        return new kd("CodedInputStream encountered an embedded string or message which claimed to have negative size.");
    }

    static kd kQ() {
        return new kd("CodedInputStream encountered a malformed varint.");
    }

    static kd kR() {
        return new kd("Protocol message contained an invalid tag (zero).");
    }

    static kd kS() {
        return new kd("Protocol message end-group tag did not match expected tag.");
    }

    static kd kT() {
        return new kd("Protocol message tag had invalid wire type.");
    }

    static kd kU() {
        return new kd("Protocol message had too many levels of nesting.  May be malicious.  Use CodedInputStream.setRecursionLimit() to increase the depth limit.");
    }
}
