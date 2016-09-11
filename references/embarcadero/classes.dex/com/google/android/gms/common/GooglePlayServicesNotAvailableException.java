package com.google.android.gms.common;

public final class GooglePlayServicesNotAvailableException extends Exception {
    public final int errorCode;

    public GooglePlayServicesNotAvailableException(int errorCode) {
        this.errorCode = errorCode;
    }
}
