package com.google.android.gms.analytics;

public interface ExceptionParser {
    String getDescription(String str, Throwable th);
}
